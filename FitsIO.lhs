> {-# LANGUAGE ForeignFunctionInterface #-}
> {-# LANGUAGE FlexibleInstances #-}

> -- | A Haskell wrapper for CFITSIO.  See http://heasarc.nasa.gov/docs/software/fitsio/fitsio.html
> module Data.Fits.FitsIO where

> import Foreign.C
> import Foreign.ForeignPtr
> import Foreign.Ptr
> import Foreign.Marshal
> import Foreign.Storable
> import Control.Monad
> import Control.Monad.Trans
> import Data.Char
> import Data.Word

> peekIntConv   :: (Storable a, Integral a, Integral b) 
>               => Ptr a -> IO b
> peekIntConv    = liftM fromIntegral . peek

> peekFloatConv :: (Storable a, RealFloat a, RealFloat b) 
>                => Ptr a -> IO b
> peekFloatConv  = liftM realToFrac . peek

> peekEnum :: (Enum a, Integral b, Storable b) => Ptr b -> IO a
> peekEnum  = liftM cToEnum . peek

> cToEnum :: (Integral i, Enum e) => i -> e
> cToEnum  = toEnum . fromIntegral

> cFromEnum :: (Enum e, Integral i) => e -> i
> cFromEnum  = fromIntegral . fromEnum

> class Read a => FitsValue a where
>     readKey     :: FitsFile -> String -> FitsIO (a, String)
>     readKey f n = do
>         (v, c) <- readKeyString f n
>         return (read v, c)
>
>     readCol :: FitsFile -> Int -> Int -> Int -> Int -> a -> FitsIO ([a], Int)
>     readDef :: a

> instance FitsValue Double where
>     readKey = readKeyCDouble
>     readCol = readColCDouble
>     readDef = 0.0

> instance FitsValue Float where
>     readKey = readKeyCFloat
>     readCol = readColCFloat
>     readDef = 0.0

> instance FitsValue Int where
>     readKey = readKeyCInt
>     readCol = readColCInt
>     readDef = 0

> instance FitsValue String where
>     readKey f n = do
>         (v, c) <- readKeyString f n
>         return (trim v, c)
>       where
>         ltrim = dropWhile isSpace
>         rtrim = reverse . ltrim . reverse
>         trim  = rtrim . ltrim
>   
>     readCol = readColString
>     readDef = ""

> data HduType = ImageHdu
>              | AsciiTbl
>              | BinaryTbl
>              | AnyHdu
>              deriving (Eq,Show)

> instance Enum HduType where
>   fromEnum ImageHdu = 0
>   fromEnum AsciiTbl = 1
>   fromEnum BinaryTbl = 2
>   fromEnum AnyHdu = (-1)
>
>   toEnum 0 = ImageHdu
>   toEnum 1 = AsciiTbl
>   toEnum 2 = BinaryTbl
>   toEnum (-1) = AnyHdu
>   toEnum unmatched = error ("HduType.toEnum: Cannot match " ++ show unmatched)



> -- For the FFI, we have to use the 8-char Fortran style names.  Alas!
> 
> 
> data FLen = FLenFileName
>           | FLenKeyword
>           | FLenCard
>           | FLenValue
>           | FLenComment
>           | FLenErrMsg
>           | FLenStatus
>           
> instance Enum FLen where
>   fromEnum FLenFileName = 1025
>   fromEnum FLenKeyword = 72
>   fromEnum FLenCard = 81
>   fromEnum FLenValue = 71
>   fromEnum FLenComment = 73
>   fromEnum FLenErrMsg = 81
>   fromEnum FLenStatus = 31
> 
>   toEnum 1025 = FLenFileName
>   toEnum 72 = FLenKeyword
>   toEnum 81 = FLenCard
>   toEnum 71 = FLenValue
>   toEnum 73 = FLenComment
>   toEnum 81 = FLenErrMsg
>   toEnum 31 = FLenStatus
>   toEnum unmatched = error ("FLen.toEnum: Cannot match " ++ show unmatched)
> 
> allocaFileName = allocaBytes (fromEnum FLenFileName)
> allocaKeyword  = allocaBytes (fromEnum FLenKeyword)
> allocaCard     = allocaBytes (fromEnum FLenCard)
> allocaValue    = allocaBytes (fromEnum FLenValue)
> allocaComment  = allocaBytes (fromEnum FLenComment)
> allocaErrMsg   = allocaBytes (fromEnum FLenErrMsg)
> allocaStatus   = allocaBytes (fromEnum FLenStatus)
> 
> data ColType = TBit
>              | TByte
>              | TSByte
>              | TLogical
>              | TString
>              | TUShort
>              | TShort
>              | TUInt
>              | TInt
>              | TULong
>              | TLong
>              | TFloat
>              | TLongLong
>              | TDouble
>              | TComplex
>              | TDblComplex
>              deriving (Show)
> instance Enum ColType where
>   fromEnum TBit = 1
>   fromEnum TByte = 11
>   fromEnum TSByte = 12
>   fromEnum TLogical = 14
>   fromEnum TString = 16
>   fromEnum TUShort = 20
>   fromEnum TShort = 21
>   fromEnum TUInt = 30
>   fromEnum TInt = 31
>   fromEnum TULong = 40
>   fromEnum TLong = 41
>   fromEnum TFloat = 42
>   fromEnum TLongLong = 81
>   fromEnum TDouble = 82
>   fromEnum TComplex = 83
>   fromEnum TDblComplex = 163
> 
>   toEnum 1 = TBit
>   toEnum 11 = TByte
>   toEnum 12 = TSByte
>   toEnum 14 = TLogical
>   toEnum 16 = TString
>   toEnum 20 = TUShort
>   toEnum 21 = TShort
>   toEnum 30 = TUInt
>   toEnum 31 = TInt
>   toEnum 40 = TULong
>   toEnum 41 = TLong
>   toEnum 42 = TFloat
>   toEnum 81 = TLongLong
>   toEnum 82 = TDouble
>   toEnum 83 = TComplex
>   toEnum 163 = TDblComplex
>   toEnum unmatched = error ("ColType.toEnum: Cannot match " ++ show unmatched)

> -- | The API is designed to thread a "status" (really, error) variable
> -- through most routines.  These routines take an in/out status
> -- parameter in last place and return status redundantly as the value
> -- of the function.  C uses an int, newtype'd here for safety.

> newtype Status = Status { status :: Int } deriving (Eq, Show)

> -- | Default status value at the start of a program.
> noError :: Status
> noError = Status 0

> -- | A monad to handle threading of status information.
> newtype FitsIO a = FitsIO { runFitsIO :: Status -> IO (Status, a) }

> runFits   :: FitsIO a -> IO a
> runFits f = do
>   (_, x) <- runFitsIO f noError
>   return x

> instance Functor FitsIO where
>   fmap f m = FitsIO $ \s -> do
>     (t, x) <- runFitsIO m s
>     return (t, f x)

> instance Applicative FitsIO where
>   pure a    = FitsIO $ \s -> return (s, a)
>   (<*>) f g = FitsIO $ \s -> do
>     (s',x1) <- runFitsIO f s 
>     (s'',x2)  <- runFitsIO g s' 
>     return (s'',x1 x2)
     
> instance Monad FitsIO where
>   return a = FitsIO $ \s -> return (s, a)
>   m >>= k  = FitsIO $ \s -> do
>     (t, x) <- runFitsIO m s
>     runFitsIO (k x) t
>   fail str = FitsIO $ \_ -> fail str

> instance MonadIO FitsIO where
>   liftIO f = FitsIO $ \s -> do
>     x <- f
>     return (s, x)

> clearStatus :: FitsIO ()
> clearStatus = setStatus noError

> setStatus   :: Status -> FitsIO ()
> setStatus s = FitsIO $ \_ -> return (s, ())

> getStatus :: FitsIO Status
> getStatus = FitsIO $ \s -> return (s, s)

> fp   :: (Status -> IO (Status, a)) -> FitsIO a
> fp f = FitsIO $ \s -> f s
> 
> f1 :: (b -> Status -> IO (Status, a)) -> b -> FitsIO a
> f1 f b = fp $ f b
> 
> f2 :: (b -> c -> Status -> IO (Status, a)) -> b -> c -> FitsIO a
> f2 f b c = fp $ f b c
> 
> f3 :: (b -> c -> d -> Status -> IO (Status, a)) -> b -> c -> d -> FitsIO a
> f3 f b c d = fp $ f b c d
> 
> f4 :: (b -> c -> d -> e -> Status -> IO (Status, a)) -> b -> c -> d -> e -> FitsIO a
> f4 f b c d e = fp $ f b c d e
> 
> r2             :: (a, b, c) -> IO (a, (b, c))
> r2 (a,  b,  c) = return (a, (b, c))
> 
> r3              :: (a, b, c, d) -> IO (a, (b, c, d))
> r3 (a, b, c, d) = return (a, (b, c, d))
> 
> r4                 :: (a, b, c, d, e) -> IO (a, (b, c, d, e))
> r4 (a, b, c, d, e) = return (a, (b, c, d, e))
> 
> -- | Convert a C int to a status value.
> cToStatus :: Integral a => a -> Status
> cToStatus = Status . fromIntegral
> 
> -- | Convert a status value to a C int.
> cFromStatus :: Integral a => Status -> a
> cFromStatus = fromIntegral . status
> 
> -- | Pass a status value as an in/out parameter.
> withStatusConv :: (Integral a, Storable a) => Status -> (Ptr a -> IO b) -> IO b
> withStatusConv = with . cFromStatus
> 
> -- | Return a descriptive text string (30 char max.) corresponding to
> -- a CFITSIO error status code.
> getErrStatus   :: Status -> IO String
> getErrStatus s =
>   allocaStatus $ \str -> do
>     ffgerr (cFromStatus s) str
>     peekCString str
> 
> -- | Return the top (oldest) 80-character error message from the
> -- internal CFITSIO stack of error messages and shift any remaining
> -- messages on the stack up one level. Call this routine repeatedly to
> -- get each message in sequence. The function returns a value = 0 and
> -- a null error message when the error stack is empty.
> readErrMsg :: IO String
> readErrMsg =
>   allocaErrMsg $ \str -> do
>     ffgmsg str
>     peekCString str
> 
> writeErrMark :: IO ()
> writeErrMark =
>   writeErrMark'_ >>= \res ->
>   return ()
> clearErrMark :: IO ()
> clearErrMark =
>   clearErrMark'_ >>= \res ->
>   return ()
> clearErrMsg :: IO ()
> clearErrMsg =
>   clearErrMsg'_ >>= \res ->
>   return ()
> 
> newtype FitsFile = FitsFile (ForeignPtr (FitsFile))
> withFitsFile (FitsFile fptr) = withForeignPtr fptr
> 
> foreign import ccall "CloseFile.h &CloseFile"
>   closeFile_'_ :: FunPtr (Ptr FitsFile -> IO ())

 closeFile_ :: FitsFile -> Status -> IO (Status)
 closeFile_ a1 a2 =
   withFitsFile a1 $ \a1' ->
   withStatusConv a2 $ \a2' ->
   closeFile_'_ a1' a2' >>= \res ->
   let {res' = cToStatus res} in
   return res'
 
 closeFile :: FitsFile -> IO ()
 closeFile a1 = do
   closeFile_ a1 noError
   return ()

> newFitsFile   :: Ptr (Ptr FitsFile) -> IO FitsFile
> newFitsFile p = peek p >>= fmap FitsFile . newForeignPtr closeFile_'_
> 
> deleteFile_ :: FitsFile -> Status -> IO (Status)
> deleteFile_ a1 a2 =
>   withFitsFile a1 $ \a1' -> 
>   withStatusConv a2 $ \a2' -> 
>   deleteFile_'_ a1' a2' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> -- | Return the name of the opened FITS file.
> fileName = f1 fileName_
> 
> fileName_     :: FitsFile -> Status -> IO (Status, String)
> fileName_ f s =
>   allocaFileName $ \str -> do
>     s''  <- withFitsFile f (\f' -> withStatusConv s (\s' -> ffflnm f' str s'))
>     str' <- peekCString str
>     return (cToStatus s'', str')
> 
> data IOMode = ReadOnly
>             | ReadWrite
>             deriving (Show)
> instance Enum IOMode where
>   fromEnum ReadOnly = 0
>   fromEnum ReadWrite = 1
> 
>   toEnum 0 = ReadOnly
>   toEnum 1 = ReadWrite
>   toEnum unmatched = error ("IOMode.toEnum: Cannot match " ++ show unmatched)
> 
> -- | Return the I/O mode of the opened FITS file.
> fileMode = f1 fileMode_
> fileMode_ :: FitsFile -> Status -> IO (Status, IOMode)
> fileMode_ a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   fileMode_'_ a1' a2' a3' >>= \res ->
>   peekEnum a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')
> 
> [openFile, openDiskFile, openData, openTable, openImage] =
>   map f2 [openFile_, openDiskFile_, openData_, openTable_, openImage_]
> 
> -- | Open an existing data file.
> openFile_ :: String -> IOMode -> Status -> IO (Status, FitsFile)
> openFile_ a2 a3 a4 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = cFromEnum a3} in 
>   withStatusConv a4 $ \a4' -> 
>   openFile_'_ a1' a2' a3' a4' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> openDiskFile_ :: String -> IOMode -> Status -> IO (Status, FitsFile)
> openDiskFile_ a2 a3 a4 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = cFromEnum a3} in 
>   withStatusConv a4 $ \a4' -> 
>   openDiskFile_'_ a1' a2' a3' a4' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> openData_ :: String -> IOMode -> Status -> IO (Status, FitsFile)
> openData_ a2 a3 a4 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = cFromEnum a3} in 
>   withStatusConv a4 $ \a4' -> 
>   openData_'_ a1' a2' a3' a4' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> openTable_ :: String -> IOMode -> Status -> IO (Status, FitsFile)
> openTable_ a2 a3 a4 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = cFromEnum a3} in 
>   withStatusConv a4 $ \a4' -> 
>   openTable_'_ a1' a2' a3' a4' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> openImage_ :: String -> IOMode -> Status -> IO (Status, FitsFile)
> openImage_ a2 a3 a4 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = cFromEnum a3} in 
>   withStatusConv a4 $ \a4' -> 
>   openImage_'_ a1' a2' a3' a4' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> [createFile, createDiskFile] = map f1 [createFile_, createDiskFile_]
> 
> -- | Create and open a new empty output FITS file.
> createFile_ :: String -> Status -> IO (Status, FitsFile)
> createFile_ a2 a3 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   createFile_'_ a1' a2' a3' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> createDiskFile_ :: String -> Status -> IO (Status, FitsFile)
> createDiskFile_ a2 a3 =
>   alloca $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   createDiskFile_'_ a1' a2' a3' >>= \res ->
>   newFitsFile a1'>>= \a1'' -> 
>   let {res' = cToStatus res} in
>   return (res', a1'')
> 
> -- | Return the total number of Hdus in the FITS file. This returns
> -- the number of completely defined Hdus in the file. If a new Hdu has
> -- just been added to the FITS file, then that last Hdu will only be
> -- counted if it has been closed, or if data has been written to the
> -- Hdu. The current Hdu remains unchanged by this routine.
> 
> getNumHdus :: FitsFile -> FitsIO Int
> getNumHdus = f1 getNumHdus'
> 
> getNumHdus' :: FitsFile -> Status -> IO (Status, Int)
> getNumHdus' a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   getNumHdus''_ a1' a2' a3' >>= \res ->
>   peekIntConv a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')
> 
> -- | Return the number of the current Hdu (CHdu) in the FITS file
> -- (where the primary array = 1). This function returns the Hdu number
> -- rather than a status value.
> 
> getHduNum :: FitsFile -> FitsIO Int
> getHduNum = liftIO . getHduNum'
> 
> getHduNum' :: FitsFile -> IO (Int)
> getHduNum' a1 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   getHduNum''_ a1' a2' >>= \res ->
>   let {res' = fromIntegral res} in
>   return (res')
> 
> -- | Return the type of the current Hdu in the FITS file. The possible
> -- values for hdutype are: IMAGE_Hdu, ASCII_TBL, or BINARY_TBL.
> 
> getHduType :: FitsFile -> FitsIO HduType
> getHduType = f1 getHduType'
> 
> getHduType' :: FitsFile -> Status -> IO (Status, HduType)
> getHduType' a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   getHduType''_ a1' a2' a3' >>= \res ->
>   peekEnum a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')
> 
> movAbsHdu = f2 movAbsHdu_
> movAbsHdu_ :: FitsFile -> Int -> Status -> IO (Status, HduType)
> movAbsHdu_ a1 a2 a4 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   alloca $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   movAbsHdu_'_ a1' a2' a3' a4' >>= \res ->
>   peekEnum a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'')
> 
> movRelHdu :: FitsFile -> Int -> FitsIO HduType
> movRelHdu = f2 movRelHdu'
> 
> movRelHdu' :: FitsFile -> Int -> Status -> IO (Status, HduType)
> movRelHdu' a1 a2 a4 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   alloca $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   movRelHdu''_ a1' a2' a3' a4' >>= \res ->
>   peekEnum a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'')
> 
> movNamHdu              :: FitsFile -> HduType -> String -> Int -> FitsIO ()
> movNamHdu f t name ver = FitsIO $ \s -> do
>     s' <- movNamHdu_ f t name ver s
>     return (s, ())
> 
> movNamHdu_ :: FitsFile -> HduType -> String -> Int -> Status -> IO Status
> movNamHdu_ a1 a2 a3 a4 a5 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = cFromEnum a2} in 
>   withCString a3 $ \a3' -> 
>   let {a4' = fromIntegral a4} in 
>   withStatusConv a5 $ \a5' -> 
>   movNamHdu_'_ a1' a2' a3' a4' a5' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> -- | Copy all or part of the Hdus in the FITS file associated with
> -- infptr and append them to the end of the FITS file associated with
> -- outfptr. If 'previous' is true (not 0), then any Hdus preceding the
> -- current Hdu in the input file will be copied to the output
> -- file. Similarly, 'current' and 'following' determine whether the
> -- current Hdu, and/or any following Hdus in the input file will be
> -- copied to the output file. Thus, if all 3 parameters are true, then
> -- the entire input file will be copied. On exit, the current Hdu in
> -- the input file will be unchanged, and the last Hdu in the output
> -- file will be the current Hdu.
> copyFile :: FitsFile -> FitsFile -> Int -> Int -> Int -> Status -> IO (Status)
> copyFile a1 a2 a3 a4 a5 a6 =
>   withFitsFile a1 $ \a1' -> 
>   withFitsFile a2 $ \a2' -> 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   withStatusConv a6 $ \a6' -> 
>   copyFile'_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> -- | Copy the current Hdu from the FITS file associated with infptr
> -- and append it to the end of the FITS file associated with
> -- outfptr. Space may be reserved for MOREKEYS additional keywords in
> -- the output header.
> copyHdu :: FitsFile -> FitsFile -> Int -> Status -> IO (Status)
> copyHdu a1 a2 a3 a4 =
>   withFitsFile a1 $ \a1' -> 
>   withFitsFile a2 $ \a2' -> 
>   let {a3' = fromIntegral a3} in 
>   withStatusConv a4 $ \a4' -> 
>   copyHdu'_ a1' a2' a3' a4' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> -- | Copy the header (and not the data) from the CHdu associated with
> -- infptr to the CHdu associated with outfptr. If the current output
> -- Hdu is not completely empty, then the CHdu will be closed and a new
> -- Hdu will be appended to the output file. An empty output data unit
> -- will be created with all values initially = 0).
> copyHeader :: FitsFile -> FitsFile -> Status -> IO (Status)
> copyHeader a1 a2 a3 =
>   withFitsFile a1 $ \a1' -> 
>   withFitsFile a2 $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   copyHeader'_ a1' a2' a3' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> -- | Delete the CHdu in the FITS file. Any following Hdus will be
> -- shifted forward in the file, to fill in the gap created by the
> -- deleted Hdu. In the case of deleting the primary array (the first
> -- Hdu in the file) then the current primary array will be replace by
> -- a null primary array containing the minimum set of required
> -- keywords and no data. If there are more extensions in the file
> -- following the one that is deleted, then the the CHdu will be
> -- redefined to point to the following extension. If there are no
> -- following extensions then the CHdu will be redefined to point to
> -- the previous Hdu. The output hdutype parameter returns the type of
> -- the new CHdu. A null pointer may be given for hdutype if the
> -- returned value is not needed.
> deleteHdu :: FitsFile -> Status -> IO (Status, HduType)
> deleteHdu a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   deleteHdu'_ a1' a2' a3' >>= \res ->
>   peekEnum a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')
> 
> 
> -- | Return the number of existing keywords (not counting the END
> -- keyword) and the amount of space currently available for more
> -- keywords. It returns morekeys = -1 if the header has not yet been
> -- closed. Note that CFITSIO will dynamically add space if required
> -- when writing new keywords to a header so in practice there is no
> -- limit to the number of keywords that can be added to a header. A
> -- null pointer may be entered for the morekeys parameter if it's
> -- value is not needed.
> 
> getHdrSpace   :: FitsFile -> FitsIO (Int, Int)
> getHdrSpace f = FitsIO $ \s -> getHdrSpace' f s >>= r2
> 
> getHdrSpace' :: FitsFile -> Status -> IO (Status, Int, Int)
> getHdrSpace' a1 a4 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   alloca $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   getHdrSpace''_ a1' a2' a3' a4' >>= \res ->
>   peekIntConv a2'>>= \a2'' -> 
>   peekIntConv a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'', a3'')
> 
> readKeyString     :: FitsFile -> String -> FitsIO (String, String)
> readKeyString f n = FitsIO $ \s -> readKeyString' f n s >>= r2
> 
> readKeyString' :: FitsFile -> String -> Status -> IO (Status, String, String)
> readKeyString' a1 a2 a5 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   allocaValue $ \a3' -> 
>   allocaComment $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   readKeyString''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekCString a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'')
> 
> readKeyCInt     :: FitsFile -> String -> FitsIO (Int, String)
> readKeyCInt f n = FitsIO $ \s -> readKeyCInt' f n s >>= r2
> 
> readKeyCInt' :: FitsFile -> String -> Status -> IO (Status, Int, String)
> readKeyCInt' a1 a2 a5 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   alloca $ \a3' -> 
>   allocaComment $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   readKeyCInt''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekIntConv a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'')
> 
> readKeyCFloat     :: FitsFile -> String -> FitsIO (Float, String)
> readKeyCFloat f n = FitsIO $ \s -> readKeyCFloat' f n s >>= r2
> 
> readKeyCFloat' :: FitsFile -> String -> Status -> IO (Status, Float, String)
> readKeyCFloat' a1 a2 a5 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   alloca $ \a3' -> 
>   allocaComment $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   readKeyCFloat''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekFloatConv a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'')
> 
> readKeyCDouble     :: FitsFile -> String -> FitsIO (Double, String)
> readKeyCDouble f n = FitsIO $ \s -> readKeyCDouble' f n s >>= r2
> 
> readKeyCDouble' :: FitsFile -> String -> Status -> IO (Status, Double, String)
> readKeyCDouble' a1 a2 a5 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   alloca $ \a3' -> 
>   allocaComment $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   readKeyCDouble''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekFloatConv a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'')
> 
> {--
> int ffgkyj(fitsfile *fptr, char *keyname, long *value, char *comm, int *status);
> --}
> 
> -- | Return the nth header record in the CHU. The first keyword in the
> -- header is at keynum = 1; if keynum = 0 then these routines simply
> -- reset the internal CFITSIO pointer to the beginning of the header
> -- so that subsequent keyword operations will start at the top of the
> -- header (e.g., prior to searching for keywords using wild cards in
> -- the keyword name). The first routine returns the entire
> -- 80-character header record (with trailing blanks truncated), while
> -- the second routine parses the record and returns the name, value,
> -- and comment fields as separate (blank truncated) character
> -- strings. If a NULL comment pointer is given on input, then the
> -- comment string will not be returned.
> 
> readRecord :: FitsFile -> Int -> FitsIO String
> readRecord = f2 readRecord'
> 
> readRecord' :: FitsFile -> Int -> Status -> IO (Status, String)
> readRecord' a1 a2 a4 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   allocaCard $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   readRecord''_ a1' a2' a3' a4' >>= \res ->
>   peekCString a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'')
> 
> readKeyN     :: FitsFile -> Int -> FitsIO (String, String, String)
> readKeyN f n = FitsIO $ \s -> readKeyN' f n s >>= r3
> 
> readKeyN' :: FitsFile -> Int -> Status -> IO (Status, String, String, String)
> readKeyN' a1 a2 a6 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   allocaKeyword $ \a3' -> 
>   allocaValue $ \a4' -> 
>   allocaComment $ \a5' -> 
>   withStatusConv a6 $ \a6' -> 
>   readKeyN''_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   peekCString a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   peekCString a5'>>= \a5'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'', a5'')
> 
> readCard :: FitsFile -> String -> FitsIO String
> readCard = f2 readCard'
> 
> readCard' :: FitsFile -> String -> Status -> IO (Status, String)
> readCard' a1 a2 a4 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   allocaCard $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   readCard''_ a1' a2' a3' a4' >>= \res ->
>   peekCString a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'')
> 
> readKeyword     :: FitsFile -> String -> FitsIO (String, String)
> readKeyword f k = FitsIO $ \s -> readKeyword' f k s >>= r2
> 
> readKeyword' :: FitsFile -> String -> Status -> IO (Status, String, String)
> readKeyword' a1 a2 a5 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   allocaValue $ \a3' -> 
>   allocaComment $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   readKeyword''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekCString a3'>>= \a3'' -> 
>   peekCString a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'')
> 
> 
> -- | Get the number of rows in the current FITS table.  The number of
> -- rows is given by the NAXIS2 keyword.
> 
> getNumRows :: FitsFile -> FitsIO Int
> getNumRows = f1 getNumRows'
> 
> getNumRows' :: FitsFile -> Status -> IO (Status, Int)
> getNumRows' a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   getNumRows''_ a1' a2' a3' >>= \res ->
>   peekIntConv a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')
> 
> -- | Get the number of columns in the current FITS table.  The number
> -- of rows is given by the TFIELDS keyword in the header of the table.
> 
> getNumCols :: FitsFile -> FitsIO Int
> getNumCols = f1 getNumCols'
> 
> getNumCols' :: FitsFile -> Status -> IO (Status, Int)
> getNumCols' a1 a3 =
>   withFitsFile a1 $ \a1' -> 
>   alloca $ \a2' -> 
>   withStatusConv a3 $ \a3' -> 
>   getNumCols''_ a1' a2' a3' >>= \res ->
>   peekIntConv a2'>>= \a2'' -> 
>   let {res' = cToStatus res} in
>   return (res', a2'')

> getColNum :: FitsFile -> Bool -> String -> FitsIO Int
> getColNum = f3 getColNum'

> getColNum' :: FitsFile -> Bool -> String -> Status -> IO (Status, Int)
> getColNum' a1 a2 a3 a5 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromBool a2} in 
>   withCString a3 $ \a3' -> 
>   alloca $ \a4' -> 
>   withStatusConv a5 $ \a5' -> 
>   getColNum''_ a1' a2' a3' a4' a5' >>= \res ->
>   peekIntConv a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a4'')

> getColName                  :: FitsFile -> Bool -> String -> FitsIO (String, Int)
> getColName f casesen templt = FitsIO $ \s -> getColName' f casesen templt s >>= r2

> getColName' :: FitsFile -> Bool -> String -> Status -> IO (Status, String, Int)
> getColName' a1 a2 a3 a6 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromBool a2} in 
>   withCString a3 $ \a3' -> 
>   allocaKeyword $ \a4' -> 
>   alloca $ \a5' -> 
>   withStatusConv a6 $ \a6' -> 
>   getColName''_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   peekCString a4'>>= \a4'' -> 
>   peekIntConv a5'>>= \a5'' -> 
>   let {res' = cToStatus res} in
>   return (res', a4'', a5'')

> getColType     :: FitsFile -> Int -> FitsIO (ColType, Int, Int)
> getColType f n = FitsIO $ \s -> getColType' f n s >>= r3
> 
> getColType' :: FitsFile -> Int -> Status -> IO (Status, ColType, Int, Int)
> getColType' a1 a2 a6 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   alloca $ \a3' -> 
>   alloca $ \a4' -> 
>   alloca $ \a5' -> 
>   withStatusConv a6 $ \a6' -> 
>   getColType''_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   peekEnum a3'>>= \a3'' -> 
>   peekIntConv a4'>>= \a4'' -> 
>   peekIntConv a5'>>= \a5'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'', a5'')
> 
> getEqColType     :: FitsFile -> Int -> FitsIO (ColType, Int, Int)
> getEqColType f n = FitsIO $ \s -> getEqColType' f n s >>= r3
> 
> getEqColType' :: FitsFile -> Int -> Status -> IO (Status, ColType, Int, Int)
> getEqColType' a1 a2 a6 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   alloca $ \a3' -> 
>   alloca $ \a4' -> 
>   alloca $ \a5' -> 
>   withStatusConv a6 $ \a6' -> 
>   getEqColType''_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   peekEnum a3'>>= \a3'' -> 
>   peekIntConv a4'>>= \a4'' -> 
>   peekIntConv a5'>>= \a5'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'', a4'', a5'')
> 
> -- | Return the display width of a column. This is the length of the
> -- string that will be returned by the fits_read_col routine when
> -- reading the column as a formatted string. The display width is
> -- determined by the TDISPn keyword, if present, otherwise by the data
> -- type of the column.
> 
> getColDisplayWidth :: FitsFile -> Int -> FitsIO Int
> getColDisplayWidth = f2 getColDisplayWidth'
> 
> getColDisplayWidth' :: FitsFile -> Int -> Status -> IO (Status, Int)
> getColDisplayWidth' a1 a2 a4 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   alloca $ \a3' -> 
>   withStatusConv a4 $ \a4' -> 
>   getColDisplayWidth''_ a1' a2' a3' a4' >>= \res ->
>   peekIntConv a3'>>= \a3'' -> 
>   let {res' = cToStatus res} in
>   return (res', a3'')
> 
> -- | Return the number of and size of the dimensions of a table column
> -- in a binary table. Normally this information is given by the TDIMn
> -- keyword, but if this keyword is not present then this routine
> -- returns naxis = 1 and naxes[0] equal to the repeat count in the
> -- TFORM keyword.
> 
> readTDim           :: FitsFile -> Int -> Int -> FitsIO [Int]
> readTDim f col max = FitsIO $ \s ->
>   allocaArray max $ \p -> do
>     (t, n) <- readTDim' f col max p s
>     dim <- peekArray n p
>     return (t, map fromIntegral dim)
> 
> readTDim' :: FitsFile -> Int -> Int -> Ptr CLong -> Status -> IO (Status, Int)
> readTDim' a1 a2 a3 a5 a6 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   alloca $ \a4' -> 
>   let {a5' = id a5} in 
>   withStatusConv a6 $ \a6' -> 
>   readTDim''_ a1' a2' a3' a4' a5' a6' >>= \res ->
>   peekIntConv a4'>>= \a4'' -> 
>   let {res' = cToStatus res} in
>   return (res', a4'')
> 
> -- | Decode the input TDIMn keyword string (e.g. '(100,200)') and
> -- return the number of and size of the dimensions of a binary table
> -- column. If the input tdimstr character string is null, then this
> -- routine returns naxis = 1 and naxes[0] equal to the repeat count in
> -- the TFORM keyword. This routine is called by fits_read_tdim.
> 
> decodeTDim             :: FitsFile -> String -> Int -> Int -> FitsIO [Int]
> decodeTDim f d col max = FitsIO $ \s ->
>   allocaArray max $ \p -> do
>     (t, n) <- decodeTDim' f d col max p s
>     dim <- peekArray n p
>     return (t, map fromIntegral dim)
> 
> decodeTDim' :: FitsFile -> String -> Int -> Int -> Ptr CLong -> Status -> IO (Status, Int)
> decodeTDim' a1 a2 a3 a4 a6 a7 =
>   withFitsFile a1 $ \a1' -> 
>   withCString a2 $ \a2' -> 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   alloca $ \a5' -> 
>   let {a6' = id a6} in 
>   withStatusConv a7 $ \a7' -> 
>   decodeTDim''_ a1' a2' a3' a4' a5' a6' a7' >>= \res ->
>   peekIntConv a5'>>= \a5'' -> 
>   let {res' = cToStatus res} in
>   return (res', a5'')
> 
> -- | Write a TDIMn keyword whose value has the form '(l,m,n...)' where
> -- l, m, n... are the dimensions of a multidimension array column in a
> -- binary table.
> 
> writeTDim           :: FitsFile -> Int -> [Int] -> FitsIO ()
> writeTDim f col dim = FitsIO $ \s ->
>   withArrayLen (map fromIntegral dim) $ \n p -> do
>     t <- writeTDim' f col n p s
>     return (t, ())
> 
> writeTDim' :: FitsFile -> Int -> Int -> Ptr CLong -> Status -> IO (Status)
> writeTDim' a1 a2 a3 a4 a5 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = id a4} in 
>   withStatusConv a5 $ \a5' -> 
>   writeTDim''_ a1' a2' a3' a4' a5' >>= \res ->
>   let {res' = cToStatus res} in
>   return (res')
> 
> readColCInt :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCInt fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCInt' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCInt' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CInt -> Status -> IO (Status, Int)
> readColCInt' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCInt''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCUInt :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCUInt fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCUInt' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCUInt' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CUInt -> Status -> IO (Status, Int)
> readColCUInt' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCUInt''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCShort :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCShort fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCShort' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCShort' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CShort -> Status -> IO (Status, Int)
> readColCShort' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCShort''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCUShort :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCUShort fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCUShort' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCUShort' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CUShort -> Status -> IO (Status, Int)
> readColCUShort' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCUShort''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCLong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCLong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCLong' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCLong' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CLong -> Status -> IO (Status, Int)
> readColCLong' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCLong''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCULong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCULong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCULong' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCULong' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CULong -> Status -> IO (Status, Int)
> readColCULong' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCULong''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCLLong :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCLLong fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCLLong' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCLLong' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CLLong -> Status -> IO (Status, Int)
> readColCLLong' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCLLong''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCFloat :: FitsFile -> Int -> Int -> Int -> Int -> Float -> FitsIO ([Float], Int)
> readColCFloat fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCFloat' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map realToFrac array, fromIntegral anynul))
> 
> readColCFloat' :: FitsFile -> Int -> Int -> Int -> Int -> Float -> Ptr CFloat -> Status -> IO (Status, Int)
> readColCFloat' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = realToFrac a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCFloat''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCDouble :: FitsFile -> Int -> Int -> Int -> Int -> Double -> FitsIO ([Double], Int)
> readColCDouble fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCDouble' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map realToFrac array, fromIntegral anynul))
> 
> readColCDouble' :: FitsFile -> Int -> Int -> Int -> Int -> Double -> Ptr CDouble -> Status -> IO (Status, Int)
> readColCDouble' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = realToFrac a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCDouble''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCSByte :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCSByte fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCSByte' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCSByte' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CSChar -> Status -> IO (Status, Int)
> readColCSByte' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCSByte''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColCUByte :: FitsFile -> Int -> Int -> Int -> Int -> Int -> FitsIO ([Int], Int)
> readColCUByte fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColCUByte' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map fromIntegral array, fromIntegral anynul))
> 
> readColCUByte' :: FitsFile -> Int -> Int -> Int -> Int -> Int -> Ptr CUChar -> Status -> IO (Status, Int)
> readColCUByte' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromIntegral a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColCUByte''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColBool :: FitsFile -> Int -> Int -> Int -> Int -> Bool -> FitsIO ([Bool], Int)
> readColBool fptr colnum firstrow firstelem nelem nulval = FitsIO $ \s ->
>   allocaArray nelem $ \array' -> do
>     (t, anynul) <- readColBool' fptr colnum firstrow firstelem nelem nulval array' s
>     array <- peekArray nelem array'
>     return (t, (map toBool array, fromIntegral anynul))
> 
> readColBool' :: FitsFile -> Int -> Int -> Int -> Int -> Bool -> Ptr CChar -> Status -> IO (Status, Int)
> readColBool' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   let {a6' = fromBool a6} in 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColBool''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> readColString :: FitsFile -> Int -> Int -> Int -> Int -> String -> FitsIO ([String], Int)
> readColString fptr colnum firstrow firstelem nelem nulval = do
>   len <- getColDisplayWidth fptr colnum
>   readColStrLen fptr colnum firstrow firstelem nelem nulval (len+1)
> 
> readColStrLen :: FitsFile -> Int -> Int -> Int -> Int -> String -> Int -> FitsIO ([String], Int)
> readColStrLen fptr colnum firstrow firstelem nelem nulval len = FitsIO $ \s -> do
>   str <- mapM mallocArray (replicate nelem len)
>   withArray str $ \str' -> do
>     (t, anynul) <- readColStrLen' fptr colnum firstrow firstelem nelem nulval str' s
>     array' <- peekArray nelem str'
>     array <- mapM peekCString array'
>     mapM_ free str
>     return (t, (array, fromIntegral anynul))
> 
> readColStrLen' :: FitsFile -> Int -> Int -> Int -> Int -> String -> Ptr (Ptr CChar) -> Status -> IO (Status, Int)
> readColStrLen' a1 a2 a3 a4 a5 a6 a7 a9 =
>   withFitsFile a1 $ \a1' -> 
>   let {a2' = fromIntegral a2} in 
>   let {a3' = fromIntegral a3} in 
>   let {a4' = fromIntegral a4} in 
>   let {a5' = fromIntegral a5} in 
>   withCString a6 $ \a6' -> 
>   let {a7' = id a7} in 
>   alloca $ \a8' -> 
>   withStatusConv a9 $ \a9' -> 
>   readColStrLen''_ a1' a2' a3' a4' a5' a6' a7' a8' a9' >>= \res ->
>   peekIntConv a8'>>= \a8'' -> 
>   let {res' = cToStatus res} in
>   return (res', a8'')
> 
> foreign import ccall unsafe "fitsio.h ffgerr"
>   ffgerr :: (CInt -> ((Ptr CChar) -> (IO ())))
> 
> foreign import ccall unsafe "fitsio.h ffgmsg"
>   ffgmsg :: ((Ptr CChar) -> (IO CInt))
> 
> foreign import ccall unsafe "fitsio.h ffpmrk"
>   writeErrMark'_ :: (IO ())
> 
> foreign import ccall unsafe "fitsio.h ffcmrk"
>   clearErrMark'_ :: (IO ())
> 
> foreign import ccall unsafe "fitsio.h ffcmsg"
>   clearErrMsg'_ :: (IO ())
> 
> foreign import ccall unsafe "fitsio.h ffdelt"
>   deleteFile_'_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> (IO CInt)))
> 
> foreign import ccall unsafe "fitsio.h ffflnm"
>   ffflnm :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffflmd"
>   fileMode_'_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffopen"
>   openFile_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffdkopn"
>   openDiskFile_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffdopn"
>   openData_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h fftopn"
>   openTable_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffiopn"
>   openImage_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffinit"
>   createFile_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffdkinit"
>   createDiskFile_'_ :: ((Ptr (Ptr (FitsFile))) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffthdu"
>   getNumHdus''_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffghdn"
>   getHduNum''_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> (IO CInt)))
> 
> foreign import ccall unsafe "fitsio.h ffghdt"
>   getHduType''_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffmahd"
>   movAbsHdu_'_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffmrhd"
>   movRelHdu''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffmnhd"
>   movNamHdu_'_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CChar) -> (CInt -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffcpfl"
>   copyFile'_ :: ((Ptr (FitsFile)) -> ((Ptr (FitsFile)) -> (CInt -> (CInt -> (CInt -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffcopy"
>   copyHdu'_ :: ((Ptr (FitsFile)) -> ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffcphd"
>   copyHeader'_ :: ((Ptr (FitsFile)) -> ((Ptr (FitsFile)) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffdhdu"
>   deleteHdu'_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffghsp"
>   getHdrSpace''_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffgkys"
>   readKeyString''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgkyl"
>   readKeyCInt''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CInt) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgkye"
>   readKeyCFloat''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CFloat) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgkyd"
>   readKeyCDouble''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CDouble) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgrec"
>   readRecord''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffgkyn"
>   readKeyN''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcrd"
>   readCard''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffgkey"
>   readKeyword''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgnrw"
>   getNumRows''_ :: ((Ptr (FitsFile)) -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffgncl"
>   getNumCols''_ :: ((Ptr (FitsFile)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))
> 
> foreign import ccall unsafe "fitsio.h ffgcno"
>   getColNum''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CChar) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcnn"
>   getColName''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffgtcl"
>   getColType''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> ((Ptr CLong) -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffeqty"
>   getEqColType''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> ((Ptr CLong) -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcdw"
>   getColDisplayWidth''_ :: ((Ptr (FitsFile)) -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))
> 
> foreign import ccall unsafe "fitsio.h ffgtdm"
>   readTDim''_ :: ((Ptr (FitsFile)) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt)))))))
> 
> foreign import ccall unsafe "fitsio.h ffdtdm"
>   decodeTDim''_ :: ((Ptr (FitsFile)) -> ((Ptr CChar) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt))))))))
> 
> foreign import ccall unsafe "fitsio.h ffptdm"
>   writeTDim''_ :: ((Ptr (FitsFile)) -> (CInt -> (CInt -> ((Ptr CLong) -> ((Ptr CInt) -> (IO CInt))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvk"
>   readColCInt''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvuk"
>   readColCUInt''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CUInt -> ((Ptr CUInt) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvi"
>   readColCShort''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CShort -> ((Ptr CShort) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvui"
>   readColCUShort''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CUShort -> ((Ptr CUShort) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvj"
>   readColCLong''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CLong -> ((Ptr CLong) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvuj"
>   readColCULong''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CULong -> ((Ptr CULong) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvjj"
>   readColCLLong''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CLLong -> ((Ptr CLLong) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcve"
>   readColCFloat''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CFloat -> ((Ptr CFloat) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvd"
>   readColCDouble''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CDouble -> ((Ptr CDouble) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvsb"
>   readColCSByte''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CSChar -> ((Ptr CSChar) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvb"
>   readColCUByte''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CUChar -> ((Ptr CUChar) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvl"
>   readColBool''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> (CChar -> ((Ptr CChar) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))
> 
> foreign import ccall unsafe "fitsio.h ffgcvs"
>   readColStrLen''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> (CLLong -> ((Ptr CChar) -> ((Ptr (Ptr CChar)) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))))

-----------------------------------------------------------------------------------------

> f5 :: (b -> c -> d -> e -> g -> Status -> IO (Status, a)) -> b -> c -> d -> e -> g -> FitsIO a
> f5 f b c d e g = fp $ f b c d e g
> 
> data BitPix = BPByte
>             | BPShort
>             | BPLong
>             | BPLongLong
>             | BPFloat
>             | BPDouble
>             deriving (Show)
> instance Enum BitPix where
>   fromEnum BPByte = 8
>   fromEnum BPShort = 16
>   fromEnum BPLong = 32
>   fromEnum BPLongLong = 64
>   fromEnum BPFloat = (-32)
>   fromEnum BPDouble = (-64)
> 
>   toEnum 8 = BPByte
>   toEnum 16 = BPShort
>   toEnum 32 = BPLong
>   toEnum 64 = BPLongLong
>   toEnum (-32) = BPFloat
>   toEnum (-64) = BPDouble
>   toEnum unmatched = error ("BitPix.toEnum: Cannot match " ++ show unmatched)

> createImage = f4 createImage_

> createImage_ :: FitsFile -> BitPix -> Int -> [Word64] -> Status -> IO (Status,())
> createImage_ a1 a2 a3 a4 a5 = 
>   withFitsFile a1 $ \a1' ->  
>     allocaArray 2 $ \naxes -> do
>       mapM_ (\x -> pokeElemOff naxes x (fromIntegral (a4 !! x))) [0..((length a4) - 1)]
>       withStatusConv a5 $ \a5' ->
>         createImage''_ a1' (cFromEnum a2) (fromIntegral a3) naxes a5' >>= \res ->
>           let {res' = cToStatus res} in
>           return (res',())    

> writeImage = f5 writeImage_

> writeImage_ :: FitsFile -> ColType -> Word64 -> Word64 -> (Ptr Double) -> Status -> IO (Status,())
> writeImage_ a1 a2 a3 a4 a5 a6 = 
>   withFitsFile a1 $ \a1' -> do 
>     withStatusConv a6 $ \a6' ->
>       writeImage''_ a1' (cFromEnum a2) (fromIntegral a3) (fromIntegral a4) a5 a6' >>= \res ->
>         let {res' = cToStatus res} in
>         return (res',())    

>
> foreign import ccall unsafe "fitsio.h ffcrim"
>   createImage''_ :: ((Ptr (FitsFile)) -> (CInt -> (CInt -> ((Ptr CLong) -> ((Ptr CInt) -> IO CInt)))))
>
> foreign import ccall unsafe "fitsio.h ffppr"
>   writeImage''_ :: ((Ptr (FitsFile)) -> (CInt -> (CLLong -> (CLLong -> ((Ptr Double) -> ((Ptr CInt) -> IO CInt))))))
