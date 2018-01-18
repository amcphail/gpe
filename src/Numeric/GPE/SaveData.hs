{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GPE.WaveFunction
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- SaveData
--
-----------------------------------------------------------------------------

module Numeric.GPE.SaveData (
       save2DImage
  ) where

-----------------------------------------------------------------------------

import Foreign.Marshal

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Conversion.From as BSF

import Data.Array.Accelerate
import Data.Array.Accelerate.IO.Data.ByteString
import Data.Array.Accelerate.IO.Foreign.ForeignPtr
--import Data.Array.Accelerate.LLVM.Interpreter
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Data.Complex

--import qualified Data.Vector as V

import Prelude(Show(..),IO(..),Maybe(..))
import qualified Prelude as P

import Numeric.GPE.Utils
import Numeric.GPE.SimulationData
import Numeric.GPE.WaveFunction
import Numeric.GPE.Potential

import Data.Fits.FitsIO

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
                      
save2DImage :: SimulationData              -- ^ the simulation data
               -> WaveFunction             -- ^ the wave function
               -> P.FilePath                 -- ^ the file name
               -> IO (Maybe P.String)
save2DImage sd wf fn = let fpixel = 1
                           naxis = 2
                           num_x = _num_x sd
                           num_y = _num_y sd
                           nelements = num_x * num_y
                           image = (run $ transpose $ map fth4 $ fold1 add4th (_psi_abs wf)) :: Array DIM2 Double 
                       in do 
                         (status,_) <- runFitsIO (do
                           fits_file <- createDiskFile fn
                           createImage fits_file BPDouble naxis [P.fromIntegral num_x,P.fromIntegral num_y]
                           fp (\stat -> allocaArray nelements $ \arr -> do
                             toPtr image arr
                             (status',_) <- writeImage_ fits_file TDouble fpixel (P.fromIntegral nelements) arr stat
                             P.return (status',()))) noError
                         if status P.== noError
                           then P.return Nothing
                           else do
                             status_string <- getErrStatus status    
                             P.return $ Just status_string
                   

save3DImage :: SimulationData              -- ^ the simulation data
               -> WaveFunction             -- ^ the wave function
               -> P.FilePath                 -- ^ the file name
               -> IO (Maybe P.String)
save3DImage sd wf fn = let fpixel = 1
                           naxis = 3
                           num_x = _num_x sd
                           num_y = _num_y sd
                           num_z = _num_z sd
                           nelements = num_x * num_y * num_z
                           image = (run $ map fth4 (_psi_abs wf)) :: Array DIM3 Double
                       in do
                         (status,_) <- runFitsIO (do
                           fits_file <- createDiskFile fn                          
                           createImage fits_file BPDouble naxis [P.fromIntegral num_x,P.fromIntegral num_y,P.fromIntegral num_z]
                           fp (\stat -> allocaArray nelements $ \arr -> do
                             toPtr image arr
                             (status',_) <- writeImage_ fits_file TDouble fpixel (P.fromIntegral nelements) arr stat
                             P.return (status',()))) noError
                         if status P.== noError
                           then P.return Nothing
                           else do
                             status_string <- getErrStatus status    
                             P.return $ Just status_string

-----------------------------------------------------------------------------

{-

save2DPotential :: SimulationData              -- ^ the simulation data
                   -> Potential                -- ^ the wave function
                   -> FilePath                 -- ^ the file name
                   -> IO ()
-}
-----------------------------------------------------------------------------
{-
saveBinary :: SimulationData               -- ^ the simulation data
              -> WaveFunction              -- ^ the wave function
              -> FilePath                  -- ^ the file name
              -> IO ()
saveBinary sd wf fn = do              
-}              
{-              
loadBinary :: FilePath                             -- ^ the file name
              -> IO (SimulationData,WaveFunction)  

-}

-----------------------------------------------------------------------------

readWaveFunction :: P.FilePath                 -- ^ the file path
                    -> IO WaveFunction
readWaveFunction fn = do
  bs <- BSL.readFile fn
  let (x',r) = BSL.splitAt 8 bs 
      (Just x'') = BSF.fromByteString $ BSL.toStrict x' :: Maybe Int64
      x = P.fromIntegral x'' 
      (y',r') = BSL.splitAt 8 r
      (Just y'') = BSF.fromByteString $ BSL.toStrict y' :: Maybe Int64
      y = P.fromIntegral y''
      (z',r'') = BSL.splitAt 8 r'
      (Just z'') = BSF.fromByteString $ BSL.toStrict z' :: Maybe Int64
      z = P.fromIntegral z''
      (bsr,bsi) = BSL.splitAt (P.fromIntegral $ x*y*z) $ r''
  psi' <- fromByteString (Z :. x :. y :. z) (((),BSL.toStrict bsr),BSL.toStrict bsi)
  sd <- newSimulationData x y z
  let psi = imap (getCoordinates sd) (use psi')
  P.return $ newWaveFunction sd psi
    where
      getCoordinates :: SimulationData -> Exp DIM3 -> Exp (Complex Double)
                        -> Exp ComplexVoxel
      getCoordinates sd ix a = let ix' = unindex3 ix 
                                   x = fst3 ix' :: Exp Int
                                   y = snd3 ix' :: Exp Int
                                   z = trd3 ix' :: Exp Int
                                   qx = (use (_q_x_values sd)) ! (lift $ Z :. x)
                                   qy = (use (_q_y_values sd)) ! (lift $ Z :. y)
                                   qz = (use (_q_x_values sd)) ! (lift $ Z :. z)
                               in lift (qx,qy,qz,a) 


writeWaveFunction :: P.FilePath                -- ^ the file path
                    -> WaveFunction            -- ^ the wave function  
                    -> IO ()
writeWaveFunction fn wf = do
  let psi = run $ map complexPair $ map fth4 $ (_psi wf)
      (Z :. x :. y :. z) = arrayShape psi
      x' = BSB.toLazyByteString $ BSB.int64LE $ P.fromIntegral x
      y' = BSB.toLazyByteString $ BSB.int64LE $ P.fromIntegral y
      z' = BSB.toLazyByteString $ BSB.int64LE $ P.fromIntegral z
  (((),bsr),bsi) <- toByteString psi 
  BSL.writeFile fn (x' `BSL.append` y' `BSL.append` z' 
                       `BSL.append` (BSL.fromStrict bsr) 
                       `BSL.append` (BSL.fromStrict bsi))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

