{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GPE.Utils
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities
--
-----------------------------------------------------------------------------

module Numeric.GPE.Utils (
  Voxel, ComplexVoxel
  , padN
  , fst3, snd3, trd3
  , fst4, snd4, trd4, fth4
  , add4th
  , complexPair
  ) where

-----------------------------------------------------------------------------

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Complex

--import qualified Data.Vector as V

import Prelude(Show(..),String)
import qualified Prelude as P

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data ComplexElement = REAL | IMAGINARY deriving(P.Eq, Show, P.Ord)

type Position = Double
type Momentum = Double
type Psi = Complex Double

type Voxel = (Position,Position,Position
             ,Double)
type ComplexVoxel = (Position,Position,Position
                    ,Complex Double)

-----------------------------------------------------------------------------

padN :: Int -> Int -> String  
padN p x = let ln = P.length (show x)
           in (P.concat (P.take (p - ln) $ P.repeat "0")) P.++ (show x)

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

fst3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp a
fst3 t = let (x,y,z) = unlift t :: (Exp a,Exp b,Exp c)
         in x

snd3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp b
snd3 t = let (x,y,z) = unlift t :: (Exp a,Exp b,Exp c)
         in y

trd3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a,b,c) -> Exp c
trd3 t = let (x,y,z) = unlift t :: (Exp a,Exp b,Exp c)
         in z

-----------------------------------------------------------------------------

fst4 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp a
fst4 t = let (x,y,z,c) = unlift t :: (Exp a,Exp b,Exp c,Exp d)
         in x

snd4 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp b
snd4 t = let (x,y,z,c) = unlift t :: (Exp a,Exp b,Exp c,Exp d)
         in y

trd4 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp c
trd4 t = let (x,y,z,c) = unlift t :: (Exp a,Exp b,Exp c,Exp d)
         in z

fth4 :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a,b,c,d) -> Exp d
fth4 t = let (x,y,z,c) = unlift t :: (Exp a,Exp b,Exp c,Exp d)
         in c

-----------------------------------------------------------------------------

add4th :: (Elt a, Elt b, Elt c, Elt d, Num d) => Exp (a,b,c,d) -> Exp (a,b,c,d)
          -> Exp (a,b,c,d)
add4th t1 t2 = let x1 = fst4 t1
                   y1 = snd4 t1
                   z1 = trd4 t1
                   c1 = fth4 t1
                   c2 = fth4 t2
               in lift (x1,y1,z1,c1+c2)

-----------------------------------------------------------------------------

complexPair :: Exp (Complex Double) -> Exp (Double,Double)
complexPair t = let r = real t
                    i = imag t
                in lift (r,i)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
