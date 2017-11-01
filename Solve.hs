{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GPE.Solve
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Solve
--
-----------------------------------------------------------------------------

module Numeric.GPE.Solve (
  solveImag
  , solveReal
  ) where

-----------------------------------------------------------------------------

import Data.Array.Accelerate
--import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Math.DFT

--import qualified Data.Vector as V

import Prelude(Show(..))
import qualified Prelude as P

import Control.Monad

import System.FilePath

import Numeric.GPE.Utils
import Numeric.GPE.SimulationData
import Numeric.GPE.WaveFunction
import Numeric.GPE.Potential
import Numeric.GPE.SaveData

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

saveImage :: Int -> Bool -> SimulationData -> WaveFunction -> P.IO (P.Maybe P.String)
saveImage i b sd p = do
            let f = if b 
                    then "psi" P.++ (show i) P.++ ".fits"
                    else "phi" P.++ (show i) P.++ ".fits"
            let fn = (_folder sd) </> f 
            save2DImage sd p fn

-----------------------------------------------------------------------------

solveImag :: SimulationData                 -- ^ simulation data
             -> WaveFunction                -- ^ wave function
             -> Potential                   -- ^ potential
             -> P.IO (WaveFunction,Potential) 
solveImag sd wf pot = do
          let i_steps = _num_I_steps sd
          foldM (\(w,p) i -> do
                let (w',p') = imagStep sd w p
                if i `P.mod` 100 P.== 0
                then P.putStrLn $ "Imaginary step " P.++ (show i) P.++ " out of " P.++ (show i_steps)
                else return ()
                return (w',p')) (wf,pot) [1..i_steps]


imagStep :: SimulationData                  -- ^ simulation data
            -> WaveFunction                 -- ^ wave function
            -> Potential                    -- ^ potential
            -> (WaveFunction,Potential)
imagStep sd wf pot = let wf' = getAbs wf
                         wf'' = getNorm sd wf'
                         pot' = calcNonLinearEnergy sd wf'' pot
                         pot'' = assignPositionTimeEvol sd pot' True False
                         P.Just p = _p_time_evolution pot''
                         wf''' = zipWith mul4th (_psi wf'') p
                         ft = zipWith sub4th wf''' $ dft $ map fth4 wf'''
                         P.Just q = _q_time_evolution pot''
                         wf'''' = zipWith mul4th ft q
                         ift = idft (map fth4 wf'''')
                         wf''''' = zipWith sub4th wf''''' $ zipWith (*) ift p
                     in (wf'' { _psi = wf'''''},pot'')
                        
-----------------------------------------------------------------------------


solveReal :: SimulationData                 -- ^ simulation data
             -> WaveFunction                -- ^ wave function
             -> Potential                   -- ^ potential
             -> P.IO (WaveFunction,Potential) 
solveReal sd wf pot = do
          let r_steps = _num_R_steps sd
          foldM (\(w,p) i -> do
                let wf' = getAbs wf 
                if i `P.mod` 500 P.== 0
                then do
                     P.putStrLn $ "Real step " P.++ (show i) P.++ " out of " P.++ (show r_steps)
                     saveImage (i `P.mod` 500) True sd wf'
                else return P.Nothing
                let (w',p') = realStep1 sd (_psi wf') p
                let wf'' = getAbs $ wf' { _psi = w' }
                if i `P.mod` 500 P.== 0
                then saveImage (i `P.mod` 500) False sd wf''
                else return P.Nothing
                let (w'',p'') = realStep2 sd (_psi wf'') p'
                return (wf'' { _psi = w''},p'')) (wf,pot) [1..r_steps]

realStep1 :: SimulationData                  -- ^ simulation data
            -> AccWaveFunction                 -- ^ wave function
            -> Potential                    -- ^ potential
            -> (AccWaveFunction,Potential)
realStep1 sd wf pot = let pot' = assignPositionTimeEvol sd pot True True
                          P.Just p = _p_time_evolution pot'
                          wf' = zipWith mul4th wf p
                          ft = zipWith sub4th wf' $ dft $ map fth4 wf'
                      in (ft,pot)

realStep2 :: SimulationData                  -- ^ simulation data
            -> AccWaveFunction                 -- ^ wave function
            -> Potential                    -- ^ potential
            -> (AccWaveFunction,Potential)
realStep2 sd wf pot = let P.Just q = _q_time_evolution pot
                          wf' = zipWith mul4th wf q 
                          ift = idft (map fth4 wf')
                          P.Just p = _p_time_evolution pot
                          wf'' = zipWith sub4th wf' $ zipWith (*) ift p
                      in (wf'',pot)
                        


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
