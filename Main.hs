-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Main
--
-----------------------------------------------------------------------------

module Main (
  main
  ) where

-----------------------------------------------------------------------------

import Numeric.GPE.Utils
import Numeric.GPE.SimulationData
import Numeric.GPE.WaveFunction
import Numeric.GPE.Potential
import Numeric.GPE.SaveData
import Numeric.GPE.Solve

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

load_bin = False

main :: IO ()
main = do
     sd <- newSimulationData 128 1024 128
     let pot = newPotential sd
     let wf = newWaveFunction sd $ newPsi sd
     let wf' = getAbs wf
     let wf'' = getNorm sd wf'
     let pot' = calcNonLinearEnergy sd wf'' pot
     let pot'' = assignPositionTimeEvol sd pot' True False
     let pot''' = assignMomentumTimeEvol sd pot'' False
     (wf''',pot'''') <- solveImag sd wf'' pot'''
     let wf'''' = getAbs wf'''
     let wf''''' = getNorm sd wf''''
     let wf'''''' = createSuperPosition sd wf'''''
     let wf''''''' = getAbs wf''''''
     let wf'''''''' = getNorm sd wf'''''''
     let pot''''' = assignPositionTimeEvol sd pot'''' True False
     let pot'''''' = assignMomentumTimeEvol sd pot''''' False
     (_,_) <- solveReal sd wf'''''''' pot''''''
     return ()

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
