{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- WaveFunction
--
-----------------------------------------------------------------------------

module Numeric.GPE.WaveFunction (
  AccWaveFunction
  , WaveFunction(..)
  , newWaveFunction, newPsi
  , createSuperPosition
  , getNorm
  , getAbs
  ) where

-----------------------------------------------------------------------------

import Data.Array.Accelerate
--import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.Data.Complex

--import qualified Data.Vector as V

import Prelude(Show(..))
import qualified Prelude as P

import Numeric.GPE.Utils
import Numeric.GPE.SimulationData

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

radius = 5

-----------------------------------------------------------------------------

type AccWaveFunction = Acc (Array DIM3 ComplexVoxel)

data WaveFunction = WaveFunction {
  _psi         :: Acc (Array DIM3 ComplexVoxel)
  , _psi_conj  :: Acc (Array DIM3 ComplexVoxel)
  , _psi_abs   :: Acc (Array DIM3 Voxel)
    --
  , psi_norm   :: Double
  } deriving(Show)
            
-----------------------------------------------------------------------------

newPsi :: SimulationData              -- ^ the simulation data
          -> Acc (Array DIM3 ComplexVoxel)
newPsi sd = let nx = (_num_x sd)
                ny = (_num_y sd)  
                nz = (_num_z sd)
            in use (fromList (Z :. nx :. ny :. nz)
                [ let qx = indexArray (_q_x_values sd) (Z:.x)
                      qy = indexArray (_q_y_values sd) (Z:.y)
                      qz = indexArray (_q_x_values sd) (Z:.z)
                  in if ((P.sqrt ((qx P.^ 2) + (qy P.^2) + (qz P.^2))) P.<= radius)
                     then (qx,qy,qz,(1 :+ 0)) 
                     else (qx,qy,qz,(0 :+ 0))
                | x <- [0..(nx-1)], y <- [0..(ny-1)], z <- [0..(nz-1)] ])

  
newWaveFunction :: SimulationData     -- ^ the simulation data
                   -> Acc (Array DIM3 ComplexVoxel)
                   -> WaveFunction    -- ^ Wave Function
newWaveFunction sd psi = WaveFunction 
                         psi
                         (map (\ix -> lift (fst4 ix,snd4 ix,trd4 ix,conjugate $ fth4 ix)) $ psi)
                         (map (\ix -> lift (fst4 ix,snd4 ix,trd4 ix,let cx = fth4 ix in (real (cx * (conjugate cx))))) $ psi)
                         0

-----------------------------------------------------------------------------

createSuperPosition :: SimulationData    -- ^ the simulation data
                       -> WaveFunction   -- ^ the wave function
                       -> WaveFunction
createSuperPosition sd wf = let lk = _laser_kick sd
                                psi = lift $ _psi wf
                                new_psi = map (\ix ->
                                                let cx = fth4 ix
                                                    (r,i) = (real cx, imag cx)
                                                    qy = snd4 ix
                                                in lift (fst4 ix,qy,trd4 ix 
                                                          ,((r+r*(cos (2*(lift lk)*qy))-i*(sin (2*(lift lk)*qy)))                             
                                                            :+ (i+i*(cos (2*(lift lk)*qy))+r*(sin (2*(lift lk)*qy)))))) psi
                            in wf { _psi = new_psi }
                               
-----------------------------------------------------------------------------

getNorm :: SimulationData      -- ^ the simulation data
           -> WaveFunction     -- ^ the wave function
           -> WaveFunction   
getNorm sd wf = let psi_sum = fth4 $ the $ fold1 add4th $ fold1 add4th $ fold1 add4th (_psi_abs wf)
                    norm_fac = sqrt (1/(psi_sum 
                                        * (lift $ _dqx sd)
                                        * (lift $ _dqy sd) 
                                        * (lift $ _dqz sd)))
                    psi_normed = map (\ix -> let cx = fth4 ix
                                                 (r,i) = (real cx, imag cx)
                                             in lift (fst4 ix,snd4 ix,trd4 ix,((r * norm_fac) :+ (i * norm_fac)))) $ (_psi wf)
                in wf { _psi = psi_normed }

-----------------------------------------------------------------------------

getAbs :: WaveFunction    -- ^ the wave function
          -> WaveFunction
getAbs wf = wf { _psi_abs = map (\ix -> let cx = fth4 ix
                                        in lift (fst4 ix,snd4 ix,trd4 ix,(real $ cx * (conjugate cx)))) $ (_psi wf) }
                      
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
