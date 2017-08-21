{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GPE.Potential
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Potential
--
-----------------------------------------------------------------------------

module Numeric.GPE.Potential (
  newPotential
  , calcNonLinearEnergy
  , assignPositionTimeEvol
  , assignMomentumTimeEvol
  ) where

-----------------------------------------------------------------------------

import Data.Array.Accelerate
--import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.Data.Complex

--import qualified Data.Vector as V

import Prelude(Show(..),Maybe(..))
import qualified Prelude as P

import Numeric.GPE.Utils
import Numeric.GPE.SimulationData
import Numeric.GPE.WaveFunction
--import Numeric.GPE.SaveData

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data Potential = Potential {
  _harmonic_trap      :: Acc (Array DIM3 Double)
  , _kinetic_energy   :: Acc (Array DIM3 Double)
  , _non_linear       :: Maybe (Acc (Array DIM3 Double))
  , _q_time_evolution :: Maybe (Acc (Array DIM3 (Complex Double)))
  , _p_time_evolution :: Maybe (Acc (Array DIM3 (Complex Double)))
  } deriving(Show)
            
-----------------------------------------------------------------------------

newPotential :: SimulationData      -- ^ the simulation data
                -> Potential
newPotential sd = let nx = (_num_x sd)
                      ny = (_num_y sd)  
                      nz = (_num_z sd)
                      gamma_x = (_gamma_x sd)
                      gamma_y = (_gamma_y sd)
                      gamma_z = (_gamma_z sd)
                      gx2 = gamma_x P.^ 2
                      gy2 = gamma_y P.^ 2
                      gz2 = gamma_z P.^ 2
                  in Potential
                     (use (fromList (Z :. nx :. ny :. nz)
                      [ let qx = indexArray (_q_x_values sd) (Z:.x)
                            qy = indexArray (_q_y_values sd) (Z:.y)
                            qz = indexArray (_q_x_values sd) (Z:.z)
                        in (0.5 P.* gx2 P.* qx P.^ 2
                            +0.5 P.* gy2 P.* qy P.^ 2
                            +0.5 P.* gz2 P.* qz P.^ 2)
                      | x <- [0..(nx-1)], y <- [0..(ny-1)], z <- [0..(nz-1)] ]))
                     (use (fromList (Z :. nx :. ny :. nz)
                      [ let px = indexArray (_p_x_values sd) (Z:.x)
                            py = indexArray (_p_y_values sd) (Z:.y)
                            pz = indexArray (_p_x_values sd) (Z:.z)
                        in (0.5 P.* px P.^ 2
                            +0.5 P.* py P.^ 2
                            +0.5 P.* pz P.^ 2)
                      | x <- [0..(nx-1)], y <- [0..(ny-1)], z <- [0..(nz-1)] ]))
                     Nothing
                     Nothing
                     Nothing
                            
-----------------------------------------------------------------------------

calcNonLinearEnergy :: SimulationData      -- ^ the simulation data
                       -> WaveFunction     -- ^ the wave function 
                       -> Potential        -- ^ the potential
                       -> Potential
calcNonLinearEnergy sd wf pot = let beta = lift $ _beta sd
                                    non_lin = map (\ix -> beta * (fth4 ix)) (_psi_abs wf)  
                                in pot { _non_linear = Just non_lin }


-----------------------------------------------------------------------------

assPos :: Bool -> Bool -> Exp Double -> Exp Double -> Exp Double -> Exp (Complex Double)
assPos trap_on is_real dt non_lin har_trap =
  if is_real 
    then if trap_on
         then let theta = (non_lin + har_trap) * 0.5 * dt in lift ((cos theta) :+ (-(sin theta))) 
         else let theta = non_lin * 0.5 * dt in lift ((cos theta) :+ (-(sin theta))) 
    else let theta = (non_lin + har_trap) * 0.5 * dt in lift ((exp (-theta)) :+ 0)

assignPositionTimeEvol :: SimulationData   -- ^ the simulation data
                       -> Potential        -- ^ the potential
                       -> Bool             -- ^ is trap pn  
                       -> Bool             -- ^ is real
                       -> Potential
assignPositionTimeEvol sd pot trap_on is_real = let dt = _dt sd
                                                    zs = zipWith (assPos trap_on is_real (lift dt)) ((\(Just x) -> x) $ (_non_linear pot)) (_harmonic_trap pot)
                                                in pot { _q_time_evolution = Just zs }
                                                   
-----------------------------------------------------------------------------

assMom :: Bool -> Exp Double -> Exp Double -> Exp (Complex Double)
assMom is_real dt kin_energy =
  let theta = kin_energy * dt 
  in if is_real 
       then lift ((cos theta) :+ (-(sin theta))) 
       else lift ((exp (-theta)) :+ 0)

assignMomentumTimeEvol :: SimulationData   -- ^ the simulation data
                       -> Potential        -- ^ the potential
                       -> Bool             -- ^ is real
                       -> Potential
assignMomentumTimeEvol sd pot is_real = let dt = _dt sd
                                            ms = map (assMom is_real (lift dt)) (_kinetic_energy pot)
                                        in pot { _p_time_evolution = Just ms }
                                                   
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
