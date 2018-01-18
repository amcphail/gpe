{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE DeriveAnyClass #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GPE.SimulationData
-- Copyright   :  (c) A. V. H. McPhail 2017
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Simulation Data
--
-----------------------------------------------------------------------------

module Numeric.GPE.SimulationData (
  SimulationData(..)
  , newSimulationData
  ) where

-----------------------------------------------------------------------------

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import System.FilePath
import System.Directory

--import qualified Data.Vector as V

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Complex

--import Prelude hiding((++),length,splitAt,take,concat)
import qualified Prelude as P
import Prelude(Show(..),Monad(..),IO(..),String)

import Numeric.GPE.Utils

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

rootFolder = "Data"

length_x = 30
length_y = 120
length_z = 30

dt_factor = 0.001

--num_I_steps = 100000
--num_R_steps = 100000

num_I_steps = 1000
num_R_steps = 1000

sigma_x = 1
sigma_y = 1
sigma_z = 1

gamma_x = 1
gamma_y = 1.5
gamma_z = 3

beta = 2

laser_kick = 7

-----------------------------------------------------------------------------

data SimulationData = SimulationData {
  _time         :: UTCTime
  --
  , _folder     :: FilePath 
    --
  , _num_x       :: Int
  , _num_y       :: Int
  , _num_z       :: Int
    --
  , _num         :: Int
    --
  , _length_x   :: Double
  , _length_y   :: Double
  , _length_z   :: Double
    --
  , _num_R_steps :: Int
  , _num_I_steps :: Int
    --
  , _sigma_x    :: Double
  , _sigma_y    :: Double
  , _sigma_z    :: Double
    --
  , _gamma_x    :: Double
  , _gamma_y    :: Double
  , _gamma_z    :: Double
    --
  , _beta       :: Double
    --
--  , _chemical_potential :: Double
    --
  , _q_x_values :: Vector Double
  , _q_y_values :: Vector Double
  , _q_z_values :: Vector Double
    --
  , _dqx        :: Double
  , _dqy        :: Double
  , _dqz        :: Double
  , _dqx2       :: Double
  , _dqy2       :: Double
  , _dqz2       :: Double
    --
  , _dt          :: Double
    --  
  , _p_x_values :: Vector Double
  , _p_y_values :: Vector Double
  , _p_z_values :: Vector Double
    --  
--  , _dpx        :: Double
--  , _dpy        :: Double
--  , _dpz        :: Double
    --
  , _laser_kick :: Double
  } deriving(Show)

newSimulationData :: Int                  -- ^ num_x
                     -> Int               -- ^ num_y
                     -> Int               -- ^ num_z
                     -> IO SimulationData -- ^ Simulation Data
newSimulationData nx ny nz = do
  time <- getCurrentTime
  let (y,mo,d) = toGregorian (utctDay time)
  let tod = timeToTimeOfDay (utctDayTime time)   
      (h,mi) = (todHour tod, todMin tod) 
  let folder = rootFolder </> (padN 4 (fromInteger y)) P.++ (padN 2 mo) P.++ (padN 2 d)  
               P.++ (padN 2 h) P.++ (padN 2 mi)
  let dx = length_x / (P.fromIntegral nx)
      dy = length_y / (P.fromIntegral ny)
      dz = length_z / (P.fromIntegral nz)
  let ax = -(P.fromIntegral nx)/(P.fromIntegral 2)
      bx = (P.fromIntegral nx)/2-1
      ay = -(P.fromIntegral ny)/2
      by = (P.fromIntegral ny)/2-1
      az = -(P.fromIntegral nz)/2
      bz = (P.fromIntegral nz)/2-1
  let il_x = (2*pi/length_x) 
      il_y = (2*pi/length_y) 
      il_z = (2*pi/length_z) 
  let step_x = il_x*((bx-ax)/((P.fromIntegral nx)-1))
      step_y = il_y*((by-ay)/((P.fromIntegral ny)-1))
      step_z = il_z*((bz-az)/((P.fromIntegral nz)-1))
  let p_x' = [ (P.fromIntegral x) * step_x + il_x * ax | x <- [0..(nx-1)] ]
      p_y' = [ (P.fromIntegral y) * step_y + il_y * ay | y <- [0..(ny-1)] ]
      p_z' = [ (P.fromIntegral z) * step_z + il_z * az | z <- [0..(nz-1)] ]
  let (pxf,pxb) = P.splitAt (nx `P.div` 2) p_x'
      (pyf,pyb) = P.splitAt (ny `P.div` 2) p_y'
      (pzf,pzb) = P.splitAt (nz `P.div` 2) p_z'
  return $ SimulationData time folder
    nx ny nz
    (nx*ny*nz)
    length_x length_y length_z
    num_I_steps num_R_steps
    sigma_x sigma_y sigma_z
    gamma_x gamma_y gamma_z
    beta
    (fromList (Z:.nx) [ (P.fromIntegral x) * dx - (length_x / 2)  | x <- [0..(nx-1)] ])
    (fromList (Z:.ny) [ (P.fromIntegral y) * dy - (length_y / 2)  | y <- [0..(ny-1)] ])
    (fromList (Z:.nz) [ (P.fromIntegral z) * dz - (length_z / 2)  | z <- [0..(nz-1)] ])
    dx dy dz
    (dx P.^ 2) (dy P.^ 2) (dz P.^ 2)
    (dt_factor*dx)
    (fromList (Z:.nx) $ pxb P.++ pxf)
    (fromList (Z:.ny) $ pyb P.++ pyf)
    (fromList (Z:.nz) $ pzb P.++ pzf)
    laser_kick
    
-----------------------------------------------------------------------------
      
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
