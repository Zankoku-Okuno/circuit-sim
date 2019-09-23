{-  The `m` argument to the type constructors in this module
    are only here to avoid a dependency on the Simulation type.
-}
{-# LANGUAGE ExistentialQuantification #-}
module SimpleCircuitSim.Data.Core where

import SimpleCircuitSim.Util
import SimpleCircuitSim.Util.DblBuf

import Data.Set (Set)
import Data.Map (Map)
import Data.IORef



type Time = Word
type Delay = Word



class Signal a where
    reconcile_m :: [a] -> Maybe a
    reconcile :: [a] -> a
    reconcile = fromJust . reconcile_m
    showSignal :: a -> String



data Net_ m a = Net
    { netId :: NetId
    , netName :: NetName
    , cell :: IORef (NetData_ m a)
    }

data NetData_ m a = NetData
    { drivenTo :: DblBuf (Map DriverId a)
    , listeners :: Set (Logic_ m)
    }

newtype NetName = NetName [String]
newtype NetId = NetId Word
    deriving (Eq, Ord)
instance Eq (Net_ m a) where (==) = (==) `on` netId
instance Ord (Net_ m a) where compare = compare `on` netId



data Driver_ m a = Driver
    { driverId :: DriverId
    , driverName :: DriverName
    , driveOn :: Net_ m a
    }

newtype DriverName = DriverName String
newtype DriverId = DriverId Word
    deriving (Eq, Ord)
instance Eq (Driver_ m a) where (==) = (==) `on` driverId
instance Ord (Driver_ m a) where compare = compare `on` driverId



data Logic_ m = Logic
    { logicId :: LogicId
    , logicName :: LogicName
    , updateLogic :: m ()
    }

newtype LogicName = LogicName [String]
newtype LogicId = LogicId Word
    deriving (Eq, Ord)
instance Eq (Logic_ m) where (==) = (==) `on` logicId
instance Ord (Logic_ m) where compare = compare `on` logicId



instance Show NetId where show (NetId id) = showHex id ""
instance Show DriverId where show (DriverId id) = showHex id ""
instance Show LogicId where show (LogicId id) = showHex id ""

instance Show NetName where show (NetName names) = intercalate "." names
instance Show DriverName where show (DriverName name) = name
instance Show LogicName where show (LogicName names) = intercalate "." names
