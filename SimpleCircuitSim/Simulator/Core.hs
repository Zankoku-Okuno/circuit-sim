{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards, NamedFieldPuns #-}
module SimpleCircuitSim.Simulator.Core where

import Data.Set (Set)
import qualified Data.Set as Set
import SimpleCircuitSim.Util.EventQueue (EventQueue)
import qualified SimpleCircuitSim.Util.EventQueue as EQ
import Control.Monad
import Data.IORef
import Control.Monad.State

import SimpleCircuitSim.Util
import SimpleCircuitSim.Core
import SimpleCircuitSim.Data.Core ( Net_(..), NetData_(..), Driver_(..), Logic_(..) ) -- FIXME narrow down these imports


type Net a = Net_ Simulation a
type NetData a = NetData_ Simulation a
type Driver a = Driver_ Simulation a
type Logic = Logic_ Simulation



data SomeNet = forall a. Signal a => SomeNet (Net a)

instance Eq SomeNet where
    (SomeNet a) == (SomeNet b) = netId a == netId b
instance Ord SomeNet where
    compare (SomeNet a) (SomeNet b) = compare (netId a) (netId b)



data Event = forall a. Signal a => Event
    { driver :: Driver a
    , signal :: Maybe a
    , source :: Maybe Logic
    }



newtype Simulation a = Sim { unSim :: StateT SimState IO a }
    deriving (Functor, Applicative, Monad)

data SimState = SimState
    { now :: Time
    , eventQueue :: EventQueue Time Event
    , logicQueue :: Set Logic
    , netQueue :: Set SomeNet
    , currentLogic :: Maybe Logic
    }

startState :: SimState
startState = SimState
    { now = 0
    , eventQueue = EQ.empty
    , logicQueue = Set.empty
    , netQueue = Set.empty
    , currentLogic = Nothing
    }

runSimulation :: Simulation b -> Simulation a -> IO (a, SimState)
runSimulation powerOn simulation = runStateT (unSim (powerOn >> simulation)) startState


simulationLog :: String -> Simulation ()
simulationLog = Sim . liftIO . putStrLn


probe :: Signal a => Net a -> Simulation a
probe = (fromJust <$>) . probe_m

probe_m :: Signal a => Net a -> Simulation (Maybe a)
probe_m netRef = reconcile_m <$> Sim (getNetRef netRef)

drive :: Signal a => Delay -> Driver a -> a -> Simulation ()
drive dt driver v = do
    t0 <- Sim $ gets now
    source <- Sim $ gets currentLogic
    let signal = Just v
    let ev = Event{..}
    Sim $ modify' $ \s -> s { eventQueue = EQ.schedule (t0 + dt) ev (eventQueue s) }

float :: Signal a => Delay -> Driver a -> Simulation ()
float dt driver = do
    t0 <- Sim $ gets now
    source <- Sim $ gets currentLogic
    let signal = Nothing
    let ev = Event{..}
    Sim $ modify' $ \s -> s { eventQueue = EQ.schedule (t0 + dt) ev (eventQueue s) }


stepSim :: Simulation ()
stepSim = whenNextTime $ \t -> do
    simulationLog $ concat ["<ε.time> t=", show t]
    Sim $ modify $ \s -> s { now = t }
    evs <- drainEventQueue t
    case evs of
        [] -> pure ()
        evs -> do
            forM_ evs handleEvent
            stepNets
            stepLogic
            -- NOTE I decided not to loop, just in case time doesn't advance

handleEvent :: Event -> Simulation ()
handleEvent Event{..} = do
    let Driver{..} = driver
    Sim $ setNetRef driver signal
    simulationLog $ case signal of
        Just signal -> concat ["<ε.drive> ", maybe "" (("["++) . (++"] ") . show . logicId) source, show driverId, ": ", showSignal signal]
        Nothing -> concat ["<ε.release> ", show driverId]
    ls <- Sim $ getNetListeners driveOn
    Sim $ modify $ \s -> s {
        -- FIXME only add to the queue if it's different (either to the last, or to the next)
        netQueue = Set.insert (SomeNet driveOn) (netQueue s),
        logicQueue = Set.union ls (logicQueue s)
    }

stepNets :: Simulation ()
stepNets = do
    q <- drainNetQueue
    forM_ q $ \(SomeNet r) -> do
        Sim $ stepNet r
        probe_m r >>= \case
            Just v -> simulationLog $ concat ["<ε.net> ", show $ netId r, "=", showSignal v]
            Nothing -> simulationLog $ concat ["<ε.net> ", show $ netId r, " [ERROR]"]

stepLogic :: Simulation ()
stepLogic = do
    q <- drainLogicQueue
    forM_ q $ \l -> withLogic l $
        updateLogic l


nextTime :: Simulation (Maybe Time)
nextTime = Sim $ gets (EQ.nextTime . eventQueue)

whenNextTime :: (Time -> Simulation ()) -> Simulation ()
whenNextTime action = Sim (gets (EQ.nextTime . eventQueue)) >>= \case
    Nothing -> pure ()
    Just t -> action t

drainEventQueue :: Time -> Simulation [Event]
drainEventQueue t = do
    eq <- Sim $ gets eventQueue
    -- FIXME I'm assuming there's no event back in time, but it's some complex pre-/postconditions to prove it
    let (evs, eq') = EQ.drain t eq
    Sim $ modify' $ \s -> s { eventQueue = eq' }
    pure evs

drainNetQueue :: Simulation (Set SomeNet)
drainNetQueue = Sim $ do
    q <- gets netQueue
    modify $ \s -> s { netQueue = Set.empty }
    pure q

drainLogicQueue :: Simulation (Set Logic)
drainLogicQueue = Sim $ do
    q <- gets logicQueue
    modify $ \s -> s { logicQueue = Set.empty }
    pure q

withLogic :: Logic -> Simulation a -> Simulation a
withLogic l action = do
    oldLogic <- Sim $ gets currentLogic
    Sim $ modify $ \s -> s { currentLogic = Just l }
    a <- action
    Sim $ modify $ \s -> s { currentLogic = oldLogic }
    pure a