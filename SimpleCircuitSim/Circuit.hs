{- FIXME export an applicative instead of a monad -}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module SimpleCircuitSim.Circuit
    ( Circuit
    , runCircuit
    , withModule
    , newNet
    , newDriver
    , newLogic
    , initially
    ) where

import Control.Monad.State
import Data.IORef

import SimpleCircuitSim.Util
import SimpleCircuitSim.Core
import SimpleCircuitSim.Simulator -- FIXME apparently, circuits are dependent on simulation, which is odd
import SimpleCircuitSim.Data.Core ( Net_(..), NetData_(..), Driver_(..), Logic_(..) ) -- FIXME narrow down these imports


type Circuit a = StateT CircuitState IO a

data CircuitState = CircuitState
    { currentModuleName :: [String]
    , freeLogicIds :: [LogicId]
    , freeDriverIds :: [DriverId]
    , freeNetIds :: [NetId]
    , powerOn :: Simulation ()
    }
startCircuit :: CircuitState
startCircuit = CircuitState
    { currentModuleName = []
    , freeLogicIds = LogicId <$> [0..] -- FIXME couldn't I just make an Enum instance?
    , freeDriverIds = DriverId <$> [0..] -- FIXME couldn't I just make an Enum instance?
    , freeNetIds = NetId <$> [0..] -- FIXME couldn't I just make an Enum instance?
    , powerOn = pure ()
    }

runCircuit :: Circuit a -> IO (a, Simulation ())
runCircuit circuit = do
    (a, CircuitState{powerOn}) <- runStateT circuit startCircuit
    pure (a, powerOn)

circuitLog :: String -> Circuit ()
circuitLog = liftIO . putStrLn


newNet :: Signal a => String -> Circuit (Net a)
newNet name = do
    (netId:freeNetIds') <- gets freeNetIds
    modify $ \s -> s { freeNetIds = freeNetIds' }
    moduleName <- gets currentModuleName
    let netName = NetName $ moduleName `snoc` name
    cell <- liftIO $ newIORef emptyNet
    circuitLog $ concat ["<ν.net> ", show netId, ": ", show netName]
    pure Net{..}

newDriver :: Signal a => String -> Net a -> Circuit (Driver a)
newDriver name driveOn = do
    (driverId:freeDriverIds') <- gets freeDriverIds
    modify $ \s -> s { freeDriverIds = freeDriverIds' }
    let driverName = DriverName name
    let driver = Driver{..}
    circuitLog $ concat ["<ν.drv> ", show driverId, ": ", show $ netId driveOn, " ← ", show driverName]
    pure driver

newLogic :: String -> [SomeNet] -> Simulation () -> Circuit Logic
newLogic name listenTo updateLogic = do
    (logicId:free') <- gets freeLogicIds
    modify $ \s -> s { freeLogicIds = free' }
    moduleName <- gets currentModuleName
    let logicName = LogicName $ moduleName `snoc` name
    let logic = Logic{..}
    circuitLog $ concat ["<ν.lgc> ", show logicId, ": ", show logicName]
    forM_ listenTo $ \(SomeNet netRef) ->
        addNetListener netRef logic
    pure logic

withModule :: String -> Circuit a -> Circuit a
withModule name action = do
    oldName <- gets currentModuleName
    modify $ \s -> s { currentModuleName = oldName `snoc` name }
    a <- action
    modify $ \s -> s { currentModuleName = oldName } -- FIXME do I need to wrap this in a finally?
    pure a




initially :: Signal a => Driver a -> a -> Circuit ()
initially driver v = do
    modify $ \s -> s {
        powerOn = powerOn s >> drive 0 driver v
    }



