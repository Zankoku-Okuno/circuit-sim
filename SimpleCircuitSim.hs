module SimpleCircuitSim
    ( module X
    , Time, Delay
    , NetId, NetName
    , DriverId, DriverName
    , LogicId, LogicName
    , Signal(..)
    , simulate
    ) where

import SimpleCircuitSim.Core
import SimpleCircuitSim.Circuit as X
import SimpleCircuitSim.Simulator as X
import SimpleCircuitSim.Simulator.Core (runSimulation)

simulate :: Circuit a -> (a -> Simulation b) -> IO b
simulate circuit simulation = do
    (a, powerOn) <- runCircuit circuit
    (b, finalState) <- runSimulation powerOn (simulation a)
    pure b
