module SimpleCircuitSim.Data.Signal
    ( Signal(..)
    ) where

import SimpleCircuitSim.Data.Core


instance Signal Bool where
    reconcile_m [] = Nothing
    reconcile_m [x] = Just x
    reconcile_m xs = Nothing
    showSignal = show
