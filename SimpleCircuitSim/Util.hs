module SimpleCircuitSim.Util
    ( snoc
    , module X
    , showHex
    ) where

import Data.Function as X
import Data.List as X
import Data.Maybe as X
import Numeric (showHex)

xs `snoc` x = xs ++ [x]
