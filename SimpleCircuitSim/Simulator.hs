{- FIXME export an applicative instead of a monad -}
{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards #-}
module SimpleCircuitSim.Simulator
    ( Net
    , Driver
    , Logic
    , Event
    , SomeNet(..) -- FIXME is this really supposed to be exported?
    , Simulation
    , loopSimWhile, loopSimUntil, loopSimFor
    , isEventQueueEmpty
    , probe, drive, float
    , probe_m
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.IORef
import Control.Monad.State

import SimpleCircuitSim.Util
import SimpleCircuitSim.Core
import SimpleCircuitSim.Simulator.Core



loopSimWhile :: Simulation Bool -> Simulation ()
loopSimWhile p = p >>= \case
    True -> stepSim >> loopSimWhile p
    False -> pure ()

loopSimUntil :: Simulation Bool -> Simulation ()
loopSimUntil p = loopSimWhile (not <$> p)

loopSimFor :: Delay -> Simulation ()
loopSimFor dt = do
    t <- Sim $ gets now
    loopSimUntil $ timeIs (t + dt)

isEventQueueEmpty :: Simulation Bool
isEventQueueEmpty = isNothing <$> nextTime

timeIs :: Time -> Simulation Bool
timeIs stopTime = nextTime >>= \case
    Just now -> pure $ now >= stopTime
    Nothing -> pure True
