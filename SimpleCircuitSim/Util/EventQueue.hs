{- FIXME there's no protection to ensure that events aren't added in the past -}
module SimpleCircuitSim.Util.EventQueue
    ( EventQueue
    , empty
    , null
    , nextTime
    , drain
    , schedule
    ) where

import Prelude hiding (null)
import Data.Map (Map)
import qualified Data.Map as Map


newtype EventQueue t e = EventQueue (Map t [e])

empty :: EventQueue t e
empty = EventQueue Map.empty

nextTime :: Ord t => EventQueue t e -> Maybe t
nextTime (EventQueue eq) = lookupMin eq
    where
    lookupMin :: Ord k => Map k v -> Maybe k
    lookupMin map = case Map.keys map of
        [] -> Nothing
        ks -> Just $ minimum ks

drain :: Ord t => t -> EventQueue t e -> ([e], EventQueue t e)
drain t (EventQueue eq) = (Map.findWithDefault [] t eq, EventQueue $ Map.delete t eq)

schedule :: Ord t => t -> e -> EventQueue t e -> EventQueue t e
schedule t e (EventQueue eq) = EventQueue $ Map.alter f t eq
    where
    f Nothing = Just [e]
    f (Just es) = Just (e:es)

null :: EventQueue t e -> Bool
null (EventQueue eq) = Map.null eq