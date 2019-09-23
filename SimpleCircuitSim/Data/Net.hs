{-#LANGUAGE RecordWildCards #-}
module SimpleCircuitSim.Data.Net
    ( Net_ -- FIXME I probly want to only export Net
    , NetId(..)
    , NetName(..)
    , emptyNet
    , getNetRef
    , setNetRef
    , getNetListeners
    , addNetListener
    , stepNet
    ) where

import SimpleCircuitSim.Util.DblBuf
import SimpleCircuitSim.Data.Core

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.State


emptyNet :: NetData_ m a
emptyNet = NetData (startBuf Map.empty) Set.empty

readNetRef :: MonadIO m => Net_ m' a -> m (NetData_ m' a)
readNetRef Net{..} = liftIO $ readIORef cell

writeNetRef :: MonadIO m => Net_ m' a -> NetData_ m' a -> m ()
writeNetRef Net{..} = liftIO . writeIORef cell

modifyNetRef :: MonadIO m => Net_ m' a -> (NetData_ m' a -> NetData_ m' a) -> m ()
modifyNetRef Net{..} = liftIO . modifyIORef cell


getNetRef :: MonadIO m => Net_ m' a -> m [a]
getNetRef netRef = Map.elems . getBuf . drivenTo <$> readNetRef netRef

setNetRef :: MonadIO m => Driver_ m' a -> Maybe a -> m ()
setNetRef Driver{..} Nothing = modifyNetRef driveOn $ \net ->
    net { drivenTo = modifyBuf (drivenTo net) $ Map.delete driverId }
setNetRef Driver{..} (Just val) = modifyNetRef driveOn $ \net ->
    net { drivenTo = modifyBuf (drivenTo net) $ Map.insert driverId val }


getNetListeners :: MonadIO m => Net_ m' a -> m (Set (Logic_ m'))
getNetListeners netRef = listeners <$> readNetRef netRef

addNetListener :: MonadIO m => Net_ m' a -> Logic_ m' -> m ()
addNetListener r logic = modifyNetRef r $ \net ->
    net { listeners = Set.insert logic (listeners net) }


stepNet :: MonadIO m => Net_ m' a -> m ()
stepNet r = modifyNetRef r $ \net ->
    net { drivenTo = stepBuf (drivenTo net) }
