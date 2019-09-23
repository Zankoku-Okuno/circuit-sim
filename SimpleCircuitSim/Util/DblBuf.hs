{-#LANGUAGE NamedFieldPuns#-}
module SimpleCircuitSim.Util.DblBuf
    ( DblBuf
    , startBuf
    , stepBuf
    , getBuf
    , modifyBuf
    ) where


data DblBuf a = DblBuf
    { _bufNow :: a
    , _bufNext :: a
    }
    deriving (Show)

startBuf :: a -> DblBuf a
startBuf x = DblBuf x x

stepBuf :: DblBuf a -> DblBuf a
stepBuf DblBuf{_bufNext} = DblBuf _bufNext _bufNext

getBuf :: DblBuf a -> a
getBuf = _bufNow

modifyBuf :: DblBuf a -> (a -> a) -> DblBuf a
modifyBuf buf f = buf { _bufNext = f $ _bufNext buf }