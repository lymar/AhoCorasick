-- Simple FIFO queue in ST monad
module Text.AhoCorasick.Internal.Deque (
      mkDQ
    , pushBack
    , popFront
    , dqLength
    , DQ
    ) where

import Control.Monad.ST.Strict
import Data.STRef

data DQNode a s = DQNode {
      dqnData    :: a
    , dqnNext    :: Maybe (STRef s (DQNode a s))
    }

type DQ a s = STRef s (
    Maybe (
        STRef s (DQNode a s), 
        STRef s (DQNode a s)
        ),
    Int)

mkDQ :: ST s (DQ a s)
mkDQ = newSTRef (Nothing, 0)

pushBack :: DQ a s -> a -> ST s ()
pushBack dq dt = do
    dqr <- readSTRef dq
    case dqr of
        (Nothing,_) -> do
            nn <- newSTRef $ DQNode dt Nothing
            writeSTRef dq (Just (nn, nn), 1)
        (Just (f,l),lng) -> do
            nn <- newSTRef $ DQNode dt Nothing
            modifySTRef l (\v -> v {dqnNext = Just nn})
            writeSTRef dq $ (Just (f,nn),lng + 1)

popFront :: DQ a s -> ST s (Maybe a)
popFront dq = do
    dqr <- readSTRef dq
    case dqr of
        (Nothing,_) -> return Nothing
        (Just (f,l),lng) -> do
            fd <- readSTRef f
            case dqnNext fd of
                Nothing -> writeSTRef dq (Nothing, 0)
                Just k -> writeSTRef dq $ (Just (k,l),lng-1)
            return $ Just $ dqnData fd

dqLength :: DQ a s -> ST s Int
dqLength dq = do
    (_,l) <- readSTRef dq
    return l
