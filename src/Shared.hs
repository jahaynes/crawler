module Shared where

import Control.Concurrent               (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM           (STM, atomically)
import Control.Monad                    (when, unless)
import qualified ListT             as L
import qualified STMContainers.Set as S
--import qualified STMContainers.Map as M

sizeOfSet :: S.Set a -> STM Int
sizeOfSet = L.fold (\a _ -> return (a + 1)) 0 . S.stream

takeSet :: Int -> S.Set a -> STM [a]
takeSet n = L.toList . L.take n . S.stream

forkIO_ :: IO () -> IO ()
forkIO_ a = do
    _ <- forkIO a
    return ()

whileActive :: ThreadId -> S.Set ThreadId -> S.Set ThreadId -> IO () -> IO ()
whileActive threadId threads threadsToStop a = go
    where
    go = do
        toStop <- atomically $ do
            toStop <- S.lookup threadId threadsToStop
            when toStop $ do
                S.delete threadId threadsToStop
                S.delete threadId threads
            return toStop
        unless toStop $ do
            a
            threadDelay 1000
            go