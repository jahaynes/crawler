module Shared where

import Control.Concurrent               (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM           (STM, atomically)
import Control.Monad                    (when, unless)
import Data.Char                        (isSpace)
import qualified Data.ByteString.Char8  as C8
import qualified ListT             as L
import qualified StmContainers.Set as S
import qualified StmContainers.Map as M

mapAsList :: M.Map a b -> STM [(a,b)]
mapAsList = L.toList . M.listT

sizeOfSet :: S.Set a -> STM Int
sizeOfSet = L.fold (\a _ -> return (a + 1)) 0 . S.listT

both :: (a -> b) -> (a,a) -> (b,b)
both f (k,v) = (f k, f v)

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

trim :: C8.ByteString -> C8.ByteString
trim = C8.reverse . C8.dropWhile isSpace . C8.reverse . C8.dropWhile isSpace
