module Main where

import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM.TVar as TV

data CountedQueue a = CountedQueue {
    getQueue :: Q.TQueue a,
    getSize :: TV.TVar Int}

newIO :: IO (CountedQueue a)
newIO = do
    q <- Q.newTQueueIO
    s <- TV.newTVarIO 0
    return $ CountedQueue q s
    
main :: IO ()
main = do

    print "Bleh" 
