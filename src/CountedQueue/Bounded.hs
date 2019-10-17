module CountedQueue.Bounded where

import CountedQueue (CountedQueue (..))

import Control.Concurrent.STM

data BoundedCountedQueue a = BoundedCountedQueue (TVar Int) (TBQueue a) 

newQueueIO :: Int -> IO (BoundedCountedQueue a)
newQueueIO i = BoundedCountedQueue <$> newTVarIO 0
                                   <*> newTBQueueIO i

instance CountedQueue BoundedCountedQueue where

    readQueue (BoundedCountedQueue sz bq) = do
        modifyTVar' sz (\x -> x-1)
        readTBQueue bq

    writeQueue (BoundedCountedQueue sz bq) val = do
        writeTBQueue bq val
        modifyTVar' sz (+1)

    size (BoundedCountedQueue sz _) =
        readTVar sz

    isEmpty (BoundedCountedQueue sz _) =
        (==0) <$> readTVar sz
