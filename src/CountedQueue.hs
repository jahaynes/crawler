module CountedQueue where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM.TBQueue as BQ
import qualified Control.Concurrent.STM.TVar as TV

data CountedQueue a = CountedQueue (TV.TVar Int) (Q.TQueue a) 
                    | BoundedCountedQueue (TV.TVar Int) (BQ.TBQueue a) 

data BoundedNess = Unbounded
                 | Bounded Int

newQueueIO :: BoundedNess -> IO (CountedQueue a)
newQueueIO Unbounded = do
    s <- TV.newTVarIO 0
    q <- Q.newTQueueIO
    return $ CountedQueue s q
newQueueIO (Bounded n) = do
    s <- TV.newTVarIO 0
    bq <- BQ.newTBQueueIO n
    return $ BoundedCountedQueue s bq

isEmpty :: CountedQueue a -> STM Bool
isEmpty (CountedQueue sz _) = (==0) <$> TV.readTVar sz
isEmpty (BoundedCountedQueue sz _) = (==0) <$> TV.readTVar sz

readQueue :: CountedQueue a -> STM a
readQueue (CountedQueue sz q) = TV.modifyTVar' sz (\x -> x-1) >> Q.readTQueue q
readQueue (BoundedCountedQueue sz bq) = TV.modifyTVar' sz (\x -> x-1) >> BQ.readTBQueue bq

writeQueue :: CountedQueue a -> a -> STM ()
writeQueue (CountedQueue sz q) val = Q.writeTQueue q val >> TV.modifyTVar' sz (+1)
writeQueue (BoundedCountedQueue sz bq) val = BQ.writeTBQueue bq val >> TV.modifyTVar' sz (+1)

size :: CountedQueue a -> STM Int
size (CountedQueue sz _) = TV.readTVar sz
size (BoundedCountedQueue sz _) = TV.readTVar sz