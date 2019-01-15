module CountedQueue.Unbounded where

import CountedQueue (CountedQueue (..))

import Control.Concurrent.STM

data UnboundedCountedQueue a = UnboundedCountedQueue (TVar Int) (TQueue a) 

newQueueIO :: IO (UnboundedCountedQueue a)
newQueueIO = UnboundedCountedQueue <$> newTVarIO 0
                                   <*> newTQueueIO

instance CountedQueue UnboundedCountedQueue where

    readQueue (UnboundedCountedQueue sz q) = do
        modifyTVar' sz (\x -> x-1)
        readTQueue q

    writeQueue (UnboundedCountedQueue sz q) val = do
        writeTQueue q val
        modifyTVar' sz (+1)

    size (UnboundedCountedQueue sz _) =
        readTVar sz

    isEmpty (UnboundedCountedQueue sz _) =
        (==0) <$> readTVar sz
