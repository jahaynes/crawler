module CountedQueue where

import Control.Concurrent.STM (STM)

class CountedQueue q where
    readQueue   :: q a -> STM a
    writeQueue  :: q a -> a -> STM ()
    size        :: q a -> STM Int
    isEmpty     :: q a -> STM Bool
