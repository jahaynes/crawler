module Errors where

import CountedQueue (CountedQueue, readQueue)
import Types

import Control.Concurrent.STM (atomically)
import Control.Monad          (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit

type LogFunction = Loggable -> IO ()

logErrors :: CountedQueue bq => Crawler bq -> LogFunction -> IO ()
logErrors crawler logFunction =
    runConduit $ sourceQueue (getLogQueue crawler)
              .| (awaitForever $ liftIO . logFunction)

sourceQueue :: (MonadIO m, CountedQueue q) => q a -> ConduitT i a m b
sourceQueue q = forever $ (liftIO . atomically . readQueue $ q) >>= yield
