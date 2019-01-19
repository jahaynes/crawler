module Errors where

import CountedQueue (CountedQueue, readQueue)
import Types        (Loggable)

import Control.Concurrent.STM (atomically)
import Control.Monad          (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit

data ErrorLogger bq =
    ErrorLogger { getLogQueue    :: bq Loggable
                , getLogFunction :: Loggable -> IO ()
                }

logErrors :: CountedQueue bq => ErrorLogger bq -> IO ()
logErrors (ErrorLogger logQueue logFunction) =
    runConduit $ sourceQueue logQueue
              .| awaitForever (liftIO . logFunction)

    where
    sourceQueue q = forever $ (liftIO . atomically . readQueue $ q) >>= yield
