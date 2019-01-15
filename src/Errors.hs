{-# LANGUAGE OverloadedStrings #-}

module Errors where

import CountedQueue                     (sourceQueue)
import Types

import Control.Monad.IO.Class           (liftIO)
import Data.Conduit

type LogFunction = Loggable -> IO ()

logErrors :: Crawler -> LogFunction -> IO ()
logErrors crawler logFunction =
    runConduit $ sourceQueue (getLogQueue crawler)
              .| (awaitForever $ liftIO . logFunction)
