{-# LANGUAGE OverloadedStrings #-}

module Errors where

import CountedQueue                     (sourceQueue)
import Types

import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit

type LogFunction = Loggable -> IO ()

logErrors :: Crawler -> LogFunction -> IO ()
logErrors crawler logFunction =
    runResourceT $ sourceQueue (getLogQueue crawler)
                 $$ awaitForever $ liftIO . logFunction

