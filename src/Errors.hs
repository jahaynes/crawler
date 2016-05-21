{-# LANGUAGE OverloadedStrings #-}

module Errors where

import CountedQueue                     (sourceQueue)
import Types

import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.Conduit

logErrors :: CrawlerState -> LogFunction -> IO ()
logErrors crawlerState logFunction =
    runResourceT $ sourceQueue (getLogQueue crawlerState)
                 $$ awaitForever $ liftIO . logFunction

