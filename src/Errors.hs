{-# LANGUAGE OverloadedStrings #-}

module Errors where

import Types

import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import CountedQueue                     (sourceQueue)
import Data.ByteString.Char8 as C8      (ByteString, concat)

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary

logErrors :: CrawlerState -> LogFunction -> IO ()
logErrors crawlerState logFunction =
    runResourceT $ sourceQueue (getLogQueue crawlerState)
                 $$ awaitForever $ liftIO . logFunction

