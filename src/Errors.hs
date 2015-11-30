{-# LANGUAGE OverloadedStrings #-}

module Errors where

import Types

import Control.Monad.Trans.Resource     (runResourceT)
import CountedQueue                     (sourceQueue)
import Data.ByteString.Char8 as C8      (ByteString, concat)

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary

logErrors :: CrawlerState -> IO ()
logErrors crawlerState =
    runResourceT $ sourceQueue (getLogQueue crawlerState)
                 $$ CL.map toLogLine
                 =$ sinkFile "errors.log"

    where
    toLogLine :: Loggable -> ByteString
    toLogLine (LoggableWarning (CanonicalUrl url) message) = toLogLine' url message
    toLogLine (LoggableError (CanonicalUrl url) message) = toLogLine' url message

    toLogLine' :: ByteString -> ByteString -> ByteString
    toLogLine' url message = C8.concat ["While crawling: ", url, "\n",
                                        "found the following issue: \n",
                                        message, "\n"]
