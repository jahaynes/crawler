module Errors where

import Types

import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever)
import CountedQueue                     (readQueue)
import Data.ByteString.Char8            (ByteString, unpack)

instance Show Loggable where
    show (LoggableWarning url message) = showError url message
    show (LoggableError url message) = showError url message

showError :: CanonicalUrl -> ByteString -> String
showError url message = "While crawling: " ++ show url ++ "\n"
                    ++ " found the following issue: " ++ "\n"
                    ++ unpack message ++ "\n"

logErrors :: CrawlerState -> IO ()
logErrors crawlerState = forever $ do
    nextError <- atomically $ readQueue (getLogQueue crawlerState)
    appendFile "errors.log" (show nextError ++ "\n")
