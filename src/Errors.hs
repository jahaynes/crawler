module Errors where

import Types
import qualified Data.ByteString.Char8  as C8
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever)
import Control.Concurrent.STM.TBQueue   (readTBQueue)

instance Show Loggable where
    show (LoggableWarning url message) = showError url message
    show (LoggableError url message) = showError url message

showError url message = "While crawling: " ++ show url ++ "\n"
                    ++ " found the following issue: " ++ "\n"
                    ++ C8.unpack message ++ "\n"

logErrors :: CrawlerState -> IO ()
logErrors crawlerState = forever $ do
    nextError <- atomically $ readTBQueue (getLogQueue crawlerState)
    appendFile "errors.log" (show nextError ++ "\n")
