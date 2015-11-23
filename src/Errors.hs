module Errors where

import Types

import Control.Monad.Trans.Resource     (runResourceT)
import CountedQueue                     (sourceQueue)
import Data.ByteString.Char8            (ByteString, pack, unpack)

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary
import System.IO

instance Show Loggable where
    show (LoggableWarning url message) = showError url message
    show (LoggableError url message) = showError url message

showError :: CanonicalUrl -> ByteString -> String
showError url message = "While crawling: " ++ show url ++ "\n"
                    ++ " found the following issue: " ++ "\n"
                    ++ unpack message ++ "\n"

logErrors :: CrawlerState -> IO ()
logErrors crawlerState =
    runResourceT $ sourceQueue (getLogQueue crawlerState)
                 $$ CL.map (\x -> pack $ show x ++ "\n")
                 =$ sinkFile "errors.log"
