{-# LANGUAGE OverloadedStrings,
             InstanceSigs,
             DeriveGeneric,
             DeriveAnyClass #-}

module Communication where

import Data.Aeson                           (ToJSON)
import Data.Text    as T
import GHC.Generics                         (Generic)
import Servant (FromHttpApiData(..)) 
import Safe (readEitherSafe)

data QueueName = UrlQueue
               | StoreQueue
               | ErrorQueue
                   deriving (Generic, Read)

instance FromHttpApiData QueueName where
    parseQueryParam :: Text -> Either Text QueueName
    parseQueryParam text =
        case readEitherSafe (T.unpack text) of
            Left _ -> Left $ T.concat ["Unknown queueName: ", text]
            Right queueName -> Right queueName

data CrawlerStatus = RunningStatus
                   | IdleStatus
                   | HaltingStatus
                   | Halted
                       deriving (Generic, Eq, Show, ToJSON)
