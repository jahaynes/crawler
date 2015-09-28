{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Communication where

import Data.Conduit
import Control.Monad.Trans.Resource         (runResourceT)
import Control.Monad.IO.Class               (liftIO)
import Data.Conduit.Network
import Data.Serialize
import GHC.Generics                         (Generic)
import Data.ByteString                      (ByteString)

data Message = CommandMessage Command
             | QuestionMessage Question
             | AnswerMessage Answer
               deriving (Generic, Show)

data Command = AddUrl ByteString
             | RemoveUrl ByteString 
             | SetNumCrawlers Int 
             | SetNumParsers Int
             | SetUrlPatterns [ByteString]
             | Idle 
               deriving (Generic, Show)

data Question = GetQueueSize QueueName
                deriving (Generic, Show)

data QueueName = UrlQueue
               | ParseQueue
               | StoreQueue
               | ErrorQueue
                 deriving (Generic, Show, Read)

data Answer = Confirmation
            | Failure String
            | QueueSize Int
              deriving (Generic, Show)

instance Serialize Message
instance Serialize Command
instance Serialize Question
instance Serialize QueueName
instance Serialize Answer

sendAndGetReply :: Message -> IO Message
sendAndGetReply msg = do
    runTCPClient (clientSettings 1040 "127.0.0.1") $ \ad -> do
        runResourceT $ do
            yield (encode msg) $$ appSink ad
            appSource ad $$ do
                a <- await
                return $
                    case a of
                        Nothing -> AnswerMessage (Failure "Got no reply back")
                        Just x ->
                            case decode x of
                                Left e -> AnswerMessage (Failure "Couldn't decode")
                                Right r -> r

receiveMessagesWith :: (Message -> IO Message) -> IO ()
receiveMessagesWith f =
    runTCPServer (serverSettings 1040 "0.0.0.0") $ \ad ->
        runResourceT $
            appSource ad $$ process $= appSink ad
    where
    process =
        awaitForever $ \a ->
            case decode a of
                Left str -> error $ show (a, str)
                Right c -> do
                    r <- liftIO $ f c
                    yield . encode $ r
