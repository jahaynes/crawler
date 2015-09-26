{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Communication where

import Data.Conduit
import Control.Monad.Trans.Resource         (ResourceT, runResourceT)
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
             | Idle 
               deriving (Generic, Show)

data Question = GetNumCrawlers
                deriving (Generic, Show)

data Answer = Confirmation
            | NumCrawlers Int
              deriving (Generic, Show)

instance Serialize Message
instance Serialize Command
instance Serialize Question
instance Serialize Answer

sendAndGetReply :: Message -> IO ()
sendAndGetReply msg = do
    runTCPClient (clientSettings 1040 "127.0.0.1") $ \ad -> do
        runResourceT $
            appSource ad $$ process $= appSink ad
    where
    process = do
        yield (encode msg)
        a <- await
        case a of
            Nothing -> liftIO $ putStrLn "No reply"
            Just x ->
                case decode x :: Either String Message of
                    Left e -> liftIO $ print e
                    Right v -> liftIO $ print v

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

    