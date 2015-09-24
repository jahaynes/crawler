{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Communication where

import Data.Conduit
import Control.Concurrent                   (forkIO, threadDelay)
import Control.Monad.Trans.Resource         (ResourceT, runResourceT)
import Control.Monad.IO.Class               (liftIO)
import Data.Conduit.Network
import Data.Serialize
import GHC.Generics                         (Generic)
import Data.ByteString

import Data.ByteString.Char8 as C8

data Command = AddUrl ByteString
             | RemoveUrl ByteString 
             | SetNumCrawlers Int 
             | SetNumParsers Int 
             | Idle 
               deriving (Generic, Show)

instance Serialize Command
    
withClient :: (AppData -> ResourceT IO ()) -> IO ()
withClient a =
    runTCPClient (clientSettings 1040 "127.0.0.1") $ \ad -> do
        runResourceT $ a ad
    

    
onCommand :: (Command -> IO ()) -> IO ()
onCommand f =
    runTCPServer (serverSettings 1040 "0.0.0.0") $ \ad ->
        runResourceT $
            appSource ad $$
                awaitForever $ \a -> do
                    case decode a of
                        Left str -> error $ show (a, str)
                        Right c -> liftIO $ f c

send :: Command -> AppData -> ResourceT IO ()
send command appData = yield (encode command) $$ appSink appData
    