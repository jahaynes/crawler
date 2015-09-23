{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- module Communication where

import Data.Conduit
--import Data.Conduit.Binary as CB
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async as A
import Control.Monad   
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Conduit.Network
import Data.Serialize
import GHC.Generics (Generic)
import Data.ByteString

import Data.ByteString.Char8 as C8

data Command = AddUrl ByteString
             | RemoveUrl ByteString deriving (Generic, Show)

instance Serialize Command
         
main :: IO ()
main = do
    
    _ <- forkIO $ onCommand $ commandHandler
    
    threadDelay 10000
    
    client
    
    
client =
    runTCPClient (clientSettings 1040 "127.0.0.1") $ \ad -> do
        
        runResourceT $ do
            as <- mapM (\_ -> liftIO $ A.async ( mapM_ (\i -> send i ad) [1..20])) [1..20]
            
            _ <- liftIO $ mapM A.wait as
            return ()
    
    
commandHandler :: Command -> IO ()
commandHandler (AddUrl url) = print $ "Received add url" ++ show url
commandHandler (RemoveUrl url) = print $ "Received remove url" ++ show url
    
onCommand :: (Command -> IO ()) -> IO ()
onCommand f =
    runTCPServer (serverSettings 1040 "0.0.0.0") $ \ad ->
        runResourceT $
            appSource ad $$
                awaitForever $ \a -> do
                    case decode a of
                        Left str -> error $ show (a, str)
                        Right c -> liftIO $ f c

send :: Int -> AppData -> IO ()
send i appData = go i
    where
    go 0 = return ()
    go x = do
        yield (encode (AddUrl (C8.pack (show i)))) $$ appSink appData
        go (x-1)