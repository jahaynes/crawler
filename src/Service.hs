{-# LANGUAGE DataKinds,
             DeriveAnyClass,
             DeriveGeneric,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service where

import Communication
import qualified CountedQueue as CQ
import Crawl                                      (processNextUrl)
import qualified PoliteQueue as PQ
import Shared
import Types
import Urls                                       (canonicaliseString)                 

import           Control.Concurrent.STM           (atomically, readTVar)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           GHC.Conc                         (threadStatus)
import           Network.Wai.Handler.Warp         (run)
import           Servant
import qualified StmContainers.Set          as S

type CrawlerApi = "status"                                      :> Get '[JSON] CrawlerStatus
             :<|> "workerStatus"                                :> Get '[JSON] [String]
             :<|> "queueSize"         :> Capture "x" QueueName  :> Get '[JSON] Int
             :<|> "addUrl"            :> ReqBody '[JSON] String :> Post '[JSON] ()
             :<|> "addIncludePattern" :> ReqBody '[JSON] String :> Post '[JSON] ()

crawlerApi :: Proxy CrawlerApi
crawlerApi = Proxy

crawlerApp :: Crawler -> Workers -> Application
crawlerApp crawler workers = serve crawlerApi (crawlerServer crawler workers)

crawlerServer :: Crawler -> Workers -> Server CrawlerApi
crawlerServer crawler workers = status
                           :<|> workerStatus
                           :<|> queueSize
                           :<|> addUrl
                           :<|> addIncludePattern

    where
    status :: Handler CrawlerStatus
    status = liftIO . atomically . readTVar . getCrawlerStatus $ crawler

    workerStatus :: Handler [String]
    workerStatus = liftIO $ do
        ls <- atomically $ mapAsList (getActiveThreads workers)
        statuses <- mapM (threadStatus . fst) ls
        {- let ns = zip (map snd ls) statuses
            ss = map (\(n,s) -> n ++ ": " ++ show s) ns
            return $ WorkerStatus ss 0 -}
        return $ map show statuses

    queueSize :: QueueName -> Handler Int
    queueSize queueName = liftIO . atomically $
        case queueName of
            UrlQueue   -> PQ.size . getUrlQueue   $ crawler
            StoreQueue -> CQ.size . getStoreQueue $ crawler
            ErrorQueue -> CQ.size . getLogQueue   $ crawler

    addUrl :: String -> Handler ()
    addUrl rawUrl = 
        case canonicaliseString rawUrl of
             Nothing -> throwError $ err400 {errBody = L8.concat ["Could not understand url: "
                                                                 , L8.pack rawUrl]}
             Just url -> do
                 result <- liftIO $ processNextUrl crawler url
                 case result of
                     Left l -> throwError $ err400 {errBody = L8.fromStrict l}
                     Right () -> return ()

    addIncludePattern :: String -> Handler ()
    addIncludePattern pattern = liftIO . atomically $
        S.insert (C8.pack pattern) (getUrlPatterns crawler)

start :: Crawler -> Workers -> IO ()
start crawler workers = run 8081 (crawlerApp crawler workers)
