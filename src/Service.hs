{-# LANGUAGE DataKinds,
             FlexibleInstances,
             MultiParamTypeClasses,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Service where

import Communication
import           CountedQueue       (CountedQueue)
import qualified CountedQueue as CQ
import Crawl                                      (processNextUrl)
import qualified PoliteQueue as PQ
import Shared
import Types
import Urls                                       (canonicaliseString)                 

import           Control.Concurrent.STM           (atomically, readTVarIO, writeTVar)
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
             :<|> "stop"                                        :> Post '[JSON] ()

crawlerApi :: Proxy CrawlerApi
crawlerApi = Proxy

crawlerApp :: CountedQueue bq => Crawler bq -> Workers -> Application
crawlerApp crawler workers = serve crawlerApi (crawlerServer crawler workers)

crawlerServer :: CountedQueue bq => Crawler bq -> Workers -> Server CrawlerApi
crawlerServer crawler workers = status
                           :<|> workerStatus
                           :<|> queueSize
                           :<|> addUrl
                           :<|> addIncludePattern
                           :<|> stop

    where
    status :: Handler CrawlerStatus
    status = liftIO . readTVarIO . getCrawlerStatus $ crawler

    workerStatus :: Handler [String]
    workerStatus = liftIO $ do
        ls <- atomically $ mapAsList (getActiveThreads workers)
        map show <$> mapM (threadStatus . fst) ls

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
    addIncludePattern pat = liftIO . atomically $ S.insert (C8.pack pat) (getUrlPatterns crawler)

    stop :: Handler ()
    stop = liftIO . atomically $ writeTVar (getCrawlerStatus crawler) HaltingStatus

start :: CountedQueue bq => Crawler bq -> Workers -> IO ()
start crawler workers = run 8081 (crawlerApp crawler workers)
