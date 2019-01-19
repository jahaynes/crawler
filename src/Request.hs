module Request where

import Prelude hiding (fail)

import DownloadRequest
import Settings
import Types

import Control.Concurrent.STM
import Control.Monad.Fail     (MonadFail, fail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Exception          (SomeException)
import Network.HTTP.Conduit

buildRequest :: (MonadIO m, MonadFail m, DownloadRequest dr, Show dr)
             => CrawlerSettings -> [Cookie] -> dr -> m Request
buildRequest crawlerSettings requestCookies downloadRequest = do

    mProxySettings <- liftIO . readTVarIO . getProxySettings $ crawlerSettings

    case makeRequest mProxySettings of
        Left ex   -> fail $ unwords ["Could not makeRequest: ", show downloadRequest, "for reason", show ex]
        Right req -> return req

    where
    makeRequest :: Maybe ProxySettings -> Either SomeException Request 
    makeRequest mProxySettings = do

        req <- parseRequest . show . getUrl $ downloadRequest

        pure . applyParametersFrom downloadRequest
             . basicAuthSettings
             . applyProxy
             $ req {
                 requestHeaders = customHeaders,
                 redirectCount = 0,
                 cookieJar = Just (createCookieJar requestCookies)
               }

        where
        applyProxy :: Request -> Request
        applyProxy = case mProxySettings of
                         Nothing -> id
                         Just (ProxySettings pAddress pPort) -> addProxy pAddress pPort
