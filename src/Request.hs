module Request where

import Control.Concurrent.STM
import Control.Monad.IO.Class       (liftIO)
import Data.CaseInsensitive         (mk)
import GHC.Exception                (SomeException)
import Network.HTTP.Conduit
import Network.HTTP.Types           (methodPost)

import Settings
import Types

buildRequest :: CrawlerSettings -> [Cookie] -> DownloadRequest -> WebIO Request
buildRequest crawlerSettings requestCookies downloadRequest = do

    mProxySettings <- liftIO . readTVarIO . getProxySettings $ crawlerSettings

    case makeRequest mProxySettings of
        Left l -> webErr $ show l
        Right req -> return req

    where
    makeRequest :: Maybe ProxySettings -> Either SomeException Request 
    makeRequest mProxySettings = do

        req <- parseRequest . show . getUrl $ downloadRequest

        return . applyParametersFrom downloadRequest
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

        applyParametersFrom :: DownloadRequest -> (Request -> Request)
        applyParametersFrom (GetRequest _) = id
        applyParametersFrom (FormRequest     _ formMethod _ (FormParameters params))
            | mk formMethod == mk methodPost = urlEncodedBody params
            | otherwise                      = setQueryString (map (\(k,v) -> (k,Just v)) params)