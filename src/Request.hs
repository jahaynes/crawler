module Request where

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import GHC.Exception                (SomeException)
import Network.HTTP.Conduit
import Network.HTTP.Types           (methodPost)

import Settings
import Types

buildRequest :: [Cookie] -> DownloadRequest -> WebIO Request
buildRequest requestCookies downloadRequest =
    case makeRequest of
        Left l -> webErr $ show l
        Right req -> return req

    where
    makeRequest :: Either SomeException Request 
    makeRequest = do

        req <- parseRequest . show . getUrl $ downloadRequest

        return . applyParametersFrom downloadRequest
            . basicAuthSettings
            . proxySettings 
            $ req {
                requestHeaders = customHeaders,
                redirectCount = 0,
                cookieJar = Just (createCookieJar requestCookies)
                }

        where
        applyParametersFrom :: DownloadRequest -> (Request -> Request)
        applyParametersFrom (GetRequest _) = id
        applyParametersFrom (FormRequest     _ formMethod _ (FormParameters params))
            | mk formMethod == mk methodPost = urlEncodedBody params
            | otherwise                      = setQueryString (map (\(k,v) -> (k,Just v)) params)