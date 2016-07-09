module Request where

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import Control.Monad.Trans.Either   (EitherT, left, right)
import GHC.Exception                (SomeException)
import Network.HTTP.Conduit
import Network.HTTP.Types           (methodPost)

import Settings
import Types

buildRequest :: Monad m => [Cookie] -> DownloadRequest -> EitherT String m Request 
buildRequest requestCookies downloadRequest =
    case makeRequest of
        Left ex -> left $ "Could not make request from " ++ show downloadRequest ++ "\nThe reason was: " ++ show ex
        Right req -> right req

    where
    makeRequest :: Either SomeException Request 
    makeRequest = do

        req <- parseRequest . show . getUrl $ downloadRequest

        return . applyParametersFrom downloadRequest
            . basicAuthSettings
            . proxySettings 
            $ req {
                redirectCount = 0,
                cookieJar = Just (createCookieJar requestCookies)
                }

        where
        applyParametersFrom :: DownloadRequest -> (Request -> Request)
        applyParametersFrom (GetRequest _) = id
        applyParametersFrom (FormRequest     _ formMethod _ (FormParameters params))
            | mk formMethod == mk methodPost = urlEncodedBody params
            | otherwise                      = setQueryString (map (\(k,v) -> (k,Just v)) params)