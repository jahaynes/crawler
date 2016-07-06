{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Settings
import Types

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import Control.Monad                (when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString.Char8        (unpack)
import Data.ByteString.Lazy.Char8   (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 (($$+-), ($=))
import Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import GHC.Exception                (SomeException)
import Data.List                    (group)
import Data.Maybe                   (catMaybes)
import Network.HTTP.Conduit
import Safe                         (readMay)
import Network.HTTP.Types           

makeRequest :: [Cookie] -> DownloadRequest -> Either SomeException Request 
makeRequest requestCookies downloadRequest = do

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

getWithRedirects :: Manager
                 -> [Cookie]
                 -> DownloadRequest
                 -> IO (Either String DownloadResponse, [CanonicalUrl])
getWithRedirects man requestCookies downloadRequest = do

    case makeRequest requestCookies downloadRequest of
        Left ex -> return (Left $ "Could not make request from " ++ show downloadRequest ++ "\nThe reason was: " ++ show ex, [])

        Right req -> do

            runResourceT $ do
                (mResponse, mRedirects) <- followRedirects maxRedirects req [canonicaliseRequest req]

                mContent <- downloadEnoughContent mResponse

                --Include the initial url in the redirects
                let redirects = catMaybes mRedirects ++ [getUrl downloadRequest]

                when (length redirects < length mRedirects + 1) (liftIO $ putStrLn "Warning, not all redirects were parsed!")

                return (mContent, dedupe redirects)

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    downloadEnoughContent :: Either String DownloadSource -> (ResourceT IO) (Either String DownloadResponse)
    downloadEnoughContent mResponse =
        case mResponse of
            Left l -> return (Left l)
            Right response -> do

                let responseCookies = destroyCookieJar . responseCookieJar $ response

                case getContentLength response of
                    Just x | x <= maxContentLength -> do
                                bs <- responseBody response $$+- (toStrict <$> sinkLbs)
                                case BS.last bs of _ -> return (Right (bs, responseCookies))
                           | otherwise -> return (Left "Too big")
                    Nothing -> do
                        bs <- responseBody response $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)
                        case BS.last bs of _ -> return (Right (bs, responseCookies))

        where
        getContentLength :: Response a -> Maybe Int
        getContentLength response =
            case filter (\(k,_) -> mk k == mk hContentLength) . responseHeaders $ response of
                [] -> Nothing
                ((_,x):_) -> readMay $ unpack x

    followRedirects :: Int -> 
                       Request ->
                       [Maybe CanonicalUrl] ->
                       (ResourceT IO) (Either String DownloadSource, [Maybe CanonicalUrl])
    followRedirects 0   _ redirs = return (Left "Too many redirects", redirs)
    followRedirects n req redirs = do

        res <- http req man

        case statusCode . responseStatus $ res of
            302 -> do
                let resHeaders = responseHeaders res
                    resCookieJar = responseCookieJar res
                case getRedirectedRequest req resHeaders resCookieJar 302 of
                    Just redirReq -> followRedirects (n-1) redirReq (canonicaliseRequest redirReq:redirs)
                    Nothing -> return (Left "Could not create redirect request", redirs)
            _   -> return (Right res, redirs)
