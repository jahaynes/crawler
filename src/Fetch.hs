{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Settings
import Types

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import Control.Monad                (when)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Either   (EitherT, runEitherT, left, right)
import Control.Monad.IO.Class       (liftIO)
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

{- Big TODO - make a Debug type to represent Left,
   containing cookies, canonicalUrls, error messages, etc. 

   TODO - do it before merging and recapture all those Lefts we dropped -}

getWithRedirects :: Manager
                 -> [Cookie]
                 -> DownloadRequest
                 -> IO (Either String (BS.ByteString, [Cookie], [CanonicalUrl]))
getWithRedirects man requestCookies downloadRequest = do

    case makeRequest requestCookies downloadRequest of
        Left ex -> return (Left $ "Could not make request from " ++ show downloadRequest ++ "\nThe reason was: " ++ show ex)

        Right req -> do

            mResponseAndRedirects <- runEitherT $ followRedirects maxRedirects req [canonicaliseRequest req]

            case mResponseAndRedirects of
                Left l -> undefined

                Right (response, mRedirects) -> do

                    eContent <- runEitherT $ downloadEnoughContent response
                    let responseCookies = destroyCookieJar . responseCookieJar $ response

                    case eContent of
                        Left l -> undefined
                        Right content -> do

                            --Include the initial url in the redirects
                            let redirects = catMaybes mRedirects ++ [getUrl downloadRequest]

                            when (length redirects < length mRedirects + 1) (putStrLn "Warning, not all redirects were parsed!")

                            return (Right (content, responseCookies, dedupe redirects))

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    downloadEnoughContent :: DownloadSource -> EitherT String IO BS.ByteString
    downloadEnoughContent response =

        case getContentLength of
            Just x | x <= maxContentLength -> downloadBodySource response
                   | otherwise -> left "Too big"
            Nothing -> downloadBodySource response

        where
        downloadBodySource :: DownloadSource -> EitherT String IO BS.ByteString
        downloadBodySource res = right =<< (liftIO . runResourceT $ responseBody res $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs))

        getContentLength :: Maybe Int
        getContentLength =
            case filter (\(k,_) -> mk k == mk hContentLength) . responseHeaders $ response of
                [] -> Nothing
                ((_,x):_) -> readMay $ unpack x

    followRedirects :: Int -> 
                       Request ->
                       [Maybe CanonicalUrl] ->
                       EitherT String IO (DownloadSource, [Maybe CanonicalUrl])
    followRedirects 0   _ redirs = left "Too many redirects"
    followRedirects n req redirs = do

        res <- liftIO . runResourceT $ http req man

        case statusCode . responseStatus $ res of
            302 -> do
                let resHeaders = responseHeaders res
                    resCookieJar = responseCookieJar res
                case getRedirectedRequest req resHeaders resCookieJar 302 of
                    Just redirReq -> followRedirects (n-1) redirReq (canonicaliseRequest redirReq:redirs)
                    Nothing -> left "Could not create redirect request"
            _   -> right (res, redirs)
