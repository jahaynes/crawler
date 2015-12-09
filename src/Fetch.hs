{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Settings
import Types

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import Control.Monad                (when)
import Control.Exception.Lifted     (try)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString.Char8        (ByteString, unpack)
import Data.ByteString.Lazy.Char8   (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 (ResumableSource, ($$+-), ($=))
import Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import Data.List                    (group)
import Data.Maybe                   (catMaybes)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status    (statusCode)
import Network.HTTP.Types.Header
import Safe                         (readMay)
import Network.HTTP.Types           (methodPost)

requestWithoutRedirects :: [Cookie] -> CanonicalUrl -> IO Request 
requestWithoutRedirects requestCookies url = do
    req <- basicAuthSettings . proxySettings <$> parseUrl (show url)
    return $ req {
                  redirectCount = 0,
                  cookieJar = Just (createCookieJar requestCookies)
                 }

makeFormRequest :: [Cookie] -> FormRequest -> IO Request
makeFormRequest requestCookies (FormRequest formMethod (CanonicalUrl target) params) = do

    let addParams = if mk formMethod == mk methodPost
                        then urlEncodedBody params
                        else setQueryString (map (\(k,v) -> (k,Just v)) params) 

    req <- basicAuthSettings . proxySettings <$> parseUrl (unpack target)

    return $ addParams req {
                            redirectCount = 0,
                            cookieJar = Just (createCookieJar requestCookies)
                           }

data DownloadAmount = TooBig
                    | SmallEnough
                    | Unknown

getWithRedirects :: Manager
                 -> [Cookie]
                 -> Either FormRequest CanonicalUrl
                 -> IO (Either String (ByteString, [Cookie]), [CanonicalUrl])
getWithRedirects man requestCookies formOrUrl = do

    req <- case formOrUrl of
               Left formOptions -> makeFormRequest requestCookies formOptions
               Right url -> requestWithoutRedirects requestCookies url

    (mLbs, mRedirects) <-
        runResourceT $ do
            (mResponse, mRedirects) <- followRedirects maxRedirects req [canonicaliseRequest req]
            case mResponse of
                Left l -> return (Left l, mRedirects)
                Right response -> do

                    let responseCookies = destroyCookieJar . responseCookieJar $ response

                    let downloadAmount = 
                            case getContentLength response of
                                Nothing -> Unknown
                                Just x | x <= maxContentLength -> SmallEnough
                                       | otherwise -> TooBig

                    case downloadAmount of

                        TooBig -> return (Left "Too big", mRedirects)

                        SmallEnough -> do
                            bs <- responseBody response $$+- (toStrict <$> sinkLbs)
                            case BS.last bs of _ -> return (Right (bs, responseCookies), mRedirects)

                        Unknown -> do
                            bs <- responseBody response $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)
                            case BS.last bs of _ -> return (Right (bs, responseCookies), mRedirects)

    -- Include the starting URL as a "redirect", in case a "/" was put on the end
    let startUrl = case formOrUrl of
                       Left (FormRequest _ target _) -> [target]
                       Right url -> [url]
    let redirects = catMaybes mRedirects ++ startUrl

    when (length redirects < length mRedirects + 1) (putStrLn "Warning, not all redirects were parsed!")

    return (mLbs, dedupe redirects)

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    getContentLength :: Response a -> Maybe Int
    getContentLength response =
        case filter (\(k,_) -> k == hContentLength) . responseHeaders $ response of
            [] -> Nothing
            ((_,x):_) -> readMay $ unpack x

    followRedirects :: Int -> 
                       Request ->
                       [Maybe CanonicalUrl] ->
                       (ResourceT IO) (Either String (Response (ResumableSource (ResourceT IO) ByteString)), [Maybe CanonicalUrl])
    followRedirects 0     _ redirs = return (Left "Too many redirects", redirs)
    followRedirects n req redirs = do
        eitherResponse <- try (http req man)
        case eitherResponse of
            Right r -> return (Right r, redirs)
            Left (StatusCodeException status rheaders cj) -> do
                let code = statusCode status
                    mRedirReq = getRedirectedRequest req rheaders cj code
                case mRedirReq of
                    Just redirReq -> followRedirects (n-1) redirReq (canonicaliseRequest redirReq:redirs)
                    Nothing -> return (Left ("Code: " ++ show code), redirs)
            Left e -> return (Left ("followRedirects: " ++ show e), redirs)
