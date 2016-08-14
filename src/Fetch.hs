{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Request
import Settings
import Types

import Data.CaseInsensitive         (mk)
import Data.ByteString.Char8        (unpack)
import Data.ByteString.Lazy.Char8   (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 (($$+-), ($=))
import Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import Network.HTTP.Conduit
import Safe                         (readMay)
import Network.HTTP.Types           

{- Big TODO - make a Debug type to represent Left,
   containing cookies, canonicalUrls, error messages, etc. 

   TODO - do it before merging and recapture all those Lefts we dropped -}

fetch :: Manager -> CrawlerSettings -> [Cookie] -> DownloadRequest -> WebIO DownloadResult
fetch man crawlerSettings requestCookies downloadRequest = do

    initialRequest <- buildRequest crawlerSettings requestCookies downloadRequest

    (response, redirects) <- followRedirects maxRedirects man (startRedirectChain downloadRequest) initialRequest

    let responseCookies = destroyCookieJar . responseCookieJar $ response

    checkSize response

    content <- downloadBodySource response

    return $ DownloadResult content responseCookies redirects

    where
    checkSize :: DownloadSource -> WebIO ()
    checkSize res = case getContentLength of
                    Just x | x <= maxContentLength -> return ()
                           | otherwise -> webErr "Too big"
                    Nothing -> return ()

        where
        getContentLength :: Maybe Int
        getContentLength =
            case filter (\(k,_) -> mk k == mk hContentLength) . responseHeaders $ res of
                [] -> Nothing
                ((_,x):_) -> readMay $ unpack x

    downloadBodySource :: DownloadSource -> WebIO BS.ByteString
    downloadBodySource res = responseBody res $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)

followRedirects :: Int -> Manager -> RedirectChain -> Request -> WebIO (DownloadSource, RedirectChain)
followRedirects 0   _             _   _ = webErr "Too many redirects.  Aborting."
followRedirects n man redirectChain req = http req man >>= \response ->
    case statusCode . responseStatus $ response of
        302 -> do
            nextRedirect <- getNextRedirect req response
            nextUrl <- canonicaliseRequest nextRedirect
            followRedirects (n-1) man (appendRedirect redirectChain nextUrl) nextRedirect
        _   -> return (response, redirectChain)

getNextRedirect :: Request -> Response a -> WebIO Request
getNextRedirect req res =
    case getRedirectedRequest req (responseHeaders res) (responseCookieJar res) 302 of
        Just redirReq -> return redirReq
        Nothing -> webErr "Could not create redirect request"
