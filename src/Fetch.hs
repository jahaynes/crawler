{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Request
import Settings
import Types

import Control.Monad.Trans.Resource (MonadResource)
import Data.CaseInsensitive         (mk)
import Data.ByteString.Char8        (unpack)
import Data.ByteString.Lazy.Char8   (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 
import Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import Data.List                    (group)
import Network.HTTP.Conduit
import Safe                         (readMay)
import Network.HTTP.Types           

{- Big TODO - make a Debug type to represent Left,
   containing cookies, canonicalUrls, error messages, etc. 

   TODO - do it before merging and recapture all those Lefts we dropped -}

fetch :: MonadResource m => Manager -> CrawlerSettings -> [Cookie] -> DownloadRequest -> m DownloadResult
fetch man crawlerSettings requestCookies downloadRequest = do

    (response, redirects) <- followRedirects

    let responseCookies = destroyCookieJar . responseCookieJar $ response

    checkSize response

    content <- do
        (responseBody response) $$ CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)

    --TODO - see if this dedupe / append is necessary
    return $ DownloadResult content responseCookies (dedupe (redirects ++ [getUrl downloadRequest]))

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    -- checkSize :: DownloadSource -> WebIO ()
    checkSize res = case getContentLength of
                    Just x | x <= maxContentLength -> return ()
                           | otherwise -> error "TODO - Too big"
                    Nothing -> return ()

        where
        getContentLength :: Maybe Int
        getContentLength =
            case filter (\(k,_) -> mk k == mk hContentLength) . responseHeaders $ res of
                [] -> Nothing
                ((_,x):_) -> readMay $ unpack x

    --downloadBodySource :: DownloadSource -> WebIO BS.ByteString
    -- downloadBodySource res = responseBody res $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)

    -- followRedirects :: WebIO (DownloadSource, [CanonicalUrl])
    followRedirects = do
        initialRequest <- buildRequest crawlerSettings requestCookies downloadRequest
        firstUrl <- canonicaliseRequest initialRequest
        go [firstUrl] maxRedirects initialRequest

        where
        --go :: [CanonicalUrl] -> Int -> Request -> WebIO (DownloadSource, [CanonicalUrl])
        go      _ 0   _ = error "TODO - Too many redirects.  Aborting."
        go redirs n req = do
            res <- http req man
            let code = statusCode . responseStatus $ res
            if isRedirectCode code
                then followRedirect res code
                else return (res, redirs)

            where
            isRedirectCode code = code >= 300 && code < 400

            followRedirect res code = do
                let resHeaders = responseHeaders res
                    resCookieJar = responseCookieJar res
                case getRedirectedRequest req resHeaders resCookieJar code of --extract this into a webio
                    Just redirReq -> do
                        nextUrl <- canonicaliseRequest redirReq
                        go (nextUrl:redirs) (n-1) redirReq
                    Nothing -> error "TODO - Could not create redirect request"
