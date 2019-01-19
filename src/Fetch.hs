{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import DownloadRequest (DownloadRequest, getUrl)
import HttpUtil        (checkSize)
import Urls            (canonicaliseRequest)
import Request         (buildRequest)
import Settings
import Types           (CanonicalUrl, CrawlerSettings)

import Control.Monad.Fail                 (MonadFail)
import Control.Monad.Trans.Resource       (MonadResource)
import Data.ByteString.Lazy.Char8         (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 
import Data.Conduit.Binary                (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import Data.List                          (nub)
import Network.HTTP.Conduit
import Network.HTTP.Types           

data DownloadResult =
    DownloadResult { dr_getContent :: !BS.ByteString
                   , dr_getCookies :: ![Cookie]
                   , dr_getVisited :: ![CanonicalUrl]
                   }

fetch :: (MonadFail m, MonadResource m, DownloadRequest dr, Show dr)
      => Manager -> CrawlerSettings -> [Cookie] -> dr -> m DownloadResult
fetch man crawlerSettings requestCookies downloadRequest = do

    (response, redirects) <- followRedirects

    checkSize maxContentLength response

    content <- responseBody response $$ CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)

    --TODO - see if this dedupe / append is necessary
    pure $ DownloadResult { dr_getContent = content
                          , dr_getCookies = destroyCookieJar . responseCookieJar $ response
                          , dr_getVisited = nub (redirects ++ [getUrl downloadRequest])
                          }

    where
    followRedirects = do
        initialRequest <- buildRequest crawlerSettings requestCookies downloadRequest
        firstUrl <- canonicaliseRequest initialRequest
        go [firstUrl] maxRedirects initialRequest

        where
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
                case getRedirectedRequest req resHeaders resCookieJar code of
                    Just redirReq -> do
                        nextUrl <- canonicaliseRequest redirReq
                        go (nextUrl:redirs) (n-1) redirReq
                    Nothing -> error "TODO - Could not create redirect request"
