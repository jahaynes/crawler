{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Urls
import Request
import Settings
import Types

import Data.CaseInsensitive         (mk)
import Control.Applicative          ((<$>))
import Data.ByteString.Char8        (unpack)
import Data.ByteString.Lazy.Char8   (toStrict)
import qualified Data.ByteString    as BS
import Data.Conduit                 (($$+-), ($=))
import Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List  as CL (map)
import Data.List                    (group)
import Network.HTTP.Conduit
import Safe                         (readMay)
import Network.HTTP.Types           

{- Big TODO - make a Debug type to represent Left,
   containing cookies, canonicalUrls, error messages, etc. 

   TODO - do it before merging and recapture all those Lefts we dropped -}

fetch :: Manager -> [Cookie] -> DownloadRequest -> WebIO DownloadResult
fetch man requestCookies downloadRequest = do

    (response, redirects) <- followRedirects

    let responseCookies = destroyCookieJar . responseCookieJar $ response

    content <- downloadEnoughContent response

    --TODO - see if this dedupe / append is necessary
    return $ DownloadResult content responseCookies (dedupe (redirects ++ [getUrl downloadRequest]))

    where
    dedupe :: [CanonicalUrl] -> [CanonicalUrl]
    dedupe = map head . group

    downloadEnoughContent :: DownloadSource -> WebIO BS.ByteString
    downloadEnoughContent response =

        case getContentLength of
            Just x | x <= maxContentLength -> downloadBodySource
                   | otherwise -> webErr "Too big"
            Nothing -> downloadBodySource

        where
        downloadBodySource :: WebIO BS.ByteString
        downloadBodySource = responseBody response $$+- CL.map (BS.take maxContentLength) $= (toStrict <$> sinkLbs)

        getContentLength :: Maybe Int
        getContentLength =
            case filter (\(k,_) -> mk k == mk hContentLength) . responseHeaders $ response of
                [] -> Nothing
                ((_,x):_) -> readMay $ unpack x

    followRedirects :: WebIO (DownloadSource, [CanonicalUrl])
    followRedirects = do
        initialRequest <- buildRequest requestCookies downloadRequest
        firstUrl <- canonicaliseRequest initialRequest
        go [firstUrl] maxRedirects initialRequest

        where
        go :: [CanonicalUrl] -> Int -> Request -> WebIO (DownloadSource, [CanonicalUrl])
        go      _ 0   _ = webErr "Too many redirects.  Aborting."
        go redirs n req = do
            res <- http req man
            case statusCode . responseStatus $ res of
                302 -> do
                    let resHeaders = responseHeaders res
                        resCookieJar = responseCookieJar res
                    case getRedirectedRequest req resHeaders resCookieJar 302 of --extract this into a webio
                        Just redirReq -> do
                            nextUrl <- canonicaliseRequest redirReq
                            go (nextUrl:redirs) (n-1) redirReq
                        Nothing -> webErr "Could not create redirect request"
                _   -> return (res, redirs)
