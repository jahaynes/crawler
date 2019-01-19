{-# LANGUAGE OverloadedStrings #-}

module Settings where

import           Network.HTTP.Conduit      (Cookie, Request {-, applyBasicAuth-} )
import           Network.HTTP.Types.Header (RequestHeaders, hUserAgent)

numStartCrawlers :: Int
numStartCrawlers = 20

basicAuthSettings :: Request -> Request
basicAuthSettings = id -- applyBasicAuth "basicUser" "basicPass"

ignoreBadHttpsCerts :: Bool
ignoreBadHttpsCerts = True

maxRedirects :: Int
maxRedirects = 20

maxContentLength :: Int
maxContentLength = 2 * 1024 * 1024

--Ignore after #
discardFragments :: Bool
discardFragments = True

shareCookie :: Cookie -> Bool
shareCookie = const True

customHeaders :: RequestHeaders
customHeaders = [(hUserAgent, "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:44.0) Gecko/20100101 Crawler/0.1")]

