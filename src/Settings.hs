{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Types
import Network.HTTP.Conduit (Request, Cookie, addProxy)

numStartCrawlers :: Int
numStartCrawlers = 20

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

maxRedirects :: Int
maxRedirects = 20

maxContentLength :: Int
maxContentLength = 2 * 1024 * 1024

formIdenfifier :: CanonicalUrl -> Form -> Bool
formIdenfifier formatUrl form = False

shareCookie :: Cookie -> Bool
shareCookie cookie = True