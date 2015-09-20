{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Urls (startsWith)
import Types

import Network.HTTP.Conduit (Request, addProxy)

numStartCrawlers :: Int
numStartCrawlers = 20

numStartParsers :: Int
numStartParsers = 2

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

maxRedirects :: Int
maxRedirects = 10

isAcceptable :: CanonicalUrl -> Bool
isAcceptable url = True