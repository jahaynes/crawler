{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Network.HTTP.Conduit (Request, addProxy)

numStartCrawlers :: Int
numStartCrawlers = 20

numStartParsers :: Int
numStartParsers = 2

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

maxRedirects :: Int
maxRedirects = 10

maxContentLength :: Int
maxContentLength = 2 * 1024 * 1024