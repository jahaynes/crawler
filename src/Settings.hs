{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Network.HTTP.Conduit (Request, addProxy)

threadsPerJob :: [Int]
threadsPerJob = [1..8]

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

maxRedirects :: Int
maxRedirects = 10