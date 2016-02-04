{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Settings where

import Urls
import Data.ByteString.Char8    (unpack)
import Types
import Network.HTTP.Conduit     (Request, Cookie, addProxy, applyBasicAuth)
import Network.URI              (parseAbsoluteURI, parseRelativeReference)

numStartCrawlers :: Int
numStartCrawlers = 20

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

basicAuthSettings :: Request -> Request
basicAuthSettings = id -- applyBasicAuth "basicUser" "basicPass"

maxRedirects :: Int
maxRedirects = 20

maxContentLength :: Int
maxContentLength = 2 * 1024 * 1024

shareCookie :: Cookie -> Bool
shareCookie = const True

selectFormOptions :: [Form] -> Maybe DownloadRequest
selectFormOptions [] = Nothing
selectFormOptions ((Form (CanonicalUrl urlFormLocation) (Action method (RelativeUrl relUrl)) inputs) : fs) = do

    let target =
            case (parseAbsoluteURI $ unpack urlFormLocation, parseRelativeReference $ unpack relUrl) of
                (Just ou, Just u) -> ou `urlPlus` u
                _ -> error "Could not derelativise url in selectFormOptions"

        params =
            case (urlFormLocation, relUrl) of
                ("http://fullUrlOf/Form1", "relativeFormTarget") -> Just [error "Specify form params"]
                ("http://fullUrlOf/Form2", "relativeFormTarget2") -> Just [error "Specify form params"]
                _ -> Nothing

    case params of
        Just ps -> Just (FormRequest method target ps)
        Nothing -> selectFormOptions fs