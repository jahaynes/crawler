{-# LANGUAGE OverloadedStrings #-}

module Urls where

import Settings
import Types

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8 
import           Data.ByteString.Search      (breakOn, breakAfter)
import           Data.List.Split             (splitWhen)
import           Network.URI
import           Network.HTTP.Client         (Request, getUri)

canonicaliseRequest :: Monad m => Request -> m CanonicalUrl
canonicaliseRequest req =
    case canonicaliseNetworkUri . getUri $ req of
        Nothing -> error "TODO handle better - Couldn't canonicalise URL from Request"
        Just url -> return url

canonicaliseNetworkUri :: URI -> Maybe CanonicalUrl
canonicaliseNetworkUri = canonicaliseString . show . stripPort

    where
    stripPort :: URI -> URI
    stripPort uri = uri { uriAuthority = stripPort' (uriScheme uri) <$> uriAuthority uri }
        where
        stripPort' :: String -> URIAuth -> URIAuth
        stripPort' "http:" auth = auth {uriPort = if uriPort auth == ":80" then "" else uriPort auth}
        stripPort' "https:" auth = auth {uriPort = if uriPort auth == ":443" then "" else uriPort auth}
        stripPort' _ auth = auth

canonicaliseByteString :: ByteString -> Maybe CanonicalUrl
canonicaliseByteString = canonicaliseString . C8.unpack

canonicaliseString :: String -> Maybe CanonicalUrl
canonicaliseString str =
    case parseAbsoluteURI (discard str) of
        Just x -> Just (CanonicalUrl (C8.pack . normalize . show $ (x :: URI)))
        Nothing -> Nothing  --TODO - this looks double-handled

normalize :: String -> String
normalize = normalizeCase . normalizeEscape . normalizePathSegments

urlPlus :: URI -> URI -> CanonicalUrl
urlPlus a b = CanonicalUrl (C8.pack . discard . normalize . show . relativeTo b $ a)

discard :: String -> String
discard str | discardFragments = takeWhile (/= '#') str
            | otherwise = str

contains :: CanonicalUrl -> ByteString -> Bool
contains (CanonicalUrl u) bs = bs `C8.isInfixOf` u

startsWith :: CanonicalUrl -> ByteString -> Bool
startsWith (CanonicalUrl u) bs = bs `C8.isPrefixOf` u

parseRelative :: String -> Maybe URI
parseRelative relative =
    case stripQueryParams relative of
         (rel, Nothing) -> parseRelativeReference rel
         (rel, Just query_) -> case parseRelativeReference rel of
                                  Just x -> Just (x {uriQuery = query_})
                                  Nothing -> Nothing
    where
    stripQueryParams :: String -> (String, Maybe String)
    stripQueryParams url
        | '?' `Prelude.elem` url =
            case splitWhen (\a -> a == '?' || a == '#') url of
                [r,q,f] -> (Prelude.concat [r,"#",f], Just ('?':q))
                [r,_] -> (r, Nothing)
                _ -> (url, Nothing)
        | otherwise = (url, Nothing)

derelativise :: CanonicalUrl -> ByteString -> Either Loggable CanonicalUrl
derelativise onUrl rawBsUrl
    | "mailto:" `C8.isPrefixOf` bsUrl = Left $ CrawlWarning onUrl $ C8.append "Found an email " bsUrl
    | isURI url = case canonicaliseByteString bsUrl of
                      Nothing -> 
                          let errMessage = C8.append "Could not parse URL: " bsUrl
                          in Left (CrawlError onUrl errMessage)
                      Just canonicalised -> Right canonicalised
    | otherwise = joinParts (parseAbsoluteURI (show onUrl)) (parseRelative url)

    where
    bsUrl = sanitiseUrl rawBsUrl

    sanitiseUrl = encodeSpaces . dropFragments

    dropFragments unsanitised | '#' `C8.elem` unsanitised = C8.takeWhile (/= '#') unsanitised
                              | otherwise                 = unsanitised

    encodeSpaces unsanitised | ' ' `C8.elem` unsanitised = C8.intercalate "%20" $ C8.split ' ' unsanitised
                             | otherwise                 = unsanitised

    url = C8.unpack bsUrl

    joinParts :: Maybe URI -> Maybe URI -> Either Loggable CanonicalUrl
    joinParts  Nothing         _ = Left $ CrawlError onUrl (C8.concat ["Couldn't derelativise left side: ", (C8.pack . show) onUrl])
    joinParts        _   Nothing = Left $ CrawlError onUrl (C8.concat ["Couldn't derelativise right side: ", bsUrl])
    joinParts (Just ou) (Just u) = Right $ ou `urlPlus` u

getDomain :: CanonicalUrl -> Maybe Domain
getDomain (CanonicalUrl u) =
    let d = fst . breakOn "/" . snd . breakAfter "//" $ u
    in if C8.null d then Nothing else Just (Domain d)
