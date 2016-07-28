{-# LANGUAGE OverloadedStrings #-}

module Directions where
import Control.Concurrent.STM
import Data.Attoparsec.ByteString.Lazy          (Parser, parse, string, Result(..))
import Data.Attoparsec.ByteString.Char8         (takeTill, char, skipSpace)
import Data.ByteString                          (ByteString)
import Data.Maybe                               (mapMaybe)
import qualified Data.ByteString.Lazy.Char8  as L8
import Safe (headMay)
import Text.Regex.Posix          ((=~))

import Urls
import Types

emptyHrefDirections :: [HrefDirection]
emptyHrefDirections = []

findDirection :: CanonicalUrl -> ByteString -> CrawlerSettings -> IO (Maybe CanonicalUrl)
findDirection cUrl pageData crawlerSettings = do

    hds <- atomically . readTVar . getHrefDirections $ crawlerSettings
    return $ findDirection' cUrl hds

    where
    findDirection' :: CanonicalUrl -> [HrefDirection] -> Maybe CanonicalUrl
    findDirection' (CanonicalUrl url) hrefDirections =

        headMay . mapMaybe matchOnPage . filter urlIsMatching $ hrefDirections

        where
        urlIsMatching :: HrefDirection -> Bool
        urlIsMatching (HrefDirection _ (UrlRegex ur) _) = url =~ ur

        matchOnPage :: HrefDirection -> Maybe CanonicalUrl
        matchOnPage (HrefDirection _ _ (HrefRegex hr)) = do

            candidate <- headMay $ concatMap (drop 1) ((pageData =~ hr) :: [[ByteString]])

            case derelativise cUrl candidate of
                (Right r) -> Just r
                (Left _) -> Nothing

parseHrefDirections :: L8.ByteString -> [HrefDirection]
parseHrefDirections f
    | L8.null f = []
    | otherwise = case parse hrefDirection f of
                      (Done remainder x) -> x : parseHrefDirections remainder
                      _ -> error "Failed href directions parsing"

hrefDirection :: Parser HrefDirection
hrefDirection = skipSpace *> do
    l <- Label <$> parseHrefLine "HrefDirection"
    ur <- UrlRegex <$> parseHrefLine "UrlRegex"
    hr <- HrefRegex <$> parseHrefLine "HrefRegex"
    skipSpace
    return (HrefDirection l ur hr)

    where
    parseHrefLine :: ByteString -> Parser ByteString
    parseHrefLine particular = do
        skipSpace <* string particular <* takeTill (=='=')
        _ <- char '='
        v <- skipSpace *> takeTill (\x -> x == '\r' || x == '\n')
        return v