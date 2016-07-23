{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Settings where

import Shared (both)
import Prelude hiding (lookup)

import Control.Concurrent.STM        (writeTVar, atomically)
import qualified Data.ByteString.Char8   as C8
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Safe
import Types
import Network.HTTP.Conduit         (Request, Cookie, applyBasicAuth)
import Network.HTTP.Types.Header    (RequestHeaders, hUserAgent)
import Network.URI                  (unEscapeString)
import Text.Read                    (readMaybe)

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

stepMode :: Bool
stepMode = False

customHeaders :: RequestHeaders
customHeaders = [(hUserAgent, "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:44.0) Gecko/20100101 Crawler/0.1")]

initialiseProxy :: CrawlerSettings -> OptionMap -> IO ()
initialiseProxy crawlerSettings (OptionMap optionMap) = 

    case parseProxy of
        Just proxySettings -> atomically $ writeTVar (getProxySettings crawlerSettings) (Just proxySettings)
        Nothing -> return ()

    where
    parseProxy :: Maybe ProxySettings
    parseProxy = do
        proxy <- headMay =<< M.lookup (OptionFlag "-p") optionMap
        case splitOn ":" proxy of
            [address,strPort] -> do
                port <- readMaybe strPort
                return $ ProxySettings (C8.pack address) port
            _ -> Nothing

initialiseFormInstructions :: CrawlerSettings -> OptionMap -> IO ()
initialiseFormInstructions crawlerSettings (OptionMap optionMap) =

    case M.lookup (OptionFlag "-ff") optionMap of
        Nothing -> return ()
        Just formFiles -> do
            formFileContents <- mapM readFile formFiles
            let processed = map processFormInstructions formFileContents
                formInstructions = SuppliedFormActions $ M.unions processed
            atomically $ writeTVar (getFormInstructions crawlerSettings) formInstructions
            putStrLn $ "Inserted Form instructions: \n" ++ show formInstructions 

    where
    processFormInstructions formFile = do
        let ls = filter (not . null) . splitOn [""] . lines $ formFile
            instructions = mapMaybe chunkToInstruction ls
        M.fromList instructions

chunkToInstruction :: [String] -> Maybe (Label, (UrlRegex, FormActionRegex, FormParameters))
chunkToInstruction chunk = do

    let keysAndVals = map (splitOn "=") chunk

        tuples = map (\[a,b] -> (a,b))
               . filter (\x -> length x == 2) $ keysAndVals

        (required, paramStrings) =
            partition (\x -> fst x `elem` ["Label", "UrlRegex", "FormActionRegex"]) tuples

    label <- getVal "Label" required
    urlRegex <- getVal "UrlRegex" required
    formActionRegex <- getVal "FormActionRegex" required
    let params = map (both (C8.pack . unEscapeString)) paramStrings

    return (Label label, (UrlRegex urlRegex, FormActionRegex formActionRegex, FormParameters params))

    where
    getVal :: String -> [(String, String)] -> Maybe C8.ByteString
    getVal key = headMay . map (C8.pack . snd) . filter (\x -> fst x == key)
