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
import Network.HTTP.Conduit     (Request, Cookie, addProxy, applyBasicAuth)
import Network.URI              (unEscapeString)

numStartCrawlers :: Int
numStartCrawlers = 20

proxySettings :: Request -> Request
proxySettings = id -- addProxy "127.0.0.1" 8085

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

--Example format for form_instructions.cfg
{-x = unlines [ "Label=login"
            , "UrlRegex=http://127.0.0.1:3000/login/"
            , "FormActionRegex=http://127.0.0.1:3000/submitLogin"
            , "username=admin"
            , "password=admin"
            , ""
            , "Label=confirm"
            , "UrlRegex=http://127.0.0.1:3000/confirm/"
            , "FormActionRegex=http://127.0.0.1:3000/submitConfirm"
            , "accept=true"
            , ""
            ]-}

initialiseFormInstructions :: CrawlerState -> OptionMap -> IO ()
initialiseFormInstructions crawlerState (OptionMap optionMap) = do
    case M.lookup (OptionFlag "-ff") optionMap of
        Nothing -> return ()
        Just formFiles -> do
            processed <- mapM loadFormInstructionMap formFiles
            let formInstructions = SuppliedFormActions $ M.unions processed
            atomically $ writeTVar (getFormInstructions crawlerState) formInstructions
            putStrLn $ "Inserted Form instructions: \n" ++ show formInstructions 

    where
    loadFormInstructionMap fp = do
        f <- readFile fp
        let ls = filter (not . null) . splitOn [""] . lines $ f
            instructions = mapMaybe chunkToInstruction ls
        return $ M.fromList instructions

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
