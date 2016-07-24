{-# LANGUAGE OverloadedStrings #-}

module Initialisation where

import           CountedQueue           (writeQueue)
import           Crawl
import           Shared
import           Types
import           Urls

import           Control.Concurrent.STM (atomically, writeTVar)
import qualified Data.ByteString.Char8  as C8
import           Data.List              (groupBy, partition, union)
import           Data.List.Split        (splitOn)
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe)
import           Network.URI            (unEscapeString)
import           Safe
import qualified STMContainers.Set      as S
import           Text.Read              (readMaybe)

newtype OptionFlag = OptionFlag String deriving (Show, Eq, Ord)

newtype OptionMap = OptionMap (M.Map OptionFlag [String]) deriving Show

optionMapFromArgs :: [String] -> OptionMap
optionMapFromArgs =   OptionMap
                  <$> M.unionsWith union
                  .   mapMaybe (\gr -> M.singleton <$> (OptionFlag <$> headMay gr) <*> tailMay gr)
                  .   filter (maybe False isFlag . headMay)
                  .   groupBy (\a b -> isFlag a && not (isFlag b))

    where
    isFlag x = length x > 1 && head x == '-'

initialiseSettings :: Crawler -> [String] -> IO ()
initialiseSettings crawler args = do

    let optionMap = optionMapFromArgs args

    initialiseFormInstructions (getCrawlerSettings crawler) optionMap

    initialiseProxy (getCrawlerSettings crawler) optionMap

    initialiseIncludes optionMap

    initialiseStartUrls optionMap

    where
    initialiseIncludes :: OptionMap -> IO ()
    initialiseIncludes (OptionMap optionMap) = do

        case M.lookup (OptionFlag "-i") optionMap of
            Nothing -> return ()
            Just includePatterns -> insertIncludes (map C8.pack includePatterns)

        case M.lookup (OptionFlag "-if") optionMap of
            Nothing -> return ()
            Just includeFiles -> do
                files <- mapM C8.readFile includeFiles
                insertIncludes . map trim . filter (not . C8.null) . concatMap C8.lines $ files

        where
        insertIncludes =
            mapM_ (\i -> do
                atomically . S.insert i . getUrlPatterns $ crawler
                C8.putStrLn $ C8.concat ["Added pattern: ", i])

    initialiseStartUrls :: OptionMap -> IO ()
    initialiseStartUrls (OptionMap optionMap) = do

        case M.lookup (OptionFlag "-u") optionMap of
            Nothing -> return ()
            Just startUrls -> insertStartUrls (mapMaybe canonicaliseString startUrls)

        case M.lookup (OptionFlag "-uf") optionMap of
            Nothing -> return ()
            Just urlFiles -> do
                files <- mapM C8.readFile urlFiles
                insertStartUrls . mapMaybe (canonicaliseByteString . trim)
                                . filter (not . C8.null)
                                . concatMap C8.lines
                                $ files

        where
        insertStartUrls =
            mapM_ (\cu@(CanonicalUrl u)-> do
                result <- processNextUrl crawler cu
                case result of
                    Success -> C8.putStrLn $ C8.concat ["Added Url: ", u]
                    Failure reason -> atomically $ writeQueue (getLogQueue crawler) (LoggableError cu (C8.concat ["Could not add Url: ", u, "\n", "Reason: ", reason])))

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
        processFormInstructions :: String -> M.Map Label (UrlRegex, FormActionRegex, FormParameters)
        processFormInstructions formFile = do
            let ls = filter (not . null) . splitOn [""] . lines $ formFile
                instructions = mapMaybe chunkToInstruction ls
            M.fromList instructions

            where
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

