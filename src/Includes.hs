{-# LANGUAGE OverloadedStrings #-}

module Includes where

import CountedQueue                             (writeQueue)
import Crawl
import Shared
import Types
import Urls

import Control.Applicative                      ((<$>))
import Control.Monad.STM                        (atomically)
import qualified Data.ByteString.Char8  as C8
import qualified STMContainers.Set      as S
import qualified Data.Map               as M
import Data.Maybe

initialiseIncludes :: Crawler -> OptionMap -> IO ()
initialiseIncludes crawler (OptionMap optionMap) = do

    case M.lookup (OptionFlag "-i") optionMap of
        Nothing -> return ()
        Just includePatterns -> insertIncludes (map C8.pack includePatterns)

    case M.lookup (OptionFlag "-if") optionMap of
        Nothing -> return ()
        Just includeFiles -> do
            files <- mapM C8.readFile includeFiles
            insertIncludes . map trim . filter (not . C8.null) . concatMap C8.lines $ files

    case M.lookup (OptionFlag "-u") optionMap of
        Nothing -> return ()
        Just startUrls -> insertStartUrls (mapMaybe canonicaliseString startUrls)

    case M.lookup (OptionFlag "-uf") optionMap of
        Nothing -> return ()
        Just urlFiles -> do
            files <- mapM C8.readFile urlFiles
            insertStartUrls . mapMaybe canonicaliseByteString
                            . map trim
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


    insertIncludes =
        mapM_ (\i -> do
            atomically . S.insert i . getUrlPatterns $ crawler
            C8.putStrLn $ C8.concat ["Added pattern: ", i])