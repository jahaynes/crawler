{-# LANGUAGE OverloadedStrings #-}

module Includes where

import Types

import Control.Applicative          ((<$>))
import Control.Monad.STM            (STM)
import qualified Data.ByteString.Char8 as C8

import qualified Data.Map   as M

checkAgainstIncludePatterns :: CrawlerState -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`C8.isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)

initialiseIncludes :: CrawlerState -> OptionMap -> IO ()
initialiseIncludes crawlerState (OptionMap optionMap) = do

    case M.lookup (OptionFlag "-i") optionMap of
        Nothing -> C8.putStrLn "Warning, no include patterns included"
        Just includePatterns -> do
            print includePatterns
