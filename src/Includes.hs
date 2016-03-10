{-# LANGUAGE OverloadedStrings #-}

module Includes where

import Types

import Control.Applicative                      ((<$>))
import Control.Monad.STM                        (STM, atomically)
import qualified Data.ByteString.Char8  as C8
import qualified STMContainers.Set      as S
import qualified Data.Map               as M

checkAgainstIncludePatterns :: CrawlerState -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`C8.isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)

initialiseIncludes :: CrawlerState -> OptionMap -> IO ()
initialiseIncludes crawlerState (OptionMap optionMap) =

    case M.lookup (OptionFlag "-i") optionMap of
        Nothing -> error "Fatal, no include patterns included!"
        Just includePatterns ->
            mapM_ (\i -> do
                let pattern = C8.pack i
                atomically . S.insert pattern . getUrlPatterns $ crawlerState
                C8.putStrLn $ C8.concat ["Added pattern: ", pattern]) includePatterns
