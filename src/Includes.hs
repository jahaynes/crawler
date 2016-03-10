module Includes where

import Types                        (CrawlerState, CanonicalUrl (..), getUrlPatterns, setAsList)

import Control.Applicative          ((<$>))
import Control.Monad.STM            (STM)
import Data.ByteString              (isInfixOf)

checkAgainstIncludePatterns :: CrawlerState -> CanonicalUrl -> STM Bool
checkAgainstIncludePatterns crawlerState (CanonicalUrl url) =
    any (`isInfixOf` url) <$> setAsList (getUrlPatterns crawlerState)