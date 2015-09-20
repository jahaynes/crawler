module Crawl where

import Fetch
import Urls
import Types

import Control.Applicative ((<$>))
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM           (atomically)
import Control.Monad                    (forever, when, unless)

import qualified Data.ByteString.Char8 as C8
import Data.Maybe                       (isJust)
import GHC.Conc                         (STM)
import Network.HTTP.Conduit             (newManager, tlsManagerSettings)

import qualified STMContainers.Set as S
import qualified STMContainers.Map as M

crawlNextUrl :: CrawlerState -> IO ()    
crawlNextUrl crawlerState = newManager tlsManagerSettings >>= \man -> 
    forever $ do

        nextUrl <- atomically $ readTQueue (getUrlQueue crawlerState)

        (mBodyData, redirects) <- getWithRedirects man nextUrl

        case mBodyData of
            Nothing -> atomically $ failedDownload nextUrl
            Just bodyData -> atomically $ successfulDownload nextUrl redirects bodyData
            
    where
    failedDownload :: CanonicalUrl -> STM ()
    failedDownload attemptedUrl = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        M.insert "Couldn't get data!" attemptedUrl (getUrlsFailed crawlerState)

    successfulDownload :: CanonicalUrl -> [CanonicalUrl] -> C8.ByteString -> STM ()
    successfulDownload attemptedUrl redirects bodyData = do
        S.delete attemptedUrl (getUrlsInProgress crawlerState)
        mapM_ (\u -> S.insert u (getUrlsCompleted crawlerState)) redirects
        writeTQueue (getParseQueue crawlerState) (redirects, bodyData)
        writeTBQueue (getStoreQueue crawlerState) (redirects, bodyData)

processNextUrl :: CrawlerState -> CanonicalUrl -> IO ()
processNextUrl crawlerState url =
    when (isAcceptable url) $
        atomically $ do
            completed <- S.lookup url (getUrlsCompleted crawlerState)
            inProgress <- S.lookup url (getUrlsInProgress crawlerState)
            failed <- isJust <$> M.lookup url (getUrlsFailed crawlerState)
            unless (completed || inProgress || failed) $ do
                S.insert url (getUrlsInProgress crawlerState)
                writeTQueue (getUrlQueue crawlerState) url