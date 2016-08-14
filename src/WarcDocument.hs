{-# LANGUAGE OverloadedStrings #-}

module WarcDocument where

import qualified Data.ByteString.Char8 as C8

import Types
import Data.Warc.WarcEntry as WE
import Data.Warc.Body.Body
import Data.Warc.Shared
import Data.Warc.Header.Header
import Data.Warc.Header.HeaderLine
import Data.Warc.Header.Key
import Data.Warc.Header.Value

instance FromCrawledDocument WarcEntry where

    fromCrawledDocument (CrawledDocument redirectChain content threadId) = 

        let version = WarcVersion "1.0"
            len = IntValue (C8.length content)
            compressionMode = HeaderLine (CustomKey CompressionMode) (CompressionModeValue Uncompressed)
            contentLength = HeaderLine (MandatoryKey ContentLength) len
            originalContentLength = HeaderLine (CustomKey (UnknownKey "Original-Content-Length")) len
            uncompressedContentLength = HeaderLine (CustomKey UncompressedContentLength) len
            (CanonicalUrl urla) = getAttemptedUrl redirectChain
            (CanonicalUrl url) = getCurrentUrl redirectChain
            warcRecordId = HeaderLine (MandatoryKey WarcRecordId) (StringValue url)
            warcTargetURI = HeaderLine (OptionalKey WarcTargetURI) (StringValue url)
            warcThreadId = HeaderLine (CustomKey (UnknownKey "Thread-Id")) (StringValue (C8.pack $ show threadId))
            attemptedUrl = HeaderLine (CustomKey (UnknownKey "Attempted-Url")) (StringValue urla)
            warcType = HeaderLine (CustomKey (UnknownKey "WARC-Type")) (StringValue "response")

            header = WarcHeader version [
                        compressionMode,
                        contentLength,
                        originalContentLength,
                        uncompressedContentLength,
                        warcThreadId,
                        attemptedUrl,
                        warcRecordId,
                        warcTargetURI,
                        warcType]

            body = UncompressedBody content
        in
            WarcEntry header body

    toStorableDocument = WE.toByteString
