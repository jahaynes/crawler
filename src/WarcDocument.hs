{-# LANGUAGE OverloadedStrings #-}

module WarcDocument where

import qualified Data.ByteString.Char8 as C8

import Types
import Data.Warc.WarcEntry as WE
import Data.Warc.Body
import Data.Warc.Shared
import Data.Warc.Header
import Data.Warc.HeaderLine
import Data.Warc.Key
import Data.Warc.Value

newtype CrawledWarcEntry = CrawledWarcEntry WarcEntry

instance FromCrawledDocument CrawledWarcEntry where

    fromCrawledDocument (CrawledDocument redirectChain content threadId) = 

        let ver = WarcVersion "1.0"
            len = IntValue (C8.length content)
            compressionMode = HeaderLine (CustomKey CompressionMode) (CompressionModeValue Uncompressed)
            contentLength = HeaderLine (MandatoryKey ContentLength) len
            originalContentLength = HeaderLine (CustomKey (UnknownKey "Original-Content-Length")) len
            uncompressedContentLength = HeaderLine (CustomKey UncompressedContentLength) len
            (CanonicalUrl url) = last redirectChain
            warcRecordId = HeaderLine (MandatoryKey WarcRecordId) (StringValue url)
            warcTargetURI = HeaderLine (OptionalKey WarcTargetURI) (StringValue url)
            warcType = HeaderLine (CustomKey (UnknownKey "WARC-Type")) (StringValue "response")

            header = WarcHeader ver [
                        compressionMode,
                        contentLength,
                        originalContentLength,
                        uncompressedContentLength,
                        warcRecordId,
                        warcTargetURI,
                        warcType]

            body = UncompressedBody content
        in
            CrawledWarcEntry $ WarcEntry header body

    toStorableDocument (CrawledWarcEntry we) = WE.toByteString we
