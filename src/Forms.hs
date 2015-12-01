{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Types                    (Form (..), Input (..))
import Data.ByteString          (ByteString)
import Text.HTML.TagSoup        (Attribute, Tag (TagOpen), isTagOpenName, isTagCloseName, sections)
import Text.HTML.TagSoup.Fast   (parseTags)

getForms :: ByteString -> [Form]
getForms = map asForm . isolateForms . parseTags

    where
    asForm :: [Tag ByteString] -> Form
    asForm tags = Form attributes inputs

        where
        attributes :: [Attribute ByteString]
        attributes = (\ (TagOpen _ attrs : _) -> attrs) tags

        inputs :: [Input]
        inputs = map (\(TagOpen _ attrs) -> Input attrs) . filter (isTagOpenName "input") $ tags

    isolateForms :: [Tag ByteString] -> [[Tag ByteString]]
    isolateForms = map (takeWhilePlus1 (not . isTagCloseName "form"))
                . sections (isTagOpenName "form")

        where
        takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
        takeWhilePlus1 _     [] = []
        takeWhilePlus1 p (x:xs) | p x       = x : takeWhilePlus1 p xs
                                | otherwise = x : takeWhilePlus1 p []