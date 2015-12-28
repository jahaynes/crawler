{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Types
import Data.ByteString          (ByteString)
import Data.CaseInsensitive     (mk)
import Text.HTML.TagSoup        hiding (parseTags)
import Text.HTML.TagSoup.Fast   (parseTags)
import Network.HTTP.Types       (Method, methodGet)

--TODO -> some kind of form selector to choose forms

getForms :: CanonicalUrl -> ByteString -> [Form]
getForms onUrl = map asForm . isolateForms . parseTags

    where
    asForm :: [Tag ByteString] -> Form
    asForm tags = Form onUrl (Action method action) inputs

        where
        --TODO derelativise here?
        action :: RelativeUrl
        action = RelativeUrl $ case filter (\(k,_) -> mk k == mk "action") attributes of
                                   ((_,urlStub):_) -> urlStub
                                   [] -> ""

        method :: Method
        method = case filter (\(k,_) -> mk k == mk "method") attributes of
                     ((_,v):_) -> v
                     _ -> methodGet

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