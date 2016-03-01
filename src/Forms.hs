{-# LANGUAGE OverloadedStrings #-}

module Forms where

import Types
import Urls                     (urlPlus)

import Data.ByteString.Char8    (ByteString, unpack)
import Data.CaseInsensitive     (mk)
import qualified Data.Map       as M
import Data.Maybe               (mapMaybe)
import Safe                     (headMay)
import Text.HTML.TagSoup        hiding (parseTags)
import Text.HTML.TagSoup.Fast   (parseTags)
import Text.Regex.PCRE          ((=~))
import Network.HTTP.Types       (Method, methodGet)
import Network.URI              (parseAbsoluteURI, parseRelativeReference)

getForms :: CanonicalUrl -> ByteString -> [Form]
getForms onUrl = map asForm . isolateForms . parseTags

    where
    asForm :: [Tag ByteString] -> Form
    asForm tags = Form onUrl (Action method action) inputs

        where
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

        inputs :: [FormParameters]
        inputs = map (\(TagOpen _ attrs) -> FormParameters attrs) . filter (isTagOpenName "input") $ tags

    isolateForms :: [Tag ByteString] -> [[Tag ByteString]]
    isolateForms = map (takeWhilePlus1 (not . isTagCloseName "form"))
                . sections (isTagOpenName "form")

        where
        takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
        takeWhilePlus1 _     [] = []
        takeWhilePlus1 p (x:xs) | p x       = x : takeWhilePlus1 p xs
                                | otherwise = x : takeWhilePlus1 p []

selectFormOptions :: FormInstructions -> [Form] -> Maybe DownloadRequest
selectFormOptions              _ [] = Nothing
selectFormOptions allFormActions (Form (CanonicalUrl urlFormLocation) (Action method (RelativeUrl relUrl)) inputs : fs) = do

    let formTargetUrl =
            case (parseAbsoluteURI $ unpack urlFormLocation, parseRelativeReference $ unpack relUrl) of
                (Just ou, Just u) -> ou `urlPlus` u
                _ -> error "Could not derelativise url in selectFormOptions"

    case applicableFormParameters formTargetUrl of
        Just (label, ps) -> Just (FormRequest label method formTargetUrl ps)
        Nothing          -> selectFormOptions allFormActions fs

    where
    applicableFormParameters :: CanonicalUrl -> Maybe (Label, FormParameters)
    applicableFormParameters (CanonicalUrl formTargetUrl) = do

        (label, FormParameters parameters) <- headMay
                                            . M.toList
                                            . M.map (\(_, _, ps) -> ps)
                                            . M.filter (\(UrlRegex ur, FormActionRegex fr, _)
                                                -> urlFormLocation =~ ur && formTargetUrl =~ fr)
                                            . (\(FormInstructions fiMap) -> fiMap)
                                            $ allFormActions

        return (label, FormParameters $ prefillExisting inputs ++ parameters)

        where
        prefillExisting :: [FormParameters] -> [(FormKey, FormValue)]
        prefillExisting = mapMaybe extractNameAndValue
            where
            extractNameAndValue (FormParameters xs) = (,) <$> extract "name" xs <*> extract "value" xs
            extract key = (headMay . map snd . filter (\x -> fst x == key))