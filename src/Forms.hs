{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Forms where

import FormTypes
import Types
import Urls                     (derelativise)

import Data.ByteString.Char8    (ByteString, empty)
import Data.CaseInsensitive     (mk)
import qualified Data.Map       as M
import Data.Maybe               (mapMaybe)
import Safe                     (headMay)
import Text.HTML.TagSoup
import Text.Regex.PCRE          ((=~))
import Network.HTTP.Types       (Method, methodGet)

emptyFormActions :: SuppliedFormActions
emptyFormActions = SuppliedFormActions M.empty

getForms :: CanonicalUrl -> [Tag ByteString] -> [Form]
getForms onUrl = map asForm . isolateForms

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

selectFormOptions :: SuppliedFormActions -> [Form] -> Maybe DownloadRequest
selectFormOptions              _ [] = Nothing
selectFormOptions suppliedFormActions (Form formLocation (Action method (RelativeUrl relUrl)) inputs : fs) = do

    let formTargetUrl =
            case derelativise formLocation relUrl of
                Left l -> error $ show l --TODO clean this error handling up to act on one form at a time
                Right r -> r

    case mergeSuppliedAndDiscoveredFormActions suppliedFormActions formLocation formTargetUrl inputs of
        Just (CombinedFormActions label ps) -> Just (FormRequest label method formTargetUrl ps)
        Nothing          -> selectFormOptions suppliedFormActions fs

mergeSuppliedAndDiscoveredFormActions :: SuppliedFormActions -> CanonicalUrl -> CanonicalUrl -> [FormParameters] -> Maybe CombinedFormActions
mergeSuppliedAndDiscoveredFormActions (SuppliedFormActions suppliedFormActions)
                                      (CanonicalUrl urlFormLocation)
                                      (CanonicalUrl formTargetUrl)
                                      discoveredFormParameters = do

    (ApplicableSuppliedFormActions label params) <- getApplicableSuppliedFormActions

    let discovered = M.fromList . mapMaybe discoverFormParameter $ discoveredFormParameters
        sendTheseParams = M.toList . apply params $ discovered

    return $ CombinedFormActions label (FormParameters sendTheseParams)

    where
    apply :: FormParameters -> M.Map FormKey FormValue -> M.Map FormKey FormValue
    apply (FormParameters ps_) = go ps_
        where
        go     [] !m = m
        go ((k,v):ps) m =
            case M.lookup k m of
                Nothing -> error $ "Trying to override non-existing key: " ++ show k
                Just _ -> go ps (M.insert k v m)

    discoverFormParameter :: FormParameters -> Maybe (FormKey, FormValue)
    discoverFormParameter (FormParameters kvs) = do
        name <- headMay . map snd . filter (\(k,_) -> mk k == mk "name") $ kvs
        case headMay . map snd . filter (\(k,_) -> mk k == mk "value") $ kvs of
            Nothing -> return (name, empty)
            Just val -> return (name, val)

    getApplicableSuppliedFormActions :: Maybe ApplicableSuppliedFormActions
    getApplicableSuppliedFormActions = headMay
                                     . map (\(label, (_,_,ps)) -> ApplicableSuppliedFormActions label ps)
                                     . M.toList
                                     . M.filter (\(UrlRegex ur, FormActionRegex fr, _) -> urlFormLocation =~ ur && formTargetUrl =~ fr)
                                     $ suppliedFormActions
