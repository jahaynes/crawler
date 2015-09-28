{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Hello where

import FrontEndTypes
import Ajax

import Fay.Text
import FFI

main :: Fay ()
main = do
    
    getElementById "urlsInQueue" >>= \urlsInQueue ->
        repeatTask 300 $ ajax "/queueSize/UrlQueue" $ setInnerHTML urlsInQueue

    getElementById "start" >>= \start -> do
        addEventListener start "onclick" $ \_ -> do
            ajax "/addUrl/something" print
            putStrLn "Crawling"

    putStrLn "Loaded"

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

addEventListener :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEventListener = ffi "%1[%2] = %3"

remove :: Element -> Element -> Fay ()
remove = ffi "%1.removeChild(%2)"

repeatTask :: Int -> Fay () -> Fay TaskId
repeatTask = ffi "setInterval(%2,%1)"

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1['innerHTML'] = %2"
