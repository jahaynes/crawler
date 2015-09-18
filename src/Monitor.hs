{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Data.Text.Lazy                           (toStrict)
import Web.Spock.Safe                           
import Network.Wai.Middleware.Static            (staticPolicy, addBase)
import Text.Hamlet                              (Html, shamlet)
import Text.Blaze.Html.Renderer.Text            (renderHtml)
import Text.Blaze.Internal                      (Markup)

main :: IO ()
main =
    runSpock 8080 $
        spockT id $ do
            get root $ fromTemplate mainPage

    where
    fromTemplate :: Html -> ActionT IO a
    fromTemplate = html . toStrict . renderHtml
        
mainPage :: Markup        
mainPage = [shamlet|
    <!DOCTYPE html>
    <html lang="en-AU">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>Crawler Monitor
        <body>
|]