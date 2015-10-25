{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Data.Text.Lazy         as T              (toStrict, pack)
import Web.Spock.Safe                           
import Network.Wai.Middleware.Static            (staticPolicy, addBase)
import Text.Hamlet                              (Html, shamlet)
import Text.Blaze.Html.Renderer.Text            (renderHtml)
import Text.Blaze.Internal                      (Markup)

import Communication
import Data.ByteString.Char8                    as C8
import Control.Monad.IO.Class                   (liftIO)

main :: IO ()
main =
    runSpock 8080 $
        spockT id $ do
            middleware . staticPolicy . addBase $ "static"
            spockApp

spockApp :: SpockCtxT () IO ()
spockApp = do

    get root $ fromTemplate mainPage

    post ("addUrl" <//> var) $ \url -> do
        msg <- liftIO . addUrl $ url
        text . toStrict . T.pack . show $ msg

    get ("queueSize" <//> var) $ \qn -> do
        msg <- liftIO . queueSize $ qn
        text . toStrict . T.pack . show $ msg

    post "urlPatterns" $ do
        b <- body
        --Just one for now
        msg <- liftIO . setPatterns $ [b]
        text . toStrict . T.pack . show $ msg

    get "crawlerStatus" $ do
        msg <- liftIO crawlerStatus
        text . toStrict . T.pack . show $ msg

    post "idle" $ do
        msg <- liftIO idle
        text . toStrict . T.pack . show $ msg

    post "halt" $ do
        msg <- liftIO halt
        text . toStrict . T.pack . show $ msg

    where
    fromTemplate :: Html -> ActionT IO a
    fromTemplate = html . toStrict . renderHtml

addUrl :: String -> IO Message
addUrl url = sendAndGetReply $ CommandMessage (AddUrl (C8.pack url))

setPatterns :: [ByteString] -> IO Message
setPatterns patterns = sendAndGetReply $ CommandMessage (SetUrlPatterns patterns)

queueSize :: String -> IO Message
queueSize name = sendAndGetReply . QuestionMessage . GetQueueSize $ (read name :: QueueName)

crawlerStatus :: IO Message
crawlerStatus = sendAndGetReply $ QuestionMessage GetCrawlerStatus

idle :: IO Message
idle = sendAndGetReply $ CommandMessage Idle

halt :: IO Message
halt = sendAndGetReply $ CommandMessage Halt

mainPage :: Markup
mainPage = [shamlet|
    <!DOCTYPE html>
    <html lang="en-AU">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>Crawler Monitor
        <body>
            <div>
                <div>
                    URLs in queue:&nbsp;
                        <i>
                            <span id="urlsInQueue">

            <div>
                <div>
                    <label for="urlToAdd">Add Seed Url:
                    <input id="urlToAdd" type="text">
                <div>
                    <label for="urlPattern">Matching This Pattern:
                    <input id="urlPattern" type="text">

            <a href="#" id="addUrl">Add

            <a href="#" id="idle">Idle

            <a href="#" id="halt">Halt

            <div id="parent">Crawler Status:&nbsp;
                <i>
                    <span id="crawlerStatus">

            <script type="text/javascript" src="fay.js">
|]