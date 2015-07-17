{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module LambdaCms.Media.Foundation where

import           Data.Text               (Text, unpack)
import qualified Data.Text               as T
import           System.FilePath         ((</>))
import           Yesod

import           LambdaCms.Core

import           LambdaCms.Media.Message (MediaMessage, defaultMessage,
                                          dutchMessage, englishMessage)
import qualified LambdaCms.Media.Message as Msg
import           LambdaCms.Media.Models


data MediaAdmin = MediaAdmin

mkYesodSubData "MediaAdmin" $(parseRoutesFile "config/routes")

-- This instance requires OPTIONS_GHC -fno-warn-orphans
-- TODO: Can we avoid surpressing orphan warnings?
instance LambdaCmsMedia master => RenderMessage master MediaMessage where
    renderMessage = renderMediaMessage

type MediaHandler a = forall master. LambdaCmsMedia master => HandlerT MediaAdmin (HandlerT master IO) a

type MediaForm x = forall master. LambdaCmsMedia master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

class LambdaCmsAdmin master => LambdaCmsMedia master where
    mediaR :: Route MediaAdmin -> Route master

    staticDir :: master -> FilePath
    staticRoot :: master -> Text
    uploadDir :: master -> FilePath
    uploadDir _ = "uploads"

    renderMediaMessage :: master
                       -> [Text]
                       -> MediaMessage
                       -> Text
    renderMediaMessage m (lang:langs) = do
        case (lang `elem` (renderLanguages m), lang) of
            (True, "en") -> englishMessage
            (True, "nl") -> dutchMessage
            _ -> renderMediaMessage m langs
    renderMediaMessage _ _ = defaultMessage

defaultMediaAdminMenu :: LambdaCmsMedia master => (Route MediaAdmin -> Route master) -> [AdminMenuItem master]
defaultMediaAdminMenu tp = [ MenuItem (SomeMessage Msg.MenuMedia) (tp MediaAdminR) "picture" ]

attachedMaybeMedia :: LambdaCmsMedia master
                       => model
                       -> (model -> Maybe MediaId)
                       -> HandlerT master IO (Maybe Media)
attachedMaybeMedia model getId = do
    case getId model of
        Just mfid -> runDB $ get mfid
        Nothing -> return Nothing

mediaSrc :: LambdaCmsMedia master => master -> Media -> FilePath
mediaSrc y mf = (unpack $ staticRoot y) </> (mediaLocation mf)

mediaImg :: LambdaCmsMedia master => Media -> [Text] -> [(Text, Text)] -> WidgetT master IO ()
mediaImg mf classes attrs = do
    y <- handlerToWidget getYesod
    [whamlet|<img src=#{mediaSrc y mf} class=#{T.unwords classes} *{attrs} alt=#{mediaLabel mf}>|]
