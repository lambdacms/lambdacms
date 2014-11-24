{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module LambdaCms.Media.Foundation where

import Yesod
import Data.Text

import LambdaCms.Core

-- import LambdaCms.Media.Models
import LambdaCms.Media.Routes
import LambdaCms.Media.Message (MediaMessage, defaultMessage, englishMessage, dutchMessage)
import qualified LambdaCms.Media.Message as Msg

class LambdaCmsAdmin master => LambdaCmsMedia master where
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

type MediaHandler a = forall master. LambdaCmsMedia master => HandlerT Media (HandlerT master IO) a

type MediaWidget = forall master. LambdaCmsMedia master => WidgetT master IO ()

type Form x = forall master. LambdaCmsMedia master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

instance RenderMessage Media FormMessage where
  renderMessage _ _ = defaultFormMessage

instance LambdaCmsMedia master => RenderMessage master MediaMessage where
  renderMessage = renderMediaMessage

defaultMediaAdminMenu :: LambdaCmsMedia master => (Route Media -> Route master) -> [AdminMenuItem master]
defaultMediaAdminMenu tp = [ MenuItem (SomeMessage Msg.MenuMedia) (tp MediaFileOverviewR) "picture" ]
