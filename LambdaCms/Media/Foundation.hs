{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module LambdaCms.Media.Foundation where

import           Data.Monoid             ((<>))
import           Data.Text               (Text, unpack)
import qualified Data.Text               as T
import           System.FilePath         ((</>))
import           Yesod

import           LambdaCms.Core

import           LambdaCms.Media.Message (MediaMessage, defaultMessage,
                                          dutchMessage, englishMessage)
import qualified LambdaCms.Media.Message as Msg
import           LambdaCms.Media.Models
import           LambdaCms.Media.Routes

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

attachedMaybeMediaFile :: LambdaCmsMedia master
                          => a
                          -> (a -> Maybe MediaFileId)
                          -> HandlerT master IO (Maybe MediaFile)
attachedMaybeMediaFile model getId = do
  case getId model of
   Just mfid -> runDB $ get mfid
   Nothing -> return Nothing

mediaFileSrc :: LambdaCmsMedia master => master -> MediaFile -> FilePath
mediaFileSrc y mf = (unpack $ staticRoot y) </> (mediaFileLocation mf)

mediaFileImg :: LambdaCmsMedia master => MediaFile -> [Text] -> [(Text, Text)] -> WidgetT master IO ()
mediaFileImg mf classes attrs = do
  y <- handlerToWidget getYesod
  [whamlet|<img src=#{mediaFileSrc y mf} class=#{T.unwords classes} *{attrs} alt=#{mediaFileLabel mf}>|]
