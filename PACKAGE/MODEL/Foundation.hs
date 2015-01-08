{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module %PACKAGE%.%MODEL%.Foundation where

import           Data.Monoid             ((<>))
import           Data.Text               (Text, unpack)
import qualified Data.Text               as T
import           System.FilePath         ((</>))
import           Yesod

import           LambdaCms.Core

import           %PACKAGE%.%MODEL%.Message (%MODEL%Message, defaultMessage,
                                          englishMessage)
import qualified %PACKAGE%.%MODEL%.Message as Msg
import           %PACKAGE%.%MODEL%.Models

data %MODEL%Admin = %MODEL%Admin

mkYesodSubData "%MODEL%Admin" $(parseRoutesFile "config/routes")

instance %PACKAGE%%MODEL% master => RenderMessage master %MODEL%Message where
    renderMessage = render%MODEL%Message

type %MODEL%Handler a = forall master. %PACKAGE%%MODEL% master => HandlerT %MODEL%Admin (HandlerT master IO) a

type %MODEL%Form x = forall master. %PACKAGE%%MODEL% master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

class LambdaCmsAdmin master => %PACKAGE%%MODEL% master where
    %LC_MODEL%R :: Route %MODEL%Admin -> Route master

    render%MODEL%Message :: master
                       -> [Text]
                       -> %MODEL%Message
                       -> Text
    render%MODEL%Message m (lang:langs) = do
        case (lang `elem` (renderLanguages m), lang) of
            (True, "en") -> englishMessage
            _ -> render%MODEL%Message m langs
    render%MODEL%Message _ _ = defaultMessage

default%MODEL%AdminMenu :: %PACKAGE%%MODEL% master => (Route %MODEL%Admin -> Route master) -> [AdminMenuItem master]
default%MODEL%AdminMenu tp = [ MenuItem (SomeMessage Msg.Menu%MODEL%) (tp %MODEL%AdminIndexR) "pushpin" ]
