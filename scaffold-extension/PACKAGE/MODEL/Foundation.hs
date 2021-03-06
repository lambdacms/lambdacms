{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module %PACKAGE%.%MODEL%.Foundation where

import           Yesod
import           Data.Text                 (Text)
import           Network.Wai               (requestMethod)
import           Control.Arrow             ((&&&))

import           LambdaCms.Core

import           %PACKAGE%.%MODEL%.Message (%MODEL%Message, defaultMessage, englishMessage)
import qualified %PACKAGE%.%MODEL%.Message as Msg
import           %PACKAGE%.%MODEL%.Models

data %MODEL%Admin = %MODEL%Admin

mkYesodSubData "%MODEL%Admin" $(parseRoutesFile "config/routes")

instance %PACKAGE%%MODEL% master => RenderMessage master %MODEL%Message where
    renderMessage = render%MODEL%Message

type %MODEL%Handler a = forall master. %PACKAGE%%MODEL% master => HandlerT %MODEL%Admin (HandlerT master IO) a

type %MODEL%Form x = forall master. %PACKAGE%%MODEL% master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

class LambdaCmsAdmin master => %PACKAGE%%MODEL% master where
    %LCC_MODEL%R :: Route %MODEL%Admin -> Route master

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

instance %PACKAGE%%MODEL% master => LambdaCmsLoggable master %MODEL% where
    logMessage y "POST"   = translate%MODEL%Logs y Msg.LogCreated%MODEL%
    logMessage y "PATCH"  = translate%MODEL%Logs y Msg.LogUpdated%MODEL%
    logMessage y "DELETE" = translate%MODEL%Logs y Msg.LogDeleted%MODEL%
    logMessage _ _        = const []

translate%MODEL%Logs :: forall b master.
                     ( %PACKAGE%%MODEL% master
                     , RenderMessage master b
                     ) => master -> (Text -> b) -> %MODEL% -> [(Text, Text)]
translate%MODEL%Logs y msg e = map (id &&& messageFor) $ renderLanguages y
    where messageFor lang = renderMessage y [lang] . msg $ %LCC_MODEL%Title e

log%MODEL% :: %PACKAGE%%MODEL% master => %MODEL% -> HandlerT master IO [(Text, Text)]
log%MODEL% %LCC_MODEL% = do
    y <- getYesod
    method <- waiRequest >>= return . requestMethod
    return $ logMessage y method %LCC_MODEL%
