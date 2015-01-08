{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module %PACKAGE%.%MODEL%.Handler.%MODEL%
    ( get%MODEL%AdminIndexR
    , get%MODEL%AdminNewR
    , post%MODEL%AdminNewR
    , get%MODEL%AdminEditR
    , patch%MODEL%AdminEditR
    , delete%MODEL%AdminEditR
    ) where

import           Data.Maybe              (fromJust, isJust, fromMaybe)
import           Data.Time               (UTCTime, getCurrentTime, utctDay)
import           LambdaCms.Core.Settings
import           %PACKAGE%.%MODEL%.Import
import qualified %PACKAGE%.%MODEL%.Message as Msg

get%MODEL%AdminIndexR    :: %MODEL%Handler Html
get%MODEL%AdminNewR      :: %MODEL%Handler Html
post%MODEL%AdminNewR     :: %MODEL%Handler Html
get%MODEL%AdminEditR     :: %MODEL%Id -> %MODEL%Handler Html
patch%MODEL%AdminEditR   :: %MODEL%Id -> %MODEL%Handler Html
delete%MODEL%AdminEditR  :: %MODEL%Id -> %MODEL%Handler Html

get%MODEL%AdminIndexR = lift $ do
    can <- getCan
    (%LC_MODEL%s :: [Entity %MODEL%]) <- runDB $ selectList [] []
    adminLayout $ do
        setTitleI Msg.%MODEL%Index
        $(widgetFile "index")

get%MODEL%AdminNewR = lift $ do
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ %LC_MODEL%Form Nothing ct
    adminLayout $ do
        setTitleI Msg.New%MODEL%
        $(widgetFile "new")

post%MODEL%AdminNewR = do
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ %LC_MODEL%Form Nothing ct
    case results of
        FormSuccess %LC_MODEL% -> do
            _ <- lift . runDB $ insert %LC_MODEL%
            lift $ setMessageI Msg.SaveSuccess
            redirect %MODEL%AdminIndexR
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.New%MODEL%
                $(widgetFile "new")

get%MODEL%AdminEditR %LC_MODEL%Id = lift $ do
    %LC_MODEL% <- runDB $ get404 %LC_MODEL%Id
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ %LC_MODEL%Form (Just %LC_MODEL%) ct
    adminLayout $ do
        setTitleI Msg.Edit%MODEL%
        $(widgetFile "edit")

patch%MODEL%AdminEditR %LC_MODEL%Id = do
    %LC_MODEL% <- lift . runDB $ get404 %LC_MODEL%Id
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ %LC_MODEL%Form %LC_MODEL% ct
    case results of
        FormSuccess new%MODEL% -> do
            lift $ runDB $ replace %LC_MODEL%Id new%MODEL%
            lift $ setMessageI Msg.UpdateSuccess
            redirect $ %MODEL%AdminEditR %LC_MODEL%Id
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.Edit%MODEL%
                $(widgetFile "edit")

delete%MODEL%AdminEditR %LC_MODEL%Id = do
    %LC_MODEL% <- lift . runDB $ get404 %LC_MODEL%Id
    lift . runDB $ delete %LC_MODEL%Id
    lift $ setMessageI Msg.DeleteSuccess
    redirect %MODEL%AdminIndexR

%LC_MODEL%Form :: Maybe %MODEL% -> UTCTime -> %MODEL%Form %MODEL%
%LC_MODEL%Form m%MODEL% utct = renderBootstrap3 BootstrapBasicForm $ %MODEL%
    <$> areq textField (bfs Msg.Title) (%LC_MODEL%Title <$> m%MODEL%)
    <*> pure (fromMaybe utct $ %LC_MODEL%CreatedAt <$> m%MODEL%)
    <*  bootstrapSubmit (BootstrapSubmit Msg.Save " btn-success " [])
