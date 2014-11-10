{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCms.Media.Handler.MediaFile
       ( getMediaFileOverviewR
       , getMediaFileNewR
       , postMediaFileNewR
       , getMediaFileR
       , postMediaFileR
       , deleteMediaFileR
       ) where

import LambdaCms.Media.Import

import Data.Time (UTCTime, utctDay, getCurrentTime)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension)

getMediaFileOverviewR :: MediaHandler Html
getMediaFileNewR      :: MediaHandler Html
postMediaFileNewR     :: MediaHandler Html
getMediaFileR         :: MediaFileId -> MediaHandler Html
postMediaFileR        :: MediaFileId -> MediaHandler Html
deleteMediaFileR      :: MediaFileId -> MediaHandler Html

uploadForm :: Form (FileInfo, Text, Text, Maybe Textarea)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
             <$> areq fileField "location" Nothing
             <*> areq textField "new file name" Nothing
             <*> areq textField "label" Nothing
             <*> aopt textareaField "description" Nothing

upload :: FileInfo -> FilePath -> MediaHandler FilePath
upload f nm = do
  y <- lift getYesod
  let filename = unpack $ fileName f
      nfm = (dropExtension nm) <.> (takeExtension filename)
      path = (uploadDir y) </> nfm
  liftIO $ fileMove f path
  return nfm

-- mediaFileForm :: MediaFile -> Form MediaFile
-- mediaFileForm mf = renderBootstrap3 BootstrapBasicForm $ MediaFile
--                    <$> pure $ mediaFileLocation mf
--                    <*> areq textField "label" Nothing -- (Just $ mediaFileLabel mf)
--                    <*> aopt textareaField "description" Nothing -- $ mediaFileDescription mf
--                    <*> pure $ mediaFileUploadedAt mf

getMediaFileOverviewR = do
  (files :: [Entity MediaFile]) <- lift . runDB $ selectList [] []
  lambdaCmsAdminLayoutSub $ do
    setTitle "Media overview"
    $(whamletFile "templates/overview.hamlet")

getMediaFileNewR = do
  (fwidget, enctype) <- generateFormPost uploadForm
  lambdaCmsAdminLayoutSub $ do
    setTitle "New media"
    $(whamletFile "templates/new.hamlet")

postMediaFileNewR = do
  ((results, fwidget), enctype) <- runFormPost uploadForm
  case results of
   FormSuccess (file, name, label, description) -> do
     ct <- liftIO getCurrentTime
     location <- upload file (unpack name)
     _ <- lift . runDB . insert $ MediaFile location label description ct
     redirect MediaFileOverviewR
   _ ->
     lambdaCmsAdminLayoutSub $ do
       setTitle "New media"
       $(whamletFile "templates/new.hamlet")

getMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
postMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
deleteMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
