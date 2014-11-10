{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module LambdaCms.Media.Handler.MediaFile
       ( getMediaFileOverviewR
       , getMediaFileNewR
       , postMediaFileNewR
       , getMediaFileR
       , postMediaFileR
       , deleteMediaFileR
       ) where

import LambdaCms.Media.Import

import Data.Time (UTCTime, getCurrentTime)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

getMediaFileOverviewR :: MediaHandler Html
getMediaFileNewR      :: MediaHandler Html
postMediaFileNewR     :: MediaHandler Html
getMediaFileR         :: MediaFileId -> MediaHandler Html
postMediaFileR        :: MediaFileId -> MediaHandler Html
deleteMediaFileR      :: MediaFileId -> MediaHandler Html

uploadForm :: Form (FileInfo, Text, Maybe Textarea)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,,)
             <$> areq fileField "location" Nothing
             <*> areq textField "name" Nothing
             <*> aopt textareaField "description" Nothing

upload :: FileInfo -> MediaHandler FilePath
upload f = do
  y <- lift getYesod
  -- hash filename and use hash for uploaded file
  let filename = unpack $ fileName f
      path = (uploadDir y) </> filename
  liftIO $ fileMove f path
  return path

-- mediaFileForm :: MediaFile -> Form MediaFile
-- mediaFileForm mf = renderBootstrap3 BootstrapBasicForm $ MediaFile
--                    <$> pure $ mediaFileLocation mf
--                    <*> areq textField "name" Nothing -- (Just $ mediaFileName mf)
--                    <*> aopt textareaField "description" Nothing -- $ mediaFileDescription mf
--                    <*> pure $ mediaFileUploadedAt mf

getMediaFileOverviewR = lambdaCmsAdminLayoutSub $(whamletFile "templates/overview.hamlet")

getMediaFileNewR = do
  (fwidget, enctype) <- generateFormPost uploadForm
  lambdaCmsAdminLayoutSub $(whamletFile "templates/new.hamlet")

postMediaFileNewR = do
  ((results, fwidget), enctype) <- runFormPost uploadForm
  case results of
   FormSuccess (file, name, description) -> do
     ct <- liftIO getCurrentTime
     location <- upload file
     _ <- lift . runDB . insert $ MediaFile location name description ct
     redirect MediaFileOverviewR
   _ ->
     lambdaCmsAdminLayoutSub $(whamletFile "templates/new.hamlet")

getMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
postMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
deleteMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
