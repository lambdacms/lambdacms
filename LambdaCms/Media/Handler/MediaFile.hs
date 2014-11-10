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
import System.Directory (removeFile, doesFileExist)

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

deleteFile :: MediaFile -> MediaHandler Bool
deleteFile mf = do
  y <- lift getYesod
  let path = (uploadDir y) </> (mediaFileLocation mf)
  fileExists <- liftIO $ doesFileExist path
  case fileExists of
   True -> do
     liftIO $ removeFile path
     fileStillExists <- liftIO $ doesFileExist path
     return fileStillExists
   False -> return False

mediaFileForm :: MediaFile -> Form MediaFile
mediaFileForm mf = renderBootstrap3 BootstrapBasicForm $ MediaFile
                   <$> pure (mediaFileLocation mf)
                   <*> areq textField "label" (Just $ mediaFileLabel mf)
                   <*> aopt textareaField "description" (Just $ mediaFileDescription mf)
                   <*> pure (mediaFileUploadedAt mf)

getMediaFileOverviewR = do
  (files :: [Entity MediaFile]) <- lift . runDB $ selectList [] []
  lambdaCmsAdminLayoutSub $ do
    setTitleI MsgMediaOverview
    $(whamletFile "templates/overview.hamlet")

getMediaFileNewR = do
  (fwidget, enctype) <- generateFormPost uploadForm
  lambdaCmsAdminLayoutSub $ do
    setTitleI MsgNewMedia
    $(whamletFile "templates/new.hamlet")

postMediaFileNewR = do
  ((results, fwidget), enctype) <- runFormPost uploadForm
  case results of
   FormSuccess (file, name, label, description) -> do
     ct <- liftIO getCurrentTime
     location <- upload file (unpack name)
     _ <- lift . runDB . insert $ MediaFile location label description ct
     setMessageI $ MsgSaveSuccess label
     redirect MediaFileOverviewR
   _ ->
     lambdaCmsAdminLayoutSub $ do
       setTitleI MsgNewMedia
       $(whamletFile "templates/new.hamlet")

getMediaFileR fileId = do
  file <- lift . runDB $ get404 fileId
  (fwidget, enctype) <- generateFormPost $ mediaFileForm file
  lambdaCmsAdminLayoutSub $ do
    setTitle . toHtml $ mediaFileLabel file
    $(whamletFile "templates/edit.hamlet")

postMediaFileR fileId = do
  file <- lift . runDB $ get404 fileId
  ((results, fwidget), enctype) <- runFormPost $ mediaFileForm file
  case results of
   FormSuccess mf -> do
     _ <- lift $ runDB $ update fileId [MediaFileLabel =. mediaFileLabel mf, MediaFileDescription =. mediaFileDescription mf]
     setMessageI $ MsgUpdateSuccess (mediaFileLabel mf)
     redirect $ MediaFileR fileId
   _ ->
     lambdaCmsAdminLayoutSub $ do
       setTitle . toHtml $ mediaFileLabel file
       $(whamletFile "templates/edit.hamlet")

deleteMediaFileR fileId = do
  file <- lift . runDB $ get404 fileId
  fileExists <- deleteFile file
  case fileExists of
   False -> do
     lift . runDB $ delete fileId
     setMessageI $ MsgDeleteSuccess (mediaFileLabel file)
     redirect MediaFileOverviewR
   True -> do
     setMessageI $ MsgDeleteFail (mediaFileLabel file)
     redirect $ MediaFileR fileId
