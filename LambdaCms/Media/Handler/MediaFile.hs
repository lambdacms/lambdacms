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

import Text.Lucius (luciusFile)
import Data.Time (UTCTime, utctDay, getCurrentTime)
import Data.Text (unpack)
import qualified Data.Text as T (split, concat)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension, takeDirectory, takeFileName, normalise)
import System.Directory (removeFile, doesFileExist)

getMediaFileOverviewR :: MediaHandler Html
getMediaFileNewR      :: MediaHandler Html
postMediaFileNewR     :: MediaHandler Html
getMediaFileR         :: MediaFileId -> MediaHandler Html
postMediaFileR        :: MediaFileId -> MediaHandler Html
deleteMediaFileR      :: MediaFileId -> MediaHandler Html

getMediaFileOverviewR = do
  y <- lift $ getYesod
  let sr = unpack $ staticRoot y
  (files :: [Entity MediaFile]) <- lift . runDB $ selectList [] []
  lambdaCmsAdminLayoutSub $ do
    setTitleI MsgMediaOverview
    toWidget $(luciusFile "templates/media.lucius")
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
     (location, ctype) <- upload file (unpack name)
     _ <- lift . runDB . insert $ MediaFile location ctype label description ct
     setMessageI $ MsgSaveSuccess label
     redirect MediaFileOverviewR
   _ ->
     lambdaCmsAdminLayoutSub $ do
       setTitleI MsgNewMedia
       $(whamletFile "templates/new.hamlet")

getMediaFileR fileId = do
  y <- lift $ getYesod
  let sr = unpack $ staticRoot y
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
   _ -> do
     y <- lift $ getYesod
     let sr = unpack $ staticRoot y
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

uploadForm :: Form (FileInfo, Text, Text, Maybe Textarea)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
             <$> areq fileField (bfs MsgLocation) Nothing
             <*> areq textField (bfs MsgNewFilename) Nothing
             <*> areq textField (bfs MsgLabel) Nothing
             <*> aopt textareaField (bfs MsgDescription) Nothing
             <*  bootstrapSubmit (BootstrapSubmit MsgUpload " btn-success " [])

mediaFileForm :: MediaFile -> Form MediaFile
mediaFileForm mf = renderBootstrap3 BootstrapBasicForm $ MediaFile
                   <$> pure (mediaFileLocation mf)
                   <*> pure (mediaFileContentType mf)
                   <*> areq textField (bfs MsgLabel) (Just $ mediaFileLabel mf)
                   <*> aopt textareaField (bfs MsgDescription) (Just $ mediaFileDescription mf)
                   <*> pure (mediaFileUploadedAt mf)
                   <*  bootstrapSubmit (BootstrapSubmit MsgSave " btn-success " [])

upload :: FileInfo -> FilePath -> MediaHandler (FilePath, Text)
upload f nm = do
  y <- lift getYesod
  let filename = unpack $ fileName f
      ctype = fileContentType f
      location = (dropExtension nm) <.> (takeExtension filename)
      path = (staticDir y) </> location
  liftIO $ fileMove f path
  return (location, ctype)

deleteFile :: MediaFile -> MediaHandler Bool
deleteFile mf = do
  y <- lift getYesod
  let path = (staticDir y) </> (mediaFileLocation mf)
  fileExists <- liftIO $ doesFileExist path
  case fileExists of
   True -> do
     liftIO $ removeFile path
     fileStillExists <- liftIO $ doesFileExist path
     return fileStillExists
   False -> return False

splitContentType :: Text -> (Text, Text)
splitContentType ct = (c, t)
  where parts = T.split (== '/') ct
        c = head parts
        t = T.concat $ tail parts

isFileType :: MediaFile -> Text -> Bool
isFileType mf t = (fst $ splitContentType $ mediaFileContentType mf) == t

isImageFile :: MediaFile -> Bool
isImageFile = flip isFileType "image"

isApplicationFile :: MediaFile -> Bool
isApplicationFile = flip isFileType "application"
