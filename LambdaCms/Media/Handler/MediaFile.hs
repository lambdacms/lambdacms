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
       , postMediaFileRenameR
       ) where

import LambdaCms.Media.Import

import Text.Lucius (luciusFile)
import Data.Time (utctDay, getCurrentTime)
import Data.Text (pack, unpack)
import qualified Data.Text as T (split, concat)
import System.FilePath
import System.Directory (renameFile, removeFile, doesFileExist)

getMediaFileOverviewR :: MediaHandler Html
getMediaFileNewR      :: MediaHandler Html
postMediaFileNewR     :: MediaHandler Html
getMediaFileR         :: MediaFileId -> MediaHandler Html
postMediaFileR        :: MediaFileId -> MediaHandler Html
deleteMediaFileR      :: MediaFileId -> MediaHandler Html
postMediaFileRenameR  :: MediaFileId -> MediaHandler Html

getMediaFileOverviewR = do
  y <- lift $ getYesod
  let sr = unpack $ staticRoot y
  (files :: [Entity MediaFile]) <- lift . runDB $ selectList [] []
  lambdaCmsAdminLayoutSub $ do
    setTitleI MsgMediaOverview
    toWidget $(luciusFile "templates/media.lucius")
    $(whamletFile "templates/overview.hamlet")

getMediaFileNewR = do
  (fWidget, enctype) <- generateFormPost uploadForm
  lambdaCmsAdminLayoutSub $ do
    setTitleI MsgNewMedia
    $(whamletFile "templates/new.hamlet")

postMediaFileNewR = do
  ((results, fWidget), enctype) <- runFormPost uploadForm
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
  (fWidget, enctype) <- generateFormPost $ mediaFileForm file
  (rfWidget, rEnctype) <- generateFormPost . renameForm $ mediaFileBaseName file
  lambdaCmsAdminLayoutSub $ do
    setTitle . toHtml $ mediaFileLabel file
    $(whamletFile "templates/edit.hamlet")

postMediaFileR fileId = do
  file <- lift . runDB $ get404 fileId
  ((results, fWidget), enctype) <- runFormPost $ mediaFileForm file
  case results of
   FormSuccess mf -> do
     _ <- lift $ runDB $ update fileId [MediaFileLabel =. mediaFileLabel mf, MediaFileDescription =. mediaFileDescription mf]
     setMessageI $ MsgUpdateSuccess (mediaFileLabel mf)
     redirect $ MediaFileR fileId
   _ -> do
     y <- lift $ getYesod
     let sr = unpack $ staticRoot y
     (rfWidget, rEnctype) <- generateFormPost . renameForm $ mediaFileBaseName file
     lambdaCmsAdminLayoutSub $ do
       setTitle . toHtml $ mediaFileLabel file
       $(whamletFile "templates/edit.hamlet")

deleteMediaFileR fileId = do
  file <- lift . runDB $ get404 fileId
  fileExists <- deleteMediaFile file
  case fileExists of
   False -> do
     lift . runDB $ delete fileId
     setMessageI $ MsgDeleteSuccess (mediaFileLabel file)
     redirect MediaFileOverviewR
   True -> do
     setMessageI $ MsgDeleteFail (mediaFileLabel file)
     redirect $ MediaFileR fileId

postMediaFileRenameR fileId = do
  file <- lift . runDB $ get404 fileId
  ((results, rfWidget), rEnctype) <- runFormPost . renameForm $ mediaFileBaseName file
  case results of
   FormSuccess nn
     | nn == (mediaFileBaseName file) -> do
         setMessageI MsgRenameSuccess
         redirect $ MediaFileR fileId
     | otherwise -> do
         (isRenamed, location) <- renameMediaFile file nn
         case isRenamed of
          True -> do
            _ <- lift . runDB $ update fileId [MediaFileLocation =. location]
            setMessageI MsgRenameSuccess
            redirect $ MediaFileR fileId
          False -> do
            setMessageI MsgRenameFail
            redirect $ MediaFileR fileId
   _ -> do
     y <- lift $ getYesod
     let sr = unpack $ staticRoot y
     (fWidget, enctype) <- generateFormPost $ mediaFileForm file
     lambdaCmsAdminLayoutSub $ do
       setTitle . toHtml $ mediaFileLabel file
       $(whamletFile "templates/edit.hamlet")

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

renameForm :: Text -> Form Text
renameForm fp = renderBootstrap3 BootstrapBasicForm $
                areq textField (bfs MsgNewFilename) (Just fp)
                <*  bootstrapSubmit (BootstrapSubmit MsgRename " btn-success " [])

upload :: FileInfo -> FilePath -> MediaHandler (FilePath, Text)
upload f nn = do
  y <- lift getYesod
  let filename = unpack $ fileName f
      ctype = fileContentType f
      location = (dropExtension nn) <.> (takeExtension filename)
      path = (staticDir y) </> location
  liftIO $ fileMove f path
  return (location, ctype)

renameMediaFile :: MediaFile -> Text -> MediaHandler (Bool, FilePath)
renameMediaFile mf nn = do
  y <- lift getYesod
  let clocation = mediaFileLocation mf
      nlocation = replaceBaseName clocation $ unpack nn
      cpath = (staticDir y) </> clocation
      npath = (staticDir y) </> nlocation
  fileExists <- liftIO $ doesFileExist cpath
  case fileExists of
   True -> do
     liftIO $ renameFile (cpath) (npath)
     return (True, nlocation)
   False -> return (False, clocation)

deleteMediaFile :: MediaFile -> MediaHandler Bool
deleteMediaFile mf = do
  y <- lift getYesod
  let path = (staticDir y) </> (mediaFileLocation mf)
  fileExists <- liftIO $ doesFileExist path
  case fileExists of
   True -> do
     liftIO $ removeFile path
     fileStillExists <- liftIO $ doesFileExist path
     return fileStillExists
   False -> return False

mediaFileBaseName :: MediaFile -> Text
mediaFileBaseName = pack . takeBaseName . mediaFileLocation

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
