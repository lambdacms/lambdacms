{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.Media.Message
       ( MediaMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       , dutchMessage
       ) where

import           Data.Monoid as Import ((<>))
import           Data.Text   (Text)


data MediaMessage =
    Back
  | Delete
  | Save
  | Upload
  | Rename
  | AddMedia
  | NoMediaFound
  | CantDisplayFileType
  | UnknownFileType
  | MediaIndex
  | NewMedia
  | EditMedia { label :: Text }
  | Information
  | Preview
  | Location
  | NewFilename
  | ChangeMediaSettings
  | ChangeFilename
  | Label
  | Description
  | UploadedOn
  | Filename
  | FileContentType
  | FileLocation
  | MenuMedia
  | SaveSuccess { label :: Text }
  | UpdateSuccess { label :: Text }
  | DeleteSuccess { label :: Text }
  | DeleteFail { label :: Text }
  | RenameSuccess
  | RenameFail

defaultMessage :: MediaMessage -> Text
defaultMessage = englishMessage

englishMessage :: MediaMessage -> Text
englishMessage Back                   = "Back"
englishMessage Delete                 = "Remove"
englishMessage Save                   = "Save"
englishMessage Upload                 = "Upload"
englishMessage Rename                 = "Rename"
englishMessage AddMedia               = "Upload media"
englishMessage NoMediaFound           = "No media found."
englishMessage CantDisplayFileType    = "Can't display file"
englishMessage UnknownFileType        = "Unknown file type"
englishMessage MediaIndex             = "Media overview"
englishMessage NewMedia               = "New media file"
englishMessage (EditMedia labelM)     = labelM
englishMessage Information            = "File information"
englishMessage Preview                = "Preview"
englishMessage Location               = "File"
englishMessage NewFilename            = "New filename"
englishMessage ChangeMediaSettings    = "Change media settings"
englishMessage ChangeFilename         = "Change filename"
englishMessage Label                  = "Label"
englishMessage Description            = "Description"
englishMessage UploadedOn             = "Uploaded on"
englishMessage Filename               = "Filename"
englishMessage FileContentType        = "Content type"
englishMessage FileLocation           = "File location"
englishMessage MenuMedia              = "Media"
englishMessage (SaveSuccess labelM)   = "Successfully created: " <>  labelM
englishMessage (UpdateSuccess labelM) = "Successfully updated: " <> labelM
englishMessage (DeleteSuccess labelM) = "Successfully deleted: " <> labelM
englishMessage (DeleteFail labelM)    = "Failed to delete: " <> labelM
englishMessage RenameSuccess          = "Successfully renamed file"
englishMessage RenameFail             = "Failed to rename file"

dutchMessage :: MediaMessage -> Text
dutchMessage Back                   = "Terug"
dutchMessage Delete                 = "Verwijderen"
dutchMessage Save                   = "Opslaan"
dutchMessage Upload                 = "Uploaden"
dutchMessage Rename                 = "Hernoemen"
dutchMessage AddMedia               = "Media toevoegen"
dutchMessage NoMediaFound           = "Geen media gevonden."
dutchMessage CantDisplayFileType    = "Kan bestand niet weergeven"
dutchMessage UnknownFileType        = "Onbekend bestandstype"
dutchMessage MediaIndex             = "Media overzicht"
dutchMessage NewMedia               = "Nieuw mediabestand"
dutchMessage (EditMedia labelM)     = labelM
dutchMessage Information            = "Bestandsinformatie"
dutchMessage Preview                = "Voorvertoning"
dutchMessage Location               = "Bestand"
dutchMessage NewFilename            = "Nieuwe bestandsnaam"
dutchMessage ChangeMediaSettings    = "Pas media instellingen aan"
dutchMessage ChangeFilename         = "Bestandsnaam wijzigen"
dutchMessage Label                  = "Label"
dutchMessage Description            = "Beschrijving"
dutchMessage UploadedOn             = "Geüpload op"
dutchMessage Filename               = "Bestandsnaam"
dutchMessage FileContentType        = "Inhoudstype"
dutchMessage FileLocation           = "Bestandslocatie"
dutchMessage MenuMedia              = "Media"
dutchMessage (SaveSuccess labelM)   = "Succesvol aangemaakt: " <> labelM
dutchMessage (UpdateSuccess labelM) = "Succesvol geüpdate: " <> labelM
dutchMessage (DeleteSuccess labelM) = "Succesvol verwijderd: " <> labelM
dutchMessage (DeleteFail labelM)    = "Verwijderen mislukt voor: " <> labelM
dutchMessage RenameSuccess          = "Bestand succesvol hernoemd"
dutchMessage RenameFail             = "Hernoemen van bestand mislukt"
