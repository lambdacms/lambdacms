{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.Media.Message
       ( MediaMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       , dutchMessage
       ) where

import Data.Monoid (mappend)
import Data.Text (Text)

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
  | MediaOverview
  | NewMedia
  | EditMedia { label :: Text }
  | Information
  | Preview
  | Location
  | NewFilename
  | Label
  | Description
  | UploadedOn
  | Filename
  | FileContentType
  | FileLocation
  | SaveSuccess { label :: Text }
  | UpdateSuccess { label :: Text }
  | DeleteSuccess { label :: Text }
  | DeleteFail { label :: Text }
  | RenameSuccess
  | RenameFail

defaultMessage :: MediaMessage -> Text
defaultMessage = englishMessage

englishMessage :: MediaMessage -> Text
englishMessage Back                  = "Back"
englishMessage Delete                = "Remove"
englishMessage Save                  = "Save"
englishMessage Upload                = "Upload"
englishMessage Rename                = "Rename"
englishMessage AddMedia              = "Add media"
englishMessage NoMediaFound          = "No media found."
englishMessage CantDisplayFileType   = "Can't display file"
englishMessage UnknownFileType       = "Unknown file type"
englishMessage MediaOverview         = "Media overview"
englishMessage NewMedia              = "New media file"
englishMessage (EditMedia label)     = label
englishMessage Information           = "File information"
englishMessage Preview               = "Preview"
englishMessage Location              = "File"
englishMessage NewFilename           = "New filename"
englishMessage Label                 = "Label"
englishMessage Description           = "Description"
englishMessage UploadedOn            = "Uploaded on"
englishMessage Filename              = "Filename"
englishMessage FileContentType       = "Content type"
englishMessage FileLocation          = "File location"
englishMessage (SaveSuccess label)   = "Successfully created: " `mappend` label
englishMessage (UpdateSuccess label) = "Successfully updated: " `mappend` label
englishMessage (DeleteSuccess label) = "Successfully deleted: " `mappend` label
englishMessage (DeleteFail label)    = "Failed to delete: " `mappend` label
englishMessage RenameSuccess         = "Successfully renamed file"
englishMessage RenameFail            = "Failed to rename file"

dutchMessage :: MediaMessage -> Text
dutchMessage Back                  = "Terug"
dutchMessage Delete                = "Verwijderen"
dutchMessage Save                  = "Opslaan"
dutchMessage Upload                = "Uploaden"
dutchMessage Rename                = "Hernoemen"
dutchMessage AddMedia              = "Media toevoegen"
dutchMessage NoMediaFound          = "Geen media gevonden."
dutchMessage CantDisplayFileType   = "Kan bestand niet weergeven"
dutchMessage UnknownFileType       = "Onbekend bestandstype"
dutchMessage MediaOverview         = "Media overzicht"
dutchMessage NewMedia              = "Nieuw mediabestand"
dutchMessage (EditMedia label)     = label
dutchMessage Information           = "Bestandsinformatie"
dutchMessage Preview               = "Voorvertoning"
dutchMessage Location              = "Bestand"
dutchMessage NewFilename           = "Nieuwe bestandsnaam"
dutchMessage Label                 = "Label"
dutchMessage Description           = "Beschrijving"
dutchMessage UploadedOn            = "Geüpload op"
dutchMessage Filename              = "Bestandsnaam"
dutchMessage FileContentType       = "Inhoudstype"
dutchMessage FileLocation          = "Bestandslocatie"
dutchMessage (SaveSuccess label)   = "Succesvol aangemaakt: " `mappend` label
dutchMessage (UpdateSuccess label) = "Succesvol geüpdate: " `mappend` label
dutchMessage (DeleteSuccess label) = "Succesvol verwijderd: " `mappend` label
dutchMessage (DeleteFail label)    = "Verwijderen mislukt voor: " `mappend` label
dutchMessage RenameSuccess         = "Bestand succesvol hernoemd"
dutchMessage RenameFail            = "Hernoemen van bestand mislukt"
