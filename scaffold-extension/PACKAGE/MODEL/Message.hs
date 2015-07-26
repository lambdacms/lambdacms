{-# LANGUAGE OverloadedStrings #-}

module %PACKAGE%.%MODEL%.Message
       ( %MODEL%Message (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       ) where

import           Data.Monoid ((<>))
import           Data.Text   (Text)

data %MODEL%Message =
    Menu%MODEL%
  | %MODEL%Index
  | New%MODEL%
  | Edit%MODEL%
  | SaveSuccess
  | UpdateSuccess
  | DeleteSuccess
  | Title
  | Save
  | Back
  | Delete
  | CreatedOn
  | Change%MODEL%Settings
  | No%MODEL%sFound
  | LogCreated%MODEL% { title :: Text }
  | LogUpdated%MODEL% { title :: Text }
  | LogDeleted%MODEL% { title :: Text }

defaultMessage :: %MODEL%Message -> Text
defaultMessage = englishMessage

englishMessage :: %MODEL%Message -> Text
englishMessage Menu%MODEL%            = "%MODEL%s"
englishMessage %MODEL%Index           = "%MODEL% overview"
englishMessage New%MODEL%             = "New %LC_MODEL%"
englishMessage Edit%MODEL%            = "Edit %LC_MODEL%"
englishMessage SaveSuccess            = "Successfully saved"
englishMessage UpdateSuccess          = "Successfully updated"
englishMessage DeleteSuccess          = "Successfully deleted"
englishMessage Title                  = "Title"
englishMessage Save                   = "Save"
englishMessage Back                   = "Back"
englishMessage Delete                 = "Delete"
englishMessage CreatedOn              = "Created on"
englishMessage Change%MODEL%Settings  = "Change %LC_MODEL% settings"
englishMessage No%MODEL%sFound        = "No %LC_MODEL%s found"
englishMessage (LogCreated%MODEL% t)  = "Created %LC_MODEL% \"" <> t <> "\""
englishMessage (LogUpdated%MODEL% t)  = "Updated %LC_MODEL% \"" <> t <> "\""
englishMessage (LogDeleted%MODEL% t)  = "Deleted %LC_MODEL% \"" <> t <> "\""
