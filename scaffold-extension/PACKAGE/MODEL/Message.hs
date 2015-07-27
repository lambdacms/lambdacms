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
englishMessage Menu%MODEL%            = "%LCS_MODEL%s"
englishMessage %MODEL%Index           = "%LCS_MODEL% overview"
englishMessage New%MODEL%             = "New %LCS_MODEL%"
englishMessage Edit%MODEL%            = "Edit %LCS_MODEL%"
englishMessage SaveSuccess            = "Successfully saved %LCS_MODEL%"
englishMessage UpdateSuccess          = "Successfully updated %LCS_MODEL%"
englishMessage DeleteSuccess          = "Successfully deleted %LCS_MODEL%"
englishMessage Title                  = "Title"
englishMessage Save                   = "Save"
englishMessage Back                   = "Back"
englishMessage Delete                 = "Delete"
englishMessage CreatedOn              = "Created on"
englishMessage Change%MODEL%Settings  = "Change %LCS_MODEL% settings"
englishMessage No%MODEL%sFound        = "No %LCS_MODEL%s found"
englishMessage (LogCreated%MODEL% t)  = "Created %LCS_MODEL% \"" <> t <> "\""
englishMessage (LogUpdated%MODEL% t)  = "Updated %LCS_MODEL% \"" <> t <> "\""
englishMessage (LogDeleted%MODEL% t)  = "Deleted %LCS_MODEL% \"" <> t <> "\""
