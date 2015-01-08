{-# LANGUAGE OverloadedStrings #-}

module %PACKAGE%.%MODEL%.Message
       ( %MODEL%Message (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       ) where

import           Data.Monoid (mappend)
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
  | CreatedAt
  | Change%MODEL%Settings
  | No%MODEL%sFound

defaultMessage :: %MODEL%Message -> Text
defaultMessage = englishMessage

englishMessage :: %MODEL%Message -> Text
englishMessage Menu%MODEL%           = "%MODEL%s"
englishMessage New%MODEL%            = "New %LC_MODEL%"
englishMessage Edit%MODEL%           = "Edit %LC_MODEL%"
englishMessage SaveSuccess           = "Successfully saved"
englishMessage UpdateSuccess         = "Successfully updated"
englishMessage DeleteSuccess         = "Successfully deleted"
englishMessage Title                 = "Title"
englishMessage Save                  = "Save"
englishMessage Back                  = "Back"
englishMessage Delete                = "Delete"
englishMessage CreatedAt             = "Created at"
englishMessage Change%MODEL%Settings = "Change %LC_MODEL% settings"
englishMessage No%MODEL%sFound       = "No %LC_MODEL%s found"
