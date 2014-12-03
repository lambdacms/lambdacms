{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.Core.Message
       ( CoreMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       , dutchMessage
       ) where

import Data.Monoid (mappend)
import Data.Text (Text)

data CoreMessage =
    Dashboard
  | LambdaCms
  | NotLoggedIn
  | Logout
  | AccountSettings
  | UserIndex
  | EmailAddress
  | NewUser
  | EditUser { name :: Text }
  | PasswordTooShort
  | PasswordMismatch
  | Username
  | Password
  | Confirm
  | Create
  | Save
  | Submit
  | Change
  | Back
  | Remove
  | CreatedOn
  | LastLogin
  | AccountActivationPending
  | AccountActivationSuccess
  | AccountAlreadyActivated
  | TokenMismatch
  | NoUsersFound
  | SuccessCreate
  | SuccessReplace
  | SuccessUpdate
  | SuccessChgPwd
  | SuccessDelete
  | MenuDashboard
  | MenuUsers
  | DeletedUser { name :: Text }
  | TimeJustNow
  | TimeSecondsAgo { time :: Text }
  | TimeOneMinuteAgo
  | TimeMinutesAgo { time :: Text }
  | TimeOneHourAgo
  | TimeAboutHoursAgo { time :: Text }
  | TimeAt { time :: Text }
  | TimeDaysAgo { time :: Text }
  | TimeWeekAgo { time :: Text }
  | TimeWeeksAgo { time :: Text }
  | TimeOnYear { time :: Text }
  | DayOfWeekFmt

defaultMessage :: CoreMessage -> Text
defaultMessage = englishMessage

englishMessage :: CoreMessage -> Text
englishMessage Dashboard                = "Dashboard"
englishMessage LambdaCms                = "LambdaCms"
englishMessage NotLoggedIn              = "Not logged in"
englishMessage Logout                   = "Logout"
englishMessage AccountSettings          = "Account settings"
englishMessage UserIndex                = "User overview"
englishMessage EmailAddress             = "E-mail address"
englishMessage NewUser                  = "New user"
englishMessage (EditUser name)          = name
englishMessage PasswordTooShort         = "Password is too short"
englishMessage PasswordMismatch         = "Passwords don't match"
englishMessage Username                 = "Username"
englishMessage Password                 = "Password"
englishMessage Confirm                  = "Confirm"
englishMessage Create                   = "Create"
englishMessage Save                     = "Save"
englishMessage Submit                   = "Submit"
englishMessage Change                   = "Change"
englishMessage Back                     = "Back"
englishMessage Remove                   = "Remove"
englishMessage CreatedOn                = "Created on"
englishMessage LastLogin                = "Last login"
englishMessage AccountActivationPending = "Pending"
englishMessage AccountActivationSuccess = "Activated"
englishMessage AccountAlreadyActivated  = "This account is already activated"
englishMessage TokenMismatch            = "Invalid token"
englishMessage NoUsersFound             = "No users found."
englishMessage SuccessCreate            = "successfully created"
englishMessage SuccessReplace           = "Successfully replaced"
englishMessage SuccessUpdate            = "Successfully updated"
englishMessage SuccessChgPwd            = "Successfully changed password"
englishMessage SuccessDelete            = "Successfully deleted"
englishMessage MenuDashboard            = englishMessage Dashboard
englishMessage MenuUsers                = "Users"
englishMessage (DeletedUser name)       = "User " `mappend` name `mappend` " deleted"
englishMessage TimeJustNow              = "Just now"
englishMessage (TimeSecondsAgo time)    = time `mappend` " seconds ago"
englishMessage TimeOneMinuteAgo         = "a minute ago"
englishMessage (TimeMinutesAgo time)    = time `mappend` " minutes ago"
englishMessage TimeOneHourAgo           = "an hour ago"
englishMessage (TimeAboutHoursAgo time) = "about " `mappend` time `mappend` " hours ago"
englishMessage (TimeAt time)            = "At " `mappend` time
englishMessage (TimeDaysAgo time)       = time `mappend` " days ago"
englishMessage (TimeWeekAgo time)       = time `mappend` " week ago"
englishMessage (TimeWeeksAgo time)      = time `mappend` " weeks ago"
englishMessage (TimeOnYear time)        = "on " `mappend` time
englishMessage DayOfWeekFmt             = "%l:%M %p on %A"

dutchMessage :: CoreMessage -> Text
dutchMessage Dashboard                = "Dashboard"
dutchMessage UserIndex                = "Gebruikers overzicht"
dutchMessage LambdaCms                = "LambdaCms"
dutchMessage NotLoggedIn              = "Niet ingelogd"
dutchMessage Logout                   = "Uitloggen"
dutchMessage AccountSettings          = "Accountinstellingen"
dutchMessage EmailAddress             = "E-mailadres"
dutchMessage NewUser                  = "Nieuwe gebruiker"
dutchMessage (EditUser name)          = name
dutchMessage PasswordTooShort         = "Wachtwoord te kort"
dutchMessage PasswordMismatch         = "Wachtwoorden komen niet overeen"
dutchMessage Username                 = "Gebruikersnaam"
dutchMessage Password                 = "Wachtwoord"
dutchMessage Confirm                  = "Bevestig"
dutchMessage Create                   = "Aanmaken"
dutchMessage Save                     = "Opslaan"
dutchMessage Submit                   = "Verzenden"
dutchMessage Change                   = "Aanpassen"
dutchMessage Back                     = "Terug"
dutchMessage Remove                   = "Verwijderen"
dutchMessage CreatedOn                = "Geregistreerd op"
dutchMessage LastLogin                = "Laatst ingelogd"
dutchMessage AccountActivationPending = "In afwachting"
dutchMessage AccountActivationSuccess = "Geactiveerd"
dutchMessage AccountAlreadyActivated  = "Dit account is al geactiveerd"
dutchMessage TokenMismatch            = "Ongeldig token"
dutchMessage NoUsersFound             = "Geen gebruikers gevonden."
dutchMessage SuccessCreate            = "Succesvol aangemaakt"
dutchMessage SuccessReplace           = "Succesvol aangepast"
dutchMessage SuccessUpdate            = "Succesvol geüpdatet"
dutchMessage SuccessChgPwd            = "Wachtwoord succesvol aangepast"
dutchMessage SuccessDelete            = "Succesvol verwijdert"
dutchMessage MenuDashboard            = dutchMessage Dashboard
dutchMessage MenuUsers                = "Gebruikers"
dutchMessage (DeletedUser name)       = "Gebruiker " `mappend` name `mappend` " verwijderd"
dutchMessage TimeJustNow              = "Nu net"
dutchMessage (TimeSecondsAgo time)    = time `mappend` " seconden geleden"
dutchMessage TimeOneMinuteAgo         = "een minuut geleden"
dutchMessage (TimeMinutesAgo time)    = time `mappend` " minuten geleden"
dutchMessage TimeOneHourAgo           = "een uur geleden"
dutchMessage (TimeAboutHoursAgo time) = "ongeveer " `mappend` time `mappend` " uur geleden"
dutchMessage (TimeAt time)            = "Om " `mappend` time
dutchMessage (TimeDaysAgo time)       = time `mappend` " dagen geleden"
dutchMessage (TimeWeekAgo time)       = time `mappend` " week geleden"
dutchMessage (TimeWeeksAgo time)      = time `mappend` " weken geleden"
dutchMessage (TimeOnYear time)        = "in " `mappend` time
dutchMessage DayOfWeekFmt             = "%H:%M op %A"
