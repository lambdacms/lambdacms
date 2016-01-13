{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module LambdaCms.Core.Message
       ( CoreMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       , dutchMessage
       , russianMessage
       ) where

import           Data.Monoid ((<>))
import           Data.Text   (Text, unpack)
import           Text.Read   (readMaybe)


data CoreMessage =
    Dashboard
  | LambdaCms
  | NotLoggedIn
  | You
  | Logout
  | AccountSettings
  | UserIndex
  | EmailAddress
  | NewUser
  | EditUser { name :: Text }
  | ChangeAccountSettings
  | ChangeRoles
  | ChangePassword
  | ResetPassword
  | RequestResetToken_Text
  | RequestResetToken_Button
  | PasswordResetTokenSend
  | PasswordTooShort
  | PasswordMismatch
  | Username
  | Password
  | Roles
  | Confirm
  | Create
  | Save
  | Submit
  | Change
  | Back
  | BackHome
  | Remove
  | Deactivate
  | Activate
  | UserDeactivated
  | UserActivated
  | UserStillPending
  | CreatedOn
  | LastLogin
  | AccountStatus
  | AccountPending
  | AccountActive
  | AccountInactive
  | AccountAlreadyActivated
  | ActivationSuccess
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
  | ActionLogIndex
  | NoActionLogsFound
  | LogCreatedUser { name :: Text }
  | LogUpdatedUser { name :: Text }
  | LogDeletedUser { name :: Text}
  | LogChangedPasswordUser { name :: Text }
  | LogRequestedPasswordUser { name :: Text }
  | LogDeactivatedUser { name :: Text }
  | LogActivatedUser { name :: Text }
  | AllLogs
  | PersonalLogs
  | InvalidOffset
  | InvalidLimit
  | LoadMore
  | WelcomeTitle { name :: Text }
  | WelcomeIntro { site :: Text }
  | WelcomeMenu
  | WelcomeInfo

defaultMessage :: CoreMessage -> Text
defaultMessage = englishMessage

englishMessage :: CoreMessage -> Text
englishMessage Dashboard                       = "Dashboard"
englishMessage LambdaCms                       = "LambdaCms"
englishMessage NotLoggedIn                     = "Not logged in"
englishMessage You                             = "You"
englishMessage Logout                          = "Logout"
englishMessage AccountSettings                 = "Account settings"
englishMessage UserIndex                       = "User overview"
englishMessage EmailAddress                    = "E-mail address"
englishMessage NewUser                         = "New user"
englishMessage (EditUser name)                 = name
englishMessage ChangeAccountSettings           = "Modify account settings"
englishMessage ChangeRoles                     = "Select roles"
englishMessage ChangePassword                  = "Change password"
englishMessage ResetPassword                   = "Reset password"
englishMessage RequestResetToken_Text          = "Reset this user's password and send him/her an email with a new verification token"
englishMessage RequestResetToken_Button        = "Request password reset"
englishMessage PasswordResetTokenSend          = "Password reset link send"
englishMessage PasswordTooShort                = "Password is too short"
englishMessage PasswordMismatch                = "Passwords don't match"
englishMessage Username                        = "Username"
englishMessage Password                        = "Password"
englishMessage Roles                           = "Roles"
englishMessage Confirm                         = "Confirm"
englishMessage Create                          = "Create"
englishMessage Save                            = "Save"
englishMessage Submit                          = "Submit"
englishMessage Change                          = "Change"
englishMessage Back                            = "Back"
englishMessage BackHome                        = "back to homepage"
englishMessage Remove                          = "Remove"
englishMessage Deactivate                      = "Deactivate"
englishMessage Activate                        = "Activate"
englishMessage UserDeactivated                 = "User deactivated"
englishMessage UserActivated                   = "User activated"
englishMessage UserStillPending                = "Can't perform action user is still pending"
englishMessage CreatedOn                       = "Created on"
englishMessage LastLogin                       = "Last login"
englishMessage AccountStatus                   = "Account status"
englishMessage AccountPending                  = "Pending"
englishMessage AccountActive                   = "Active"
englishMessage AccountInactive                 = "Inactive"
englishMessage AccountAlreadyActivated         = "This account is already activated"
englishMessage ActivationSuccess               = "Account successfully activated"
englishMessage TokenMismatch                   = "Invalid token"
englishMessage NoUsersFound                    = "No users found."
englishMessage SuccessCreate                   = "Successfully created"
englishMessage SuccessReplace                  = "Successfully replaced"
englishMessage SuccessUpdate                   = "Successfully updated"
englishMessage SuccessChgPwd                   = "Successfully changed password"
englishMessage SuccessDelete                   = "Successfully deleted"
englishMessage MenuDashboard                   = englishMessage Dashboard
englishMessage MenuUsers                       = "Users"
englishMessage (DeletedUser name)              = "User " `mappend` name `mappend` " deleted"
englishMessage TimeJustNow                     = "Just now"
englishMessage (TimeSecondsAgo time)           = time `mappend` " seconds ago"
englishMessage TimeOneMinuteAgo                = "a minute ago"
englishMessage (TimeMinutesAgo time)           = time `mappend` " minutes ago"
englishMessage TimeOneHourAgo                  = "an hour ago"
englishMessage (TimeAboutHoursAgo time)        = "about " `mappend` time `mappend` " hours ago"
englishMessage (TimeAt time)                   = "at " `mappend` time
englishMessage (TimeDaysAgo time)              = time `mappend` " days ago"
englishMessage (TimeWeekAgo time)              = time `mappend` " week ago"
englishMessage (TimeWeeksAgo time)             = time `mappend` " weeks ago"
englishMessage (TimeOnYear time)               = "on " `mappend` time
englishMessage DayOfWeekFmt                    = "%l:%M %p on %A"
englishMessage ActionLogIndex                  = "Activities"
englishMessage NoActionLogsFound               = "No activities found"
englishMessage (LogCreatedUser name)           = "Created new user \"" `mappend` name `mappend` "\""
englishMessage (LogUpdatedUser name)           = "Updated user \"" `mappend` name `mappend` "\""
englishMessage (LogDeletedUser name)           = "Deleted user \"" `mappend` name `mappend` "\""
englishMessage (LogChangedPasswordUser name)   = "Changed password for user \"" `mappend` name `mappend` "\""
englishMessage (LogRequestedPasswordUser name) = "Send a password reset token to user \"" `mappend` name `mappend` "\""
englishMessage (LogDeactivatedUser name)       = "Deactivated user \"" `mappend` name `mappend` "\""
englishMessage (LogActivatedUser name)         = "Activated user \"" `mappend` name `mappend` "\""
englishMessage AllLogs                         = "All activities"
englishMessage PersonalLogs                    = "Your activities"
englishMessage InvalidOffset                   = "Invalid value for \"offset\""
englishMessage InvalidLimit                    = "Invalid value for \"limit\""
englishMessage LoadMore                        = "load more"
englishMessage (WelcomeTitle name)             = "Welcome, " `mappend` name `mappend` "!"
englishMessage (WelcomeIntro site)             = "You're viewing the backend interface of " `mappend` site `mappend` ", which is build using LambdaCms."
englishMessage WelcomeMenu                     = "With the menu on the left you can navigate the admin interface and manage this website."
englishMessage WelcomeInfo                     = "For more information about LambdaCms please visit <a href='http://www.lambdacms.org'>www.lambdacms.org</a>."

dutchMessage :: CoreMessage -> Text
dutchMessage Dashboard                       = "Dashboard"
dutchMessage UserIndex                       = "Gebruikers overzicht"
dutchMessage LambdaCms                       = "LambdaCms"
dutchMessage NotLoggedIn                     = "Niet ingelogd"
dutchMessage You                             = "Jij"
dutchMessage Logout                          = "Uitloggen"
dutchMessage AccountSettings                 = "Accountinstellingen"
dutchMessage EmailAddress                    = "E-mailadres"
dutchMessage NewUser                         = "Nieuwe gebruiker"
dutchMessage (EditUser name)                 = name
dutchMessage ChangeAccountSettings           = "Pas accountinstellingen aan"
dutchMessage ChangeRoles                     = "Rollen selecteren"
dutchMessage ChangePassword                  = "Wachtwoord wijzigen"
dutchMessage ResetPassword                   = "Wachtwoord wijzigen"
dutchMessage RequestResetToken_Text          = "Reset het wachtwoord van deze gebruiker en stuur hem/haar een email met een nieuw verificatie-token"
dutchMessage RequestResetToken_Button        = "Aanvraag doen om wachtwoord te wijzigen"
dutchMessage PasswordResetTokenSend          = "Link om wachtwoord te wijzigen verzonden"
dutchMessage PasswordTooShort                = "Wachtwoord te kort"
dutchMessage PasswordMismatch                = "Wachtwoorden komen niet overeen"
dutchMessage Username                        = "Gebruikersnaam"
dutchMessage Password                        = "Wachtwoord"
dutchMessage Roles                           = "Rollen"
dutchMessage Confirm                         = "Bevestig"
dutchMessage Create                          = "Aanmaken"
dutchMessage Save                            = "Opslaan"
dutchMessage Submit                          = "Verzenden"
dutchMessage Change                          = "Aanpassen"
dutchMessage Back                            = "Terug"
dutchMessage BackHome                        = "terug naar homepagina"
dutchMessage Remove                          = "Verwijderen"
dutchMessage Deactivate                      = "Deactiveren"
dutchMessage Activate                        = "Activeren"
dutchMessage UserDeactivated                 = "Gebruiker gedeactifeerd"
dutchMessage UserActivated                   = "Gebruiker geactiveerd"
dutchMessage UserStillPending                = "Kan actie niet voltooien omdat de gebruiker nog in afwachting is"
dutchMessage CreatedOn                       = "Geregistreerd op"
dutchMessage LastLogin                       = "Laatst ingelogd"
dutchMessage AccountStatus                   = "Account status"
dutchMessage AccountPending                  = "In afwachting"
dutchMessage AccountActive                   = "Actief"
dutchMessage AccountInactive                 = "Inactief"
dutchMessage AccountAlreadyActivated         = "Dit account is al geactiveerd"
dutchMessage ActivationSuccess               = "Account succesvol geactiveerd"
dutchMessage TokenMismatch                   = "Ongeldig token"
dutchMessage NoUsersFound                    = "Geen gebruikers gevonden."
dutchMessage SuccessCreate                   = "Succesvol aangemaakt"
dutchMessage SuccessReplace                  = "Succesvol aangepast"
dutchMessage SuccessUpdate                   = "Succesvol geüpdatet"
dutchMessage SuccessChgPwd                   = "Wachtwoord succesvol aangepast"
dutchMessage SuccessDelete                   = "Succesvol verwijdert"
dutchMessage MenuDashboard                   = dutchMessage Dashboard
dutchMessage MenuUsers                       = "Gebruikers"
dutchMessage (DeletedUser name)              = "Gebruiker " `mappend` name `mappend` " verwijderd"
dutchMessage TimeJustNow                     = "Nu net"
dutchMessage (TimeSecondsAgo time)           = time `mappend` " seconden geleden"
dutchMessage TimeOneMinuteAgo                = "een minuut geleden"
dutchMessage (TimeMinutesAgo time)           = time `mappend` " minuten geleden"
dutchMessage TimeOneHourAgo                  = "een uur geleden"
dutchMessage (TimeAboutHoursAgo time)        = "ongeveer " `mappend` time `mappend` " uur geleden"
dutchMessage (TimeAt time)                   = "om " `mappend` time
dutchMessage (TimeDaysAgo time)              = time `mappend` " dagen geleden"
dutchMessage (TimeWeekAgo time)              = time `mappend` " week geleden"
dutchMessage (TimeWeeksAgo time)             = time `mappend` " weken geleden"
dutchMessage (TimeOnYear time)               = "in " `mappend` time
dutchMessage DayOfWeekFmt                    = "%H:%M op %A"
dutchMessage ActionLogIndex                  = "Activiteiten"
dutchMessage NoActionLogsFound               = "Geen activiteiten gevonden"
dutchMessage (LogCreatedUser name)           = "Heeft gebruiker \"" `mappend` name `mappend` "\" aangemaakt"
dutchMessage (LogUpdatedUser name)           = "Heeft gebruiker \"" `mappend` name `mappend` "\" aangepast"
dutchMessage (LogDeletedUser name)           = "Heeft gebruiker \"" `mappend` name `mappend` "\" verwijderd"
dutchMessage (LogChangedPasswordUser name)   = "Heeft het wachtwoord van gebruiker \"" `mappend` name `mappend` "\" aangepast"
dutchMessage (LogRequestedPasswordUser name) = "Heeft een wachtwoord reset token verzonden naar gebruiker \"" `mappend` name `mappend` "\""
dutchMessage (LogDeactivatedUser name)       = "Heeft gebruiker \"" `mappend` name `mappend` "\" gedeactiveerd"
dutchMessage (LogActivatedUser name)         = "Heeft gebruiker \"" `mappend` name `mappend` "\" geactiveerd"
dutchMessage AllLogs                         = "Alle activiteiten"
dutchMessage PersonalLogs                    = "Jouw activiteiten"
dutchMessage InvalidOffset                   = "Ongeldige waarde voor \"offset\""
dutchMessage InvalidLimit                    = "Ongeldige waarde voor \"limit\""
dutchMessage LoadMore                        = "meer laden"
dutchMessage (WelcomeTitle name)             = "Welkom, " `mappend` name `mappend` "!"
dutchMessage (WelcomeIntro site)             = "Je bekijkt de beheeromgeving van " `mappend` site `mappend` ", die gebouwd is met behulp van LambdaCms."
dutchMessage WelcomeMenu                     = "Met het menu aan de linker kant kan je navigeren door de beheeromgeving om deze website te beheren."
dutchMessage WelcomeInfo                     = "Voor meer informatie over LambdaCms kan je terecht op <a href='http://www.lambdacms.org'>www.lambdacms.org</a>."

russianMessage :: CoreMessage -> Text
russianMessage Dashboard                       = "Панель инструментов"
russianMessage LambdaCms                       = "LambdaCms"
russianMessage NotLoggedIn                     = "Вход не выполнен"
russianMessage You                             = "Вы"
russianMessage Logout                          = "Выход"
russianMessage AccountSettings                 = "Настройки учётной записи"
russianMessage UserIndex                       = "Просмотр пользователей"
russianMessage EmailAddress                    = "Адрес эл.почты"
russianMessage NewUser                         = "Новый пользователь"
russianMessage (EditUser name)                 = name
russianMessage ChangeAccountSettings           = "Изменить настройки учётной записи"
russianMessage ChangeRoles                     = "Выберите роли"
russianMessage ChangePassword                  = "Изменить пароль"
russianMessage ResetPassword                   = "Сбросить пароль"
russianMessage RequestResetToken_Text          = "Сбросить пароль данного пользователя и отправить ему на эл.почту новое проверочное письмо"
russianMessage RequestResetToken_Button        = "Запросить сброс пароля"
russianMessage PasswordResetTokenSend          = "Ссылка для сброса пароля выслана"
russianMessage PasswordTooShort                = "Слишком короткий пароль"
russianMessage PasswordMismatch                = "Пароли не совпадают"
russianMessage Username                        = "Имя пользователя"
russianMessage Password                        = "Пароль"
russianMessage Roles                           = "Роли"
russianMessage Confirm                         = "Подтвердить"
russianMessage Create                          = "Создать"
russianMessage Save                            = "Сохранить"
russianMessage Submit                          = "Отправить"
russianMessage Change                          = "Изменить"
russianMessage Back                            = "Назад"
russianMessage BackHome                        = "на главную страницу"
russianMessage Remove                          = "Удалить"
russianMessage Deactivate                      = "Деактивировать"
russianMessage Activate                        = "Активировать"
russianMessage UserDeactivated                 = "Пользователь деактивирован"
russianMessage UserActivated                   = "Пользователь активирован"
russianMessage UserStillPending                = "Невозможно выполнить действие, пользователь ещё не прошёл проверку"
russianMessage CreatedOn                       = "Создан"
russianMessage LastLogin                       = "Последний вход"
russianMessage AccountStatus                   = "Настройки учётной записи"
russianMessage AccountPending                  = "Ожидает проверки"
russianMessage AccountActive                   = "Активный"
russianMessage AccountInactive                 = "Неактивный"
russianMessage AccountAlreadyActivated         = "Эта учётная запись уже активирована"
russianMessage ActivationSuccess               = "Учётная запись успешно активирована"
russianMessage TokenMismatch                   = "Неверный ключ"
russianMessage NoUsersFound                    = "Пользователей не найдено."
russianMessage SuccessCreate                   = "Создание прошло успешно"
russianMessage SuccessReplace                  = "Замена прошла успешно"
russianMessage SuccessUpdate                   = "Обновление прошло успешно"
russianMessage SuccessChgPwd                   = "Пароль успешно изменён"
russianMessage SuccessDelete                   = "Удаление прошло успешно"
russianMessage MenuDashboard                   = russianMessage Dashboard
russianMessage MenuUsers                       = "Пользователи"
russianMessage (DeletedUser name)              = "Пользователь " `mappend` name `mappend` " удалён"
russianMessage TimeJustNow                     = "Только что"
russianMessage (TimeSecondsAgo time)           = pluralCountRu time RuWordSecond `mappend` ruAgo
russianMessage TimeOneMinuteAgo                = "минуту назад"
russianMessage (TimeMinutesAgo time)           = pluralCountRu time RuWordMinute `mappend` ruAgo
russianMessage TimeOneHourAgo                  = "час назад"
russianMessage (TimeAboutHoursAgo time)        = "около " `mappend` pluralCountRu time RuWordHour `mappend` ruAgo
russianMessage (TimeAt time)                   = "в " `mappend` time
russianMessage (TimeDaysAgo time)              = pluralCountRu time RuWordDay `mappend` ruAgo
russianMessage (TimeWeekAgo time)              = russianMessage (TimeWeeksAgo time)
russianMessage (TimeWeeksAgo time)             = pluralCountRu time RuWordWeek `mappend` ruAgo
russianMessage (TimeOnYear time)               = "в " `mappend` time
russianMessage DayOfWeekFmt                    = "%A, %l:%M %p"
russianMessage ActionLogIndex                  = "Действия"
russianMessage NoActionLogsFound               = "Действий пока не было"
russianMessage (LogCreatedUser name)           = "Создан новый пользователь \"" `mappend` name `mappend` "\""
russianMessage (LogUpdatedUser name)           = "Обновлён пользователь \"" `mappend` name `mappend` "\" "
russianMessage (LogDeletedUser name)           = "Удалён пользователь \"" `mappend` name `mappend` "\""
russianMessage (LogChangedPasswordUser name)   = "Изменён пароль пользователя \"" `mappend` name `mappend` "\""
russianMessage (LogRequestedPasswordUser name) = "Пользователю \"" `mappend` name `mappend` "\" выслан ключ для сброса пароля"
russianMessage (LogDeactivatedUser name)       = "Деактивирован пользователь \"" `mappend` name `mappend` "\""
russianMessage (LogActivatedUser name)         = "Активирован пользователь \"" `mappend` name `mappend` "\""
russianMessage AllLogs                         = "Все события"
russianMessage PersonalLogs                    = "Ваша деятельность"
russianMessage InvalidOffset                   = "Неверное значение сдвига \"offset\""
russianMessage InvalidLimit                    = "Неверное значение ограничения \"limit\""
russianMessage LoadMore                        = "загрузить больше"
russianMessage (WelcomeTitle name)             = "Добро пожаловать, " `mappend` name `mappend` "!"
russianMessage (WelcomeIntro site)             = "Вы просматриваете интерфейс управления сайта " `mappend` site `mappend` ", основанный на LambdaCms."
russianMessage WelcomeMenu                     = "С помощью меню слева вы можете перемещаться по интерфейсу и управлять сайтом."
russianMessage WelcomeInfo                     = "Если вам нужно получить дополнительную информацию о LambdaCms, пожалуйста, посетите <a href='http://www.lambdacms.org'>www.lambdacms.org</a>."

pluralCountRu :: Text -> RussianWord -> Text
pluralCountRu tn w =
    case mayRead of
        Nothing -> pluralCountRu' 0 w -- most common case
        Just  n -> pluralCountRu' n w
 where
   mayRead = readMaybe (unpack tn) :: Maybe Int
   pluralCountRu' :: Int -> RussianWord -> Text
   pluralCountRu' n w
       | n >= 10 && n <= 20   = tn <> " " <> plural0Ru w
       | rem == 1             = tn <> " " <> plural1Ru w
       | rem >= 2 && rem <= 4 = tn <> " " <> plural234Ru w
       | otherwise            = tn <> " " <> plural0Ru w -- 0, 5-9
     where rem = n `mod` 10
   plural1Ru RuWordSecond = "секунду"
   plural1Ru RuWordMinute = "минуту"
   plural1Ru RuWordHour = "час"
   plural1Ru RuWordDay = "день"
   plural1Ru RuWordWeek = "неделю"
   plural234Ru RuWordSecond = "секунды"
   plural234Ru RuWordMinute = "минуты"
   plural234Ru RuWordHour = "часа"
   plural234Ru RuWordDay = "дня"
   plural234Ru RuWordWeek = "недели"
   plural0Ru RuWordSecond = "секунд"
   plural0Ru RuWordMinute = "минут"
   plural0Ru RuWordHour = "часов"
   plural0Ru RuWordDay = "дней"
   plural0Ru RuWordWeek = "недель"

ruAgo :: Text
ruAgo = " назад"

data RussianWord
    = RuWordSecond
    | RuWordMinute
    | RuWordHour
    | RuWordDay
    | RuWordWeek
