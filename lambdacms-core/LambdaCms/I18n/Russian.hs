module LambdaCms.I18n.Russian where

import           Data.Time.Format (TimeLocale (..))
import           Data.Time.Format.Human (HumanTimeLocale (..))
import           LambdaCms.Core.Message (CoreMessage (..))
import           LambdaCms.I18n.Default (defaultHumanTimeLocale)


-- | Russian time locale.
russianTimeLocale :: TimeLocale
russianTimeLocale =  TimeLocale
    {
        wDays  = [("Воскресенье", "Вс"),  ("Понедельник", "Пн"),
                  ("Вторник",     "Вт"),  ("Среда",       "Ср"),
                  ("Четверг",     "Чт"),  ("Пятница",     "Пт"),
                  ("Суббота",     "Сб")],

        months = [("Январь",   "Янв"), ("Февраль", "Фев"),
                  ("Март",     "Мар"), ("Апрель",  "Апр"),
                  ("Май",      "Май"), ("Июнь",    "Июн"),
                  ("Июль",     "Июл"), ("Август",  "Авг"),
                  ("Сентябрь", "Сен"), ("Октябрь", "Окт"),
                  ("Ноябрь",   "Ноя"), ("Декабрь", "Дек")],

        amPm = ("am", "pm"),
        dateTimeFmt = "%a, %e %b %Y %H:%M:%S %Z",
        dateFmt = "%d.%m.%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%H:%M:%S",
        knownTimeZones = []
    }

-- | Russian 'HumanTimeLocale'
russianHumanTimeLocale :: (CoreMessage -> String) -> HumanTimeLocale
russianHumanTimeLocale r = (defaultHumanTimeLocale r)
    { locale = russianTimeLocale
    , thisYearFmt = "%e %b"
    , prevYearFmt = "%e %b %Y"}
