module LambdaCms.I18n.Russian where

import           Data.Time.Format (TimeLocale(..))


-- | Dutch time locale.
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
