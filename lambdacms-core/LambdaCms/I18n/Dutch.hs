module LambdaCms.I18n.Dutch where

import           Data.Time.Format (TimeLocale(..))
import           Data.Time.Format.Human (HumanTimeLocale (..))
import           LambdaCms.Core.Message (CoreMessage (..))
import           LambdaCms.I18n.Default (defaultHumanTimeLocale)


-- | Dutch time locale.
dutchTimeLocale :: TimeLocale
dutchTimeLocale =  TimeLocale
    {
        wDays  = [("Zondag",    "Zo"),  ("Maandag",  "Ma"),
                  ("Dinsdag",   "Di"),  ("Woensdag", "Wo"),
                  ("Donderdag", "Do"),  ("Vrijdag",  "Vr"),
                  ("Zaterdag",  "Za")],

        months = [("Januari",   "Jan"), ("Februari",  "Feb"),
                  ("Maart",     "Mar"), ("April",     "Apr"),
                  ("Mei",       "Mei"), ("Juni",      "Jun"),
                  ("Juli",      "Jul"), ("Augustus",  "Aug"),
                  ("September", "Sep"), ("Oktober",   "Okt"),
                  ("November",  "Nov"), ("December",  "Dec")],

        amPm = ("am", "pm"),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%d-%m-%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%H:%M:%S",
        knownTimeZones = []
    }

-- | Dutch 'HumanTimeLocale'
dutchHumanTimeLocale :: (CoreMessage -> String) -> HumanTimeLocale
dutchHumanTimeLocale r = (defaultHumanTimeLocale r) { locale = dutchTimeLocale }
