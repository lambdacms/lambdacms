module LambdaCms.I18n.Dutch where

import           Data.Time.Format (TimeLocale(..))


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

        amPm = ("", ""),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%d-%m-%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%H:%M:%S",
        knownTimeZones = []
    }
