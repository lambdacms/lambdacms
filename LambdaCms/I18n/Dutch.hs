module LambdaCms.I18n.Dutch where

import           System.Locale

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

        intervals = [ ("jaar",  "jaren")
                    , ("maand", "maanden")
                    , ("dag",   "dagen")
                    , ("uur",   "uren")
                    , ("min",   "mins")
                    , ("sec",   "secs")
                    , ("usec",  "usecs")
                    ],

        amPm = ("", ""),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%d-%m-%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%H:%M:%S"
    }
