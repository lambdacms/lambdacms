module LambdaCms.I18n.Default where

import           Data.Text              (pack)
import           Data.Time              (utc)
import           Data.Time.Format       (defaultTimeLocale)
import           Data.Time.Format.Human (HumanTimeLocale (..))
import           LambdaCms.Core.Message (CoreMessage (..))


defaultHumanTimeLocale :: (CoreMessage -> String) -> HumanTimeLocale
defaultHumanTimeLocale render = HumanTimeLocale
    { justNow       = render TimeJustNow
    , secondsAgo    = \_ x -> render . TimeSecondsAgo $ pack x
    , oneMinuteAgo  = \_   -> render TimeOneMinuteAgo
    , minutesAgo    = \_ x -> render . TimeMinutesAgo $ pack x
    , oneHourAgo    = \_   -> render TimeOneHourAgo
    , aboutHoursAgo = \_ x -> render . TimeAboutHoursAgo $ pack x
    , at            = \_ x -> render . TimeAt $ pack x
    , daysAgo       = \_ x -> render . TimeDaysAgo $ pack x
    , weekAgo       = \_ x -> render . TimeWeekAgo $ pack x
    , weeksAgo      = \_ x -> render . TimeWeeksAgo $ pack x
    , onYear        = render . TimeOnYear . pack
    , locale        = defaultTimeLocale
    , dayOfWeekFmt  = render DayOfWeekFmt
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    , timeZone      = utc
    }
