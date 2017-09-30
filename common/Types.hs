{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( Tour(..)
  , TourDay(..)
  , tourSummary
  , tourDates
  , tourDates'
  , formatDate
  , dashesDate
  , undashesDate
  , renumber
  , ElevPoint(..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.Clock.POSIX (POSIXTime)
import Naqsha.Geometry
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intercalate)

data TourDay = TourDay
    { dayNum  :: Int
    , dayDate  :: Day
    , dayStart :: Maybe TimeOfDay
    , dayEnd   :: Maybe TimeOfDay
    , dayFrom  :: Text
    , dayTo    :: Text
    , dayFromCoord :: Maybe Geo
    , dayToCoord   :: Maybe Geo
    , dayDist  :: Int
    } deriving (Generic, Show, Eq)

data Tour = Tour
    { tourName  :: Text
    , tourDescription :: Text
    , tourDays  :: [TourDay]
    , tourStart :: Maybe Day
    , tourEnd  :: Maybe Day
    , tourCountries :: [Text]
    } deriving (Generic, Show, Eq)

tourDates :: Tour -> [Day]
-- tourDates Tour{..} = catMaybes $ map (parseDate . dayDate) tourDays
tourDates = map dayDate . tourDays

tourDates' :: Tour -> [String]
tourDates' = map formatDate . tourDates

parseDate :: Text -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

dashesDate :: String -> String
dashesDate d = intercalate "-" [y, m, d']
  where
    (y, y') = splitAt 4 d
    (m, m') = splitAt 2 y'
    (d', _)  = splitAt 2 m'

undashesDate :: String -> String
undashesDate = filter (/= '-')

renumber :: [TourDay] -> [TourDay]
renumber = id

parseTimeOfDay :: Monad m => String -> m TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%l:%M"

tourSummary :: [(String, Tour)] -> Map String Tour
tourSummary = M.fromList . map (\(n, t) -> (n, t { tourDays = []}))

data ElevPoint = ElevPoint
                 { ptElev :: Double
                 , ptDist :: Double
                 , ptTime :: POSIXTime
                 } deriving (Generic, Show)
