{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( Tour(..)
  , TourDay(..)
  , tourSummary
  , tourDates
  , tourDates'
  , formatISODate
  , parseISODate
  , dashesDate
  , undashesDate
  , fillTour
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
import Network.URI (URI)
import Control.Applicative
import Data.Traversable (mapAccumL)

data TourDay = TourDay
    { dayNum       :: Int
    , dayDate      :: Day
    , dayStart     :: Maybe TimeOfDay
    , dayEnd       :: Maybe TimeOfDay
    , dayFrom      :: Text
    , dayTo        :: Text
    , dayFromCoord :: Maybe Geo
    , dayToCoord   :: Maybe Geo
    , dayDist      :: Int
    , dayBlog      :: Maybe URI
    } deriving (Generic, Show, Eq, Ord)

deriving instance Ord Geo

data Tour = Tour
    { tourName  :: Text
    , tourDescription :: Text
    , tourDays  :: [TourDay]
    , tourStart :: Maybe Day
    , tourEnd  :: Maybe Day
    , tourCountries :: [Text]
    } deriving (Generic, Show, Eq)

tourDates :: Tour -> [Day]
tourDates = map dayDate . tourDays

tourDates' :: Tour -> [String]
tourDates' = map formatISODate . tourDates

parseISODate :: Text -> Maybe Day
parseISODate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

formatISODate :: Day -> String
formatISODate = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

dashesDate :: String -> String
dashesDate d = intercalate "-" [y, m, d']
  where
    (y, y') = splitAt 4 d
    (m, m') = splitAt 2 y'
    (d', _)  = splitAt 2 m'

undashesDate :: String -> String
undashesDate = filter (/= '-')

parseTimeOfDay :: Monad m => String -> m TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%l:%M"

tourSummary :: [(String, Tour)] -> Map String Tour
tourSummary = M.fromList . map (\(n, t) -> (n, t { tourDays = []}))

data ElevPoint = ElevPoint
                 { ptElev :: Double
                 , ptDist :: Double
                 , ptTime :: POSIXTime
                 } deriving (Generic, Show, Eq)

-- | Fill in values missing from the YAML
fillTour :: Tour -> Tour
fillTour t = t { tourDays = (renumber . fillFrom . fillFromCoord . tourDays $ t) }
  where
    renumber = snd . mapAccumL renumber' 0
    fillFrom = snd . mapAccumL fillFrom' ""
    fillFromCoord = snd . mapAccumL fillFromCoord' Nothing
    -- fill in zero day numbers by incrementing previous day number
    renumber' p d = (n, d { dayNum = n })
      where
        n | dayNum d /= 0 = dayNum d
          | otherwise     = p + 1
    -- fill in "from" location by taking previous "to" location
    fillFrom' p d = (to, d')
      where
        d' | T.null (dayFrom d) = d { dayFrom = p }
           | otherwise = d
        to | T.null (dayTo d) = dayFrom d'
           | otherwise = dayTo d
    -- fill in "from" coordinate by taking previous "to"
    fillFromCoord' p d = (dayToCoord d <|> from, d { dayFromCoord = from })
      where from = dayFromCoord d <|> p
