{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tour
   ( Tour(..)
   , TourDay(..)
   , tourSummary
   , tourDates
   , tourDates'
   , formatDate
   , dashesDate
   , undashesDate
   ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Aeson
import Data.Aeson.Types (camelTo2, Options(..), Parser(..), typeMismatch)
import Data.Monoid
import Control.Monad (forM)
import Data.Yaml (encodeFile)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Vector as V
import Naqsha.Geometry
import Data.Scientific (toRealFloat)

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
    } deriving (Generic, Show)

data Tour = Tour
    { tourName  :: Text
    , tourDescription :: Text
    , tourDays  :: [TourDay]
    , tourStart :: Maybe Day
    , tourEnd  :: Maybe Day
    , tourCountries :: [Text]
    } deriving (Generic, Show)

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

prefixOptions :: Options
prefixOptions = defaultOptions { fieldLabelModifier = drop 1 . dropWhile (/= '_') . camelTo2 '_' }

instance ToJSON TourDay where
  toJSON = genericToJSON prefixOptions
  toEncoding = genericToEncoding prefixOptions

instance ToJSON Tour where
  toJSON = genericToJSON prefixOptions
  toEncoding = genericToEncoding prefixOptions

instance ToJSON Geo where
  toJSON (Geo lat' lon') = Array (V.fromList [num lon', num lat'])
    where
      num :: Angular n => n -> Value
      num = Number . toDegree . toAngle

instance FromJSON Tour where
    parseJSON (Object v) = Tour <$>
                           v .: "name" <*>
                           v .:? "description" .!= "" <*>
                           (renumber <$> v .: "days") <*>
                           v .:? "start_date" <*>
                           v .:? "end_date" <*>
                           v .:? "countries" .!= []
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mempty

instance FromJSON TourDay where
  parseJSON (Object v) = TourDay <$>
                         v .:? "num" .!= 0 <*>
                         v .: "date" <*>
                         v .:? "start" <*>
                         v .:? "end" <*>
                         v .:? "from" .!= "" <*>
                         v .:? "to" .!= "" <*>
                         v .:? "from_coord" <*>
                         v .:? "to_coord" <*>
                         v .:? "dist" .!= 0

  parseJSON (String d) = do
    d' <- parseJSON (String d)
    return $ TourDay 0 d' Nothing Nothing "" "" Nothing Nothing 0

  -- A non-Object value is of the wrong type, so fail.
  parseJSON v          = typeMismatch "TourDay" v

instance FromJSON Geo where
  parseJSON (Object v) = Geo <$>
                         v .: "lat" <*>
                         v .: "lon"
  parseJSON (Array a) = case V.toList a of
    [lon', lat'] -> Geo <$> parseJSON lon' <*> parseJSON lat'
    _ -> fail "expected length 2 array"
  parseJSON v = typeMismatch "Geo" v


parseCoord :: (Angle -> a) -> Value -> Parser a
parseCoord coord (Number n) = pure . coord . degree . toRational . toRealFloat $ n
parseCoord _ v = typeMismatch "Angle" v

instance FromJSON Latitude where
  parseJSON = parseCoord lat

instance FromJSON Longitude where
  parseJSON = parseCoord lon

renumber :: [TourDay] -> [TourDay]
renumber = id

parseTimeOfDay :: Monad m => String -> m TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%l:%M"

tourSummary :: [(String, Tour)] -> Map String Tour
tourSummary = M.fromList . map (\(n, t) -> (n, t { tourDays = []}))
