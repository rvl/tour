{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module TourJson where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Control.Monad (forM)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Vector as V
import Naqsha.Geometry
import Data.Scientific (toRealFloat)
import Network.URI (URI(..), parseURI)

import Types

prefixOptions :: Options
prefixOptions = defaultOptions { fieldLabelModifier = drop 1 . dropWhile (/= '_') . camel }

instance ToJSON TourDay where
  toJSON = genericToJSON prefixOptions

instance ToJSON Tour where
  toJSON = genericToJSON prefixOptions

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
                         v .:? "dist" .!= 0 <*>
                         v .:? "blog"

  parseJSON (String d) = do
    d' <- parseJSON (String d)
    return $ TourDay 0 d' Nothing Nothing "" "" Nothing Nothing 0 Nothing

  -- A non-Object value is of the wrong type, so fail.
  parseJSON v          = typeMismatch "TourDay" v

instance FromJSON Geo where
  parseJSON (Object v) = Geo <$> v .: "lat" <*> v .: "lng"
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

instance ToJSON ElevPoint where
  toJSON (ElevPoint e s t) = object ["ele" .= e, "dist" .= s, "time" .= t]

instance FromJSON ElevPoint where
  parseJSON (Object v) = ElevPoint
                         <$> v .: "ele"
                         <*> v .: "dist"
                         <*> v .: "time"
  parseJSON v = typeMismatch "ElevPoint" v

instance ToJSON URI where
  toJSON = String . T.pack . show

instance FromJSON URI where
  parseJSON (String u) = case parseURI (T.unpack u) of
                           Just uri -> pure uri
                           Nothing  -> fail "Not a valid URI"

----------------------------------------------------------------------------

#if !MIN_VERSION_aeson(1,0,0)
instance ToJSON Day where
  toJSON _ = object []
instance ToJSON TimeOfDay where
  toJSON _ = object []
instance ToJSON NominalDiffTime where
  toJSON _ = object []
instance FromJSON Day where
  parseJSON _ = undefined
instance FromJSON TimeOfDay where
  parseJSON _ = undefined
instance FromJSON NominalDiffTime where
  parseJSON _ = undefined

camel = camelTo '_'
#else
camel = camelTo2 '_'
#endif
