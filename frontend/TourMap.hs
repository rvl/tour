{-# LANGUAGE TemplateHaskell    #-}

module TourMap where

import Control.Lens
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale, iso8601DateFormat)
import Miso
import Miso.String (MisoString, toMisoString)
import Data.Maybe (fromJust)

import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types (JSVal)
import qualified Data.JSString as JS

import Leaflet
import App
import AppTypes

instance PToJSVal Day where
  pToJSVal = pToJSVal . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

instance PFromJSVal Day where
  pFromJSVal = fromJust . parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . pFromJSVal

instance ToJSVal Day where
  toJSVal = pure . pToJSVal

instance FromJSVal Day where
  fromJSVal = pure . pFromJSVal

data MapState = MapState
                { _msView       :: RouteView
                , _msLayerAll   :: Maybe MapLayer
                , _msLayerTour  :: Maybe MapLayer
                , _msLayerDaily :: Maybe MapLayer
                , _msInfoTour   :: Maybe MapLayer
                , _msInfoDaily  :: [(Day, MapLayer)]
                }

makeLenses ''MapState
