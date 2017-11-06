{-# LANGUAGE OverloadedStrings #-}

module Leaflet
  ( Leaflet(..)
  , MapLayer
  , LatLng(..)
  , Point(..)
  , newLeaflet
  , leafletSetView
  , newLayerMapBox
  , newLayerMapnik
  , addLayer
  , removeLayer
  , setBoundsToLayer
  , bindTooltip
  , leafletGetZoom
  , leafletOnClick
  , leafletOnZoomEnd
  , LeafletMouseEvent(..)
  , getFeatureName
  , MapIcon
  , newIcon
  , newLayerGroup
  , newLayerMarker
  , bindPopup
  , geoLatLng
  , newLayerGeoJSON
  , setLayerStyle
  , newLayersControl
  ) where

import Data.Aeson -- (Value, eitherDecodeStrict, FromJSON(..), ToJSON(..), pairs)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (typeMismatch, parseMaybe)

import GHCJS.Types (JSString, JSVal, nullRef)
import qualified Data.Vector as V
import qualified Data.Map as M
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Nullable
import qualified JavaScript.Object as Object
import qualified GHCJS.Foreign as F
import qualified GHCJS.Foreign.Callback as F
import           Unsafe.Coerce
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Naqsha.Geometry (Geo(..), toDegree, Angular(..))

import Util
-- import TourJson

-- fixme: replace latlng with geo
geoLatLng :: Geo -> LatLng
geoLatLng (Geo lat lng) = LatLng (n lat) (n lng)
  where
    n :: Angular a => a -> Double
    n = toDegree . toAngle

newtype Leaflet = Leaflet { unLeaflet :: JSVal }
newtype MapLayer = MapLayer { unMapLayer :: JSVal }

foreign import javascript unsafe "L.map($1)"
  newLeaflet :: JSString -> IO Leaflet

foreign import javascript unsafe "$4.setView([$1, $2], $3)"
  leafletSetView :: Double -> Double -> Int -> Leaflet -> IO ()

foreign import javascript unsafe "L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token={accessToken}', { attribution: 'Map data &copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>, Imagery © <a href=\"http://mapbox.com\">Mapbox</a>', maxZoom: 18, id: 'mapbox.streets', accessToken: $1 })"
  newLayerMapBox :: JSString -> IO MapLayer

foreign import javascript unsafe "L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', { attribution: 'Map data &copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors' })"
  newLayerMapnik :: IO MapLayer

foreign import javascript unsafe "$1.addLayer($2)"
  addLayer :: Leaflet -> MapLayer -> IO ()

foreign import javascript unsafe "$1.removeLayer($2)"
  removeLayer :: Leaflet -> MapLayer -> IO ()

foreign import javascript unsafe "(function() { var b = $2.getBounds(); if (b.isValid()) { $1.fitBounds(b) }; })()"
  setBoundsToLayer :: Leaflet -> MapLayer -> IO ()

foreign import javascript unsafe "$1.bindTooltip(function(layer) { return $2(layer.feature.properties.name); }, { sticky: true })"
  js_bindTooltip :: MapLayer -> F.Callback (JSVal -> IO JSVal) -> IO ()

bindTooltip :: MapLayer -> (JSString -> JSString) -> IO ()
bindTooltip layer tooltip = do
  cb <- F.syncCallback1' (pure . unsafeCoerce . tooltip . unsafeCoerce)
  js_bindTooltip layer cb

foreign import javascript unsafe "L.control.layers($3, {}, { position: $1 }).addTo($2)"
  js_newLayersControl :: JSString -> Leaflet -> Object.Object -> IO JSVal

newLayersControl :: Leaflet -> [(JSString, MapLayer)] -> IO JSVal
newLayersControl leaflet layers = do
  obj <- Object.create
  mapM_ (\(k, v) -> Object.setProp k (unMapLayer v) obj) layers
  js_newLayersControl "topleft" leaflet obj

foreign import javascript unsafe "Math.floor($1.getZoom())"
  leafletGetZoom :: Leaflet -> IO Int

-- event object contains reference cycles, which breaks marshalling,
-- so just pick out the properties we need.
foreign import javascript unsafe "$3.on($1, function (ev) { var r = ['latlng', 'layerPoint', 'containerPoint', 'type'].reduce(function(o, k) { o[k] = ev[k]; return o; }, {}); r.featureProperties = ev.layer && ev.layer.feature && ev.layer.feature ? ev.layer.feature.properties : undefined; $2(r); })"
  js_leafletOn :: JSString -> F.Callback (JSVal -> IO ()) -> JSVal -> IO JSVal

data LatLng = LatLng Double Double deriving (Show, Eq)
data Point = Point Double Double deriving (Show, Eq)

data LeafletMouseEvent = LeafletMouseEvent
                         { evLatLng :: LatLng
                         , evLayerPoint :: Point
                         , evContainerPoint :: Point
                         , evType :: String
                         , evFeatureProperties :: Maybe Value
                         } deriving (Show)

getFeatureName :: Value -> Maybe String
getFeatureName = parseMaybe (withObject "properties object" (.: "name"))

instance FromJSON LeafletMouseEvent where
  parseJSON (Object v) = LeafletMouseEvent <$>
                         (v .: "latlng") <*>
                         (v .: "layerPoint") <*>
                         (v .: "containerPoint") <*>
                         (v .: "type") <*>
                         (v .:? "featureProperties")
  parseJSON v          = typeMismatch "MouseEvent" v

instance FromJSON LatLng where
  parseJSON (Object v) = LatLng <$> (v .: "lat") <*> (v .: "lng")
  parseJSON v          = typeMismatch "LatLng" v

instance FromJSON Point where
  parseJSON (Object v) = Point <$> (v .: "x") <*> (v .: "y")
  parseJSON v          = typeMismatch "Point" v

instance FromJSVal LeafletMouseEvent where
  fromJSVal = fmap (>>= may . fromJSON) . fromJSVal
    where
      may (Success a) = Just a
      may (Error _) = Nothing

instance ToJSON LatLng where
  toJSON (LatLng lat lng) = object ["lat" .= lat, "lng" .= lng]

class Clickable a where
  clickableVal :: a -> JSVal
instance Clickable Leaflet where
  clickableVal = unLeaflet
instance Clickable MapLayer where
  clickableVal = unMapLayer

leafletOnClick :: Clickable a => (LeafletMouseEvent -> IO ()) -> a -> IO ()
leafletOnClick handler l = do
  onMapClick <- F.syncCallback1 F.ContinueAsync $ \ev -> do
    evv <- fromJSVal ev
    case evv of
      Just val -> case fromJSON val of
        Success mev -> handler mev
        Error e -> putStrLn $ "leafletOnClick problem: " ++ e
      Nothing -> return ()
  js_leafletOn "click" onMapClick (clickableVal l)
  return ()

leafletOnZoomEnd :: (IO ()) -> Leaflet -> IO ()
leafletOnZoomEnd handler leaflet = do
  onZoomEnd <- F.asyncCallback1 (const handler)
  js_leafletOn "zoomend" onZoomEnd (unLeaflet leaflet)
  return ()

newtype MapIcon = MapIcon JSVal

foreign import javascript unsafe "L.marker($4, { icon: $1, title: $2, zIndexOffset: $3 })"
  js_newLayerMarker :: MapIcon -> JSString -> Nullable Int -> JSVal -> IO MapLayer

newLayerMarker :: MapIcon -> JSString -> Maybe Int -> LatLng -> IO MapLayer
newLayerMarker i t z l = toJSVal_aeson l >>= js_newLayerMarker i t (maybeToNullable z)

foreign import javascript unsafe "L.layerGroup($1)"
  js_newLayerGroup :: JSVal -> IO MapLayer

newLayerGroup :: [MapLayer] -> IO MapLayer
newLayerGroup ms = toJSVal (map unMapLayer ms) >>= js_newLayerGroup

foreign import javascript unsafe "$1.bindPopup($2)"
  bindPopup :: MapLayer -> JSString -> IO ()

foreign import javascript unsafe "L.icon({ iconUrl: $1, iconRetinaUrl: $2, iconSize: [25, 41], iconAnchor: [13, 41], popupAnchor: [0, -10] })"
  js_newIcon :: JSString -> JSString -> IO MapIcon

newIcon :: JSString -> JSString -> IO MapIcon
newIcon staticUrl colour = js_newIcon iconUrl iconRetinaUrl
  where iconUrl = images <> "marker-icon-" <> colour <> ".png"
        iconRetinaUrl = images <> "marker-icon-2x-" <> colour <> ".png"
        images = staticUrl <> "images/"

foreign import javascript unsafe "L.geoJSON($1)"
  newLayerGeoJSON :: JSVal -> IO MapLayer

foreign import javascript unsafe "$3.setStyle(function(f) { return { weight: 3, color: $1, opacity: 1, stroke: $2(f.properties.name) }; })"
  js_setLayerStyle :: JSString -> F.Callback (JSVal -> IO JSVal) -> MapLayer -> IO ()

-- fixme: use type for path style
setLayerStyle :: JSString -> (JSString -> Bool) -> MapLayer -> IO ()
setLayerStyle colour stroke layer = do
  cb <- F.syncCallback1' (toJSVal . stroke . unsafeCoerce)
  js_setLayerStyle colour cb layer
