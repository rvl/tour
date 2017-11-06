{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Main where

import Miso
import Miso.String (MisoString, toMisoString)

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Time.Calendar (Day)
import Data.Maybe (fromMaybe, catMaybes)
import Servant.API ((:<|>)(..))
import Control.Concurrent.MVar
import GHCJS.Types (JSString, JSVal, nullRef)
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Lens hiding (Context(..))
import Control.Monad (forM, forM_, mapM_, when)
import Control.Applicative
import Data.List (find)
import Data.String (fromString)

import Data.JSString (JSString)
import GHCJS.Marshal
import JavaScript.Object (Object(..))
import JavaScript.Object.Internal (getProp, Object(..))
import qualified GHCJS.Foreign as F
import qualified GHCJS.Foreign.Callback as F

import Types
import TourJson
import App
import AppTypes
import Leaflet
import TourMap
import ElevChart
import Util
import Fetch

-- | Main entry point
main :: IO ()
main = do
  cfg <- initConfig <$> getBaseURI
  currentURI <- getCurrentURI
  ref <- newEmptyMVar
  startApp App { model = initModel cfg currentURI
               , update = updateModel ref, ..}
  where
    initialAction = Init
    events = defaultEvents
    subs   = [ uriSub HandleURI
             , layerClickSub HandleLayerClick ]
    view m = either (const $ mainView m the404) (mainView m) $
             runRoute (Proxy :: Proxy ClientRoutes) clientHandlers m

-- | HasURI typeclass instance
-- In next version of miso this could go into common
instance HasURI Model where
  lensURI = makeLens getter setter
    where
      getter = uri
      setter = \m u -> m { uri = u }

-- | Update your model
updateModel :: MVar Context -> Action -> Model -> Effect Action Model
updateModel ref Init m = m <# do
  initContext (config m) ref
  pure NoOp
updateModel _ (HandleURI u) m = Effect (m { uri = unfixURI m u }) (viewHook m)
updateModel _ (ChangeURI u) m = m <# do
  pushURI (fixURI m u)
  pure NoOp
updateModel _ (SetTitle t) m = m <# (setDocumentTitle t >> pure NoOp)
updateModel ref (SetRouteView v) m = (m { routeView = v }) <# do
  ctx <- readMVar ref
  setMapView ctx v
  pure NoOp
updateModel ref (FetchData f) m = m <# (maybe NoOp (SetData f) <$> fetch ref f)
updateModel ref (SetData f a) m = setData ref f a m
updateModel _ ToggleContent m = noEff m { showContent = not (showContent m) }
updateModel _ (HandleLayerClick f) m = m <# (pure $
  case routeViewTour (routeView m) of
    Just tourName -> case featureDay (fromMisoString f) of
      Just d -> goTourDay tourName d
      Nothing -> NoOp
    Nothing -> goTourSummary f)
updateModel _ _ m = noEff m

splitEff :: [action] -> model -> Effect action model
splitEff a m = Effect m (map pure a)

----------------------------------------------------------------------------
-- fetching and setting

fetch :: MVar Context -> FetchThing a -> IO (Maybe a)
fetch ref f = do
  fc <- fetchCache <$> readMVar ref
  (a, fc') <- fetchAsync f fc
  modifyMVar_ ref (\ctx -> pure (ctx { fetchCache = fc' }))
  fetchValue a

setData :: MVar Context -> FetchThing a -> a -> Model -> Effect Action Model
setData ref f a m = update f a m <# (readMVar ref >>= effects f a m)
  where
    -- fixme: use lens
    update :: FetchThing a -> a -> Model -> Model
    update FInfoIndex tours m = m { infoIndex = Just tours }
    update (FInfoTour name) tour m = m { infoTour = M.insert name tour (infoTour m) }
    update (FBlogHtml day) html m = m { blogHtml = M.insert (dayDate day) html (blogHtml m) }
    update FTrackAll geo m = m { trackAll = Just geo }
    update (FTrackTour name) geo m = m { trackTour = M.insert name geo (trackTour m) }
    update (FTrackDaily day) geo m = m { trackDaily = M.insert day geo (trackDaily m) }
    update (FElev day) json m = m { elevDaily = M.insert day json (elevDaily m) }

    effects :: FetchThing a -> a -> Model -> Context -> IO Action
    effects (FInfoTour name) tour m ctx = do
      ctxSetInfoTour ctx tour
      pure $ case routeView m of
        ViewTourDay t d -> case find (\x -> dayDate x == d) (tourDays tour) of
          Just tourDay -> FetchData (FBlogHtml tourDay)
          Nothing -> NoOp
        _ -> NoOp
    effects (FElev day) json _ ctx = do
      updateElevChart ctx (Just (ChartConfig json))
      pure NoOp
    effects (FBlogHtml _) html _ _ = setBlogHtmlElem "section > .blog-html" (fromMaybe "" html) >> pure NoOp
    effects f@FTrackAll       geo m ctx = setMapData ctx f geo m >> pure NoOp
    effects f@(FTrackTour _)  geo m ctx = setMapData ctx f geo m >> pure NoOp
    effects f@(FTrackDaily _) geo m ctx = setMapData ctx f geo m >> pure NoOp
    effects _ _ _ _ = pure NoOp

----------------------------------------------------------------------------
-- actions to run when the route changes

viewHook :: Model -> [IO Action]
viewHook m = map pure (viewHook' m (handle m))
  where
    handle m = either (const Nothing) id $ runRoute (Proxy :: Proxy ClientRoutes) descView m

viewHook' :: Model -> Maybe RouteView -> [Action]
viewHook' m v = (SetTitle (title m v):setRoute v:maybe [] (fetchHandlers m) v)
  where
    setRoute Nothing = NoOp
    setRoute (Just view) = SetRouteView view

title :: Model -> Maybe RouteView -> MisoString
title m v = "Tour Map" <> suffix (v >>= title')
  where
    title' (ViewTour n) = toMisoString . tourName <$> getTourInfo n m
    title' (ViewTourDay n d) = tourDayTitle <$> tourDayFromModel n d m
    title' _ = Nothing
    suffix p = fromMaybe "" ((" – " <>) <$> p)

-- | What data is needed for each view
fetchHandlers :: Model -> RouteView -> [Action]
fetchHandlers _ ViewAll = [FetchData FInfoIndex, FetchData FTrackAll]
fetchHandlers m (ViewTour n) = [FetchData (FInfoTour n), FetchData (FTrackTour n)] ++ fetchHandlers m ViewAll
fetchHandlers m (ViewTourDay n d) =
  (FetchData (FElev d):FetchData (FTrackDaily d):fetchBlog) ++ fetchHandlers m (ViewTour n)
  where fetchBlog = case tourDayFromModel n d m of
                      Just tourDay -> [FetchData (FBlogHtml tourDay)]
                      Nothing -> []

----------------------------------------------------------------------------
-- Stateful IO with external libraries

data Context = Context
  { updateElevChart :: Maybe ChartConfig -> IO ()
  , setMapView :: RouteView -> IO ()
  , setMapData :: FetchThing GeoData -> GeoData -> Model -> IO ()
  , ctxSetInfoTour :: Tour -> IO ()
  , fetchCache :: FetchCache
  }

nullContext :: Config -> Context
nullContext cfg = Context noop noop (const $ const noop) noop (newFetchCache cfg)
  where noop = const $ pure ()

initContext :: Config -> MVar Context -> IO ()
initContext cfg ref = do
  updateElevChart <- initElevChartContext
  (setMapData, setMapView, ctxSetInfoTour) <- initMapViewContext cfg
  putMVar ref Context { fetchCache = newFetchCache cfg, .. }

----------------------------------------------------------------------------
-- Elev Chart

initElevChartContext :: IO (Maybe ChartConfig -> IO ())
initElevChartContext = do
  chartRef <- newMVar Nothing
  return $ \mcfg -> do
    mCanvasElem <- getElementById "elev-chart"
    modifyMVar_ chartRef $ \inst -> do
      doMaybe destroyChart inst
      case (mCanvasElem, mcfg) of
        (Just canvasElem, Just cfg) -> do
          setCanvasHeight canvasElem 20
          Just <$> newChart canvasElem cfg
        _ -> pure Nothing

----------------------------------------------------------------------------
-- Tour Map

initLeaflet :: MisoString -> MisoString -> IO Leaflet
initLeaflet accessToken sel = do
  l <- newLeaflet sel
  leafletSetView (-31.952222) 115.858889 6 l
  mapbox <- newLayerMapBox accessToken
  osm <- newLayerMapnik
  newLayersControl l [("MapBox OSM", mapbox), ("OpenStreetMap Mapnik", osm)]
  addLayer l mapbox
  return l

initMapViewContext :: Config
                   -> IO ( FetchThing GeoData -> GeoData -> Model -> IO ()
                         , RouteView -> IO ()
                         , Tour -> IO () )
initMapViewContext Config{..} = do
  stateRef <- newMVar (MapState ViewAll Nothing Nothing Nothing Nothing [])

  leaflet <- initLeaflet cfgMapBoxToken "tour-map"

  let icon = newIcon (toMisoString . show $ cfgStaticURI)
  blueIcon <- icon "blue"
  greenIcon <- icon "green"
  redIcon <- icon "red"

  let updateView = withMVar stateRef (updateVisibilityZoom leaflet)

  leafletOnClick (\ev -> putStrLn $ clickCoord ev) leaflet
  leafletOnZoomEnd updateView leaflet

  let
    removeMaybe = doMaybe (removeLayer leaflet)

    modifyMap f = modifyMVar_ stateRef $ \s -> do
      s <- f s
      updateVisibilityZoom leaflet s
      pure s

    setMapView view = modifyMap (pure . (msView .~ view))

    setMapData fetch geo model = modifyMap $ \s -> do
      let llayer :: Lens' MapState (Maybe MapLayer)
          llayer = case fetch of
                     FTrackAll -> msLayerAll
                     FTrackTour _ -> msLayerTour
                     FTrackDaily _ -> msLayerDaily
          tooltip = case fetch of
            FTrackAll -> allTooltip model
            FTrackTour n -> tourTooltip model n
            FTrackDaily _ -> \d -> routeViewTour (s ^. msView) >>= \n -> tourTooltip model n d

      layer <- makeLayer leaflet tooltip geo
      removeMaybe (s ^. llayer)

      -- convert layer click event to miso sub message
      leafletOnClick (\ev -> doMaybe (triggerLayerClickSub leaflet) (toMisoString <$> (evFeatureProperties ev >>= getFeatureName))) layer


      -- if we are updating the layer corresponding to current view,
      -- then set map view bounds to the layer.
      when (fetchIsForView fetch (s ^. msView)) $
        setBoundsToLayer leaflet layer

      pure (s & (llayer .~ (Just layer)))

    ctxSetInfoTour Tour{..} = modifyMap $ \s -> do
      -- clear out existing info layers
      removeMaybe (s ^. msInfoTour)
      mapM_ (removeLayer leaflet . snd) (s ^. msInfoDaily)

      let
        dayMarker d@TourDay{..} = case dayFromCoord <|> dayToCoord of
          Just coord -> do
            let title = toMisoString (dayFrom `orElse` dayTo)
            m <- newLayerMarker blueIcon title Nothing (geoLatLng coord)
            bindPopup m (tourDayPopup title d)
            pure (Just m)
          Nothing -> pure Nothing

      markers <- newLayerGroup =<< catMaybes <$> mapM dayMarker tourDays

      markers2 <- forM tourDays $ \d@TourDay{..} -> do
        from <- case dayFromCoord of
          Just geo -> do
            let title = toMisoString dayFrom
            m <- newLayerMarker greenIcon title (Just 1000) (geoLatLng geo)
            bindPopup m (tourDayPopup title d)
            pure (Just m)
          Nothing -> pure Nothing
        to <- case dayToCoord of
          Just geo -> do
            let title = toMisoString dayTo
            m <- newLayerMarker redIcon title (Just 1000) (geoLatLng geo)
            bindPopup m (tourDayPopup title d)
            pure (Just m)
          Nothing -> pure Nothing

        group <- newLayerGroup (catMaybes [from, to])
        pure (dayDate, group)

      pure (s & (msInfoTour .~ Just markers) & (msInfoDaily .~ markers2))

  return (setMapData, setMapView, ctxSetInfoTour)

makeLayer :: Leaflet -> (MisoString -> Maybe MisoString) -> GeoData -> IO MapLayer
makeLayer leaflet tooltip geo = do
  layer <- newLayerGeoJSON (unGeoData geo)
  bindTooltip layer (\f -> fromMaybe f (tooltip f))
  addLayer leaflet layer
  pure layer

tourDayTooltip :: TourDay -> MisoString
tourDayTooltip d = tourDayPopup (tourDayTitle d) d

tourDayPopup :: MisoString -> TourDay -> MisoString
tourDayPopup loc TourDay{..} = mconcat [ dayStr dayNum
                                       , formatDate dayDate
                                       , "<br>" , loc ]
  where dayStr 0 = ""
        dayStr n = "Day " <> toMisoString (show n) <> ". "

allTooltip :: Model -> MisoString -> Maybe MisoString
allTooltip m n = (infoIndex m >>= fmap (toMisoString . tourName) . M.lookup (misoText n))

tourTooltip :: Model -> MisoString -> MisoString -> Maybe MisoString
tourTooltip m n d = tourDayTooltip <$> tourDayFromTrackName n d m

fetchIsForView :: (FetchThing GeoData) -> RouteView -> Bool
fetchIsForView FTrackAll ViewAll = True
fetchIsForView (FTrackTour ft) (ViewTour vt) = ft == vt
fetchIsForView (FTrackDaily fd) (ViewTourDay _ vd) = fd == vd
fetchIsForView _ _ = False

updateVisibilityZoom :: Leaflet -> MapState -> IO ()
updateVisibilityZoom leaflet MapState{..} = do
  let add = doMaybe (addLayer leaflet)
      remove = doMaybe (removeLayer leaflet)

  -- show/hide tracks depending on what view
  case _msView of
    ViewAll -> do add _msLayerAll
                  remove _msLayerTour
                  remove _msLayerDaily
    ViewTour _ -> do remove _msLayerAll
                     add _msLayerTour
                     remove _msLayerDaily
    ViewTourDay _ _ -> do remove _msLayerAll
                          add _msLayerDaily
                          add _msLayerTour

  -- update visibility of individual tracks within the tour
  updateLayerStyles MapState{..}

  -- show markers for whole tour when slightly zoomed in
  zoom <- leafletGetZoom leaflet
  if zoom >= 8 then add _msInfoTour else remove _msInfoTour

  -- show markers for current tour day and hide the rest
  forM_ _msInfoDaily $ \(date, layer) ->
    let action = if routeViewDay _msView == Just date then addLayer else removeLayer
    in action leaflet layer

updateLayerStyles :: MapState -> IO ()
updateLayerStyles MapState{..} = do
  let stroke name = case _msView of
                      ViewAll -> False
                      ViewTour _ -> True
                      ViewTourDay _ d -> name /= featureName d
  doMaybe (setLayerStyle "#a40000" (const True)) _msLayerAll
  doMaybe (setLayerStyle "#c17d11" stroke) _msLayerTour
  doMaybe (setLayerStyle "#729fcf" (const True)) _msLayerDaily

clickCoord :: LeafletMouseEvent -> String
clickCoord ev = "coord: [" <> show lat <> ", " <> show lng <> "]"
  where LatLng lat lng = evLatLng ev

----------------------------------------------------------------------------
-- Inserting blog post text.
-- Would be good if Miso supported insertion of html verbatim.

foreign import javascript unsafe "document.querySelectorAll($1).forEach(function (el) { el.innerHTML = $2; });"
  setBlogHtmlElem :: JSString -> JSString -> IO ()

foreign import javascript unsafe "document.title = $1;"
  setDocumentTitle :: JSString -> IO ()


----------------------------------------------------------------------------
-- Conversion of leaflet layer clicks to sub messages

foreign import javascript unsafe "window.dispatchEvent(new CustomEvent('tourmap:layerClick', { detail: { featureName: $2 } }));"
  js_triggerLayerClickSub :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "window.addEventListener($1, function(ev) { $2(ev.detail.featureName); });"
  js_layerClickSub :: JSString -> F.Callback (JSVal -> IO ()) -> IO ()

triggerLayerClickSub :: Leaflet -> MisoString -> IO ()
triggerLayerClickSub l f = js_triggerLayerClickSub (unLeaflet l) f

layerClickSub :: (MisoString -> action) -> Sub action model
layerClickSub f _ = \sink -> do
  js_layerClickSub "tourmap:layerClick" =<< do
    F.asyncCallback1 $ \n -> do
      Just name <- fromJSVal n
      sink $ f name
