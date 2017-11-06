{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExistentialQuantification #-}

module App where

import qualified Data.Map    as M
import           Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import           Data.Proxy
import           Servant.API
import           Servant.Utils.Links
import Network.URI (relativeTo, relativeFrom, parseURI, parseRelativeReference, URI(..), uriIsAbsolute)
import Data.Aeson (Value(..))
import Data.Maybe
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           Miso
import           Miso.String (MisoString, toMisoString)
import Data.List (find)
import Data.String (IsString(..))
import Control.Monad (join)

import Types
import AppTypes
import Fetch
import Util

-- | We can pretty much share everything
--
-- model, action, view, router, links, events map
-- decoders are all shareable

-- | Model
data Model
  = Model
  { config :: Config
    -- ^ uri options
  , uri :: URI
    -- ^ current URI of application
  , routeView :: RouteView
    -- ^ description of current route
  , infoIndex :: Maybe (Map Text Tour)
    -- ^ Summaries of tours
  , infoTour :: Map MisoString Tour
    -- ^ detailed tour informations by name
  , blogHtml :: Map Day (Maybe MisoString)
    -- ^ blog html by date
  , trackAll :: Maybe GeoData
    -- ^ all tracks geojson
  , trackTour :: Map MisoString GeoData
    -- ^ tour geojson
  , trackDaily :: Map Day GeoData
    -- ^ track for a day
  , elevDaily :: Map Day [ElevPoint]
    -- ^ elevation data for a day
  , showContent :: Bool
    -- ^ whether to show text/tables/etc
  } deriving (Eq, Show)

-- makeLenses ''Model

data RouteView = ViewAll
               | ViewTour MisoString
               | ViewTourDay MisoString Day
               deriving (Show, Eq)

-- maybe put this back in Main.hs if it doesn't work
-- descView :: ClientRoutes -> RouteView
descView = descSummary :<|> descDay :<|> descAll
  where
    descAll m = Just ViewAll
    descSummary name m = Just $ ViewTour name
    descDay name day m = Just $ ViewTourDay name day

routeViewTour :: RouteView -> Maybe MisoString
routeViewTour ViewAll = Nothing
routeViewTour (ViewTour t) = Just t
routeViewTour (ViewTourDay t _) = Just t

routeViewDay :: RouteView -> Maybe Day
routeViewDay (ViewTourDay _ d) = Just d
routeViewDay _ = Nothing

initModel :: Config -> URI -> Model
initModel cfg uri = Model cfg uri' ViewAll mempty mempty mempty Nothing mempty mempty mempty True
  where
    uri' = unfixURI' (cfgBaseURI cfg) uri

-- | Action
data Action
  = Init
  | HandleURI URI
  | ChangeURI URI
  | SetTitle MisoString
  | SetRouteView RouteView
  | ToggleContent
  | HandleLayerClick MisoString
  | forall a. (Show a, Eq a) => FetchData (FetchThing a)
  | forall a. (Show a, Eq a) => SetData (FetchThing a) a
  | NoOp

deriving instance Show Action

instance Eq Action where
  NoOp == NoOp = True
  a == b       = show a == show b -- cheap hack

----------------------------------------------------------------------------

mainView :: Model -> View Action -> View Action
mainView m v = div_ [class_ "tour-main"]
  [ div_ [class_ "tour-map-container"] [tourMap m]
  , div_ [class_ "elev-chart-container", id_ "elev-chart-container"] $ elevChart m
  , div_ [class_ "main-view"] [v]
  ]

clientHandlers = tourSummaryView :<|> tourDayView :<|> tourList

tourList :: Model -> View Action
tourList m@Model{..} = div_ [class_ "tour-list"]
  [ header_ []
    [ h1_ [] [ text "All the tours" ]
    , div_ [class_"buttons"] [toggleContentButton "Tour List" showContent ] ]
  , section_ [class_ $ if showContent then "" else "hidden"]
    [ table_ [ class_ "table is-striped" ]
      [ thead_ []
        [ tr_ []
          [ th_ [] [ text "Name" ]
          , th_ [] [ text "Countries" ]
          ]
        ]
      , maybe tourListLoading tourListBody infoIndex
      ]
    ]
  ]
  where
    tourListBody tours = tbody_ [] (map (uncurry tourListRow) (M.assocs tours))
    tourListRow key Tour{..} = tr_ [onClick (goTourSummary (toMisoString key))]
                               [ td_ [] [text $ toMisoString tourName]
                               , td_ [] [] ]
    tourListLoading = tbody_ [class_ "loading"] [tr_ [] [td_ [colspan_ "2"] [ text "loading" ]]]

tourSummaryView :: MisoString -> Model -> View Action
tourSummaryView name m@Model{..} = case M.lookup name infoTour of
  Just tour -> tourSummaryView' name tour m
  Nothing -> div_ [class_ "tour-page-index loading"] [text "Loading..."]

tourSummaryView' :: MisoString -> Tour -> Model -> View Action
tourSummaryView' name Tour{..} m = div_ [class_ "tour-page-index"]
  [ header_ []
    [ h1_ [] [ text (toMisoString tourName) ]
    , breadcrumbs [ (NoOp, toMisoString tourName) ]
    , div_ [class_"buttons"]
      [ p_ [class_ "tour-description"] [ text $ toMisoString tourDescription ]
      , nav_ [class_ "tour-nav"]
        [ toggleContentButton "Table" (showContent m) ]
        , a_ [class_ "button is-link is-outlined", onClick goTourList]
          (iconButton "arrow-up" "All the tours")
        ]
    ]
  , section_ [class_ $ "content" <> if showContent m then "" else " hidden"]
             [tourSummaryTable tourDays]
  ]
  where
    tourSummaryTable days = table_ [ class_ "table is-striped" ]
      [ thead_ []
          [ tr_ [] [ th_ [class_ "col-date"] [text "Date"]
                   , th_ [class_ "col-from"] [text "From"]
                   , th_ [class_ "col-to"] [text "To"]
                   , th_ [class_ "col-dist"] [text "Dist (km)"]
                   ]
          ]
      , tbody_ [] (map tourDayRow days) ]
    tourDayRow TourDay{..} = tr_ [ onClick $ goTourDay name dayDate ]
      [ td_ [ class_ "col-date" ] [ text $ formatDate dayDate ]
      , td_ [ class_ "col-from" ] [ text $ toMisoString dayFrom ]
      , td_ [ class_ "col-to" ] [ text $ toMisoString dayTo ]
      , td_ [ class_ "col-dist" ] [ text . formatDist $ dayDist ]
      ]

formatDist :: Int -> MisoString
formatDist 0 = ""
formatDist n = toMisoString $ show n

formatDate :: Day -> MisoString
formatDate = toMisoString . formatTime defaultTimeLocale "%e/%m/%Y"

featureName :: Day -> MisoString
featureName = toMisoString . formatTime defaultTimeLocale "%Y%m%d"

featureDay :: String -> Maybe Day
featureDay = parseTimeM True defaultTimeLocale "%Y%m%d"

tourDayTitle :: TourDay -> MisoString
tourDayTitle TourDay{..} = case (dayFrom, dayTo) of
  ("", "")   -> formatDate dayDate
  (from, "") -> toMisoString from
  ("", to)   -> toMisoString to
  (from, to) -> toMisoString $ from <> " → " <> to

breadcrumbs :: [(Action, MisoString)] -> View Action
breadcrumbs items = nav_ [ class_ "breadcrumb" ] [ ul_ [] (map (uncurry li) items') ]
  where
    items' = ((goTourList, "Tours"):items)
    li a t = li_ cls [ a_ [onClick a] [text t] ]
      where cls | a == NoOp = [ class_ "is-active" ]
                | otherwise = []

tourDayView :: MisoString -> Day -> Model -> View Action
tourDayView name date m = case getTourInfo name m of
  Just tour -> case getTourDay date tour of
    Just tourDay -> tourDayView' name tour tourDay m
    Nothing -> div_ [class_ "tour-day"] [text "Not found"]
  Nothing -> div_ [class_ "tour-day loading"] [text "Loading..."]

toggleContentButton :: MisoString -> Bool -> View Action
toggleContentButton what showContent = button_
  [ class_ "button is-outlined toggle-content", onClick ToggleContent ]
  (iconButton icon $ label <> " " <> what)
  where icon | showContent = "minus-square"
             | otherwise = "plus-square"
        label | showContent = "Hide"
              | otherwise = "Show"

tourDayView' :: MisoString -> Tour -> TourDay -> Model -> View Action
tourDayView' name tour day@TourDay{..} m@Model{..} = div_ [class_ "tour-day"]
  [ header_ []
    [ h1_ [] [ text $ tourDayTitle day ]
    , breadcrumbs [ (goTourSummary name, toMisoString $ tourName tour)
                  , (NoOp, formatDate dayDate) ]
    , nav_ [class_"buttons"]
      ((if haveBlog then [toggleContentButton "Blog Post" showContent] else [])
        ++ [tourDayNav name tour day])
    ]
    , section_ [class_ $ "blog content" <> if (showContent && mightHaveBlog) then "" else " hidden"]
      [div_ [class_ "blog-html"] [text "Loading..."]]  -- placeholder for html
    ]
  where
    blog = isJust <$> M.lookup dayDate blogHtml
    haveBlog = blog == Just True
    mightHaveBlog = blog /= Just False

tourMap :: Model -> View Action
tourMap _ = div_ [class_ "tour-map", id_ "tour-map"] []

elevChart :: Model -> [View Action]
elevChart m | routeViewDay (routeView m) == Nothing = []
            | otherwise = [canvas_ [ class_ "elev-chart"
                                   , id_ "elev-chart"
                                   , width_ "640"
                                   , height_ "20"
                                   ] []]

iconButton, iconButtonL :: MisoString -> MisoString -> [View Action]
iconButton i l = [ span_ [class_ "icon"] [i_ [class_ $ "fa fa-" <> i] []]
                 , span_ [] [text l] ]
iconButtonL i = reverse . iconButton i

tourDayNav :: MisoString -> Tour -> TourDay -> View Action
tourDayNav name tour day = nav_ [class_ "tour-nav"]
                           [ a_ (cls True:attrs prev)
                             (iconButton "arrow-left" "Previous Day")
                           , a_ [ cls False, onClick (goTourSummary name) ]
                             (iconButton "arrow-up" "Tour")
                           , a_ (cls True:attrs next)
                             (iconButtonL "arrow-right" "Next Day ")
                           ]
  where
    cls p = class_ $ "button is-link" <> if p then "" else " is-outlined"
    attrs nav = case nav of
                  Just d -> [onClick (goTourDay name (dayDate d))]
                  Nothing -> [disabled_ "disabled"]
    prev = getPrevTourDay tour day
    next = getNextTourDay tour day

getTourDay :: Day -> Tour -> Maybe TourDay
getTourDay date = find ((== date) . dayDate) . tourDays

getNextTourDay, getPrevTourDay :: Tour -> TourDay -> Maybe TourDay
getNextTourDay Tour{tourDays} day = snd <$> find ((== day) . fst) (zip tourDays (tail tourDays))
getPrevTourDay Tour{tourDays} day = snd <$> find ((== day) . fst) (zip (tail tourDays) tourDays)

getTourInfo :: MisoString -> Model -> Maybe Tour
getTourInfo name m = M.lookup name (infoTour m)

tourDayFromTrackName :: MisoString -> MisoString -> Model -> Maybe TourDay
tourDayFromTrackName tourName trackName model = do
  day <- featureDay (fromMisoString trackName)
  tourDayFromModel tourName day model

tourDayFromModel :: MisoString -> Day -> Model -> Maybe TourDay
tourDayFromModel name day m =
  M.lookup name (infoTour m) >>= getTourDay day

the404 :: View Action
the404 = div_ [class_ "the404"] [
  div_ [class_ "container content"]
    [ h1_ [class_ "title"] [text "the 404"]
    , i_ [class_ "fa fa-meh-o fa-5x"] []
    , p_ [] [text "That link was not found."]
    , button_ [ onClick goTourList, class_ "button is-large is-link" ] [ text "Go to tour list" ]
    ]
  ]

-- | Type-level routes
type ClientRoutes = TourSummaryView :<|> TourDayView :<|> TourListView
type TourListView = View Action
type TourSummaryView = Capture "name" MisoString :> View Action
type TourDayView = Capture "name" MisoString :> Capture "date" Day :> View Action

-- | Type-safe links used in `onClick` event handlers to route the application
goTourList :: Action
goTourList = goto tourListView
  where
    goto b = ChangeURI (linkURI (safeLink api b))
    tourListView = Proxy :: Proxy TourListView
    api          = Proxy :: Proxy ClientRoutes

goTourSummary :: MisoString -> Action
-- goTourSummary name = FetchInfoTour name
goTourSummary name = ChangeURI $ uriTourSummary name

goTourDay :: MisoString -> Day -> Action
goTourDay name date = ChangeURI $ uriTourDay name date

uriTourSummary :: MisoString -> URI
uriTourSummary name = linkURI (safeLink api tourSummaryView name)
  where
    tourSummaryView = Proxy :: Proxy TourSummaryView
    api = Proxy :: Proxy ClientRoutes

uriTourDay :: MisoString -> Day -> URI
uriTourDay name date = linkURI (safeLink api tourDayView name date)
  where
    tourDayView = Proxy :: Proxy TourDayView
    api = Proxy :: Proxy ClientRoutes

--linkHref :: URI -> attrs
linkHref uri = href_ . toMisoString $ "/" <> show uri

-- | Add base href to a URL path.
fixURI' :: URI -> URI -> URI
fixURI' base = flip relativeTo base

fixURI :: Model -> URI -> URI
fixURI m = fixURI' (cfgBaseURI $ config m)

-- | Remove base href from a URL path.
-- This is ugly and nasty.
unfixURI' :: URI -> URI -> URI
unfixURI' base u = relativeFrom (unWeird u) base `relativeTo` root
  where
    Just root = parseRelativeReference "/"
    unWeird :: URI -> URI
    unWeird u | weird = u { uriScheme = "", uriAuthority = Nothing, uriPath = "/" ++ uriPath u }
              | otherwise = u
      where weird = uriIsAbsolute u && take 1 (uriPath u) /= "/"

unfixURI :: Model -> URI -> URI
unfixURI m = unfixURI' (cfgBaseURI $ config m)
