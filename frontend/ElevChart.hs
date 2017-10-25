module ElevChart
  ( newChart
  , destroyChart
  , Chart
  , ChartConfig(..)
  , setCanvasHeight
  ) where

import GHCJS.Types (JSString, JSVal, nullRef)
import qualified Data.Vector as V
import qualified Data.Map as M
import GHCJS.Marshal
import Unsafe.Coerce

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)

import Types

data ChartConfig = ChartConfig
  { chartPoints :: [ElevPoint]
  } deriving (Show, Eq)

foreign import javascript unsafe "$2.height = Math.floor($3._height * window.innerHeight * $1); $2.style.height = $2.height + 'px'; $r = new Chart($2, $3);"
  js_newChart :: Double -> JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "$1.height = $2; $1.style.height = $2 + 'px';"
  setCanvasHeight :: JSVal -> Int -> IO ()

newtype Chart = Chart JSVal

newChart :: JSVal -> ChartConfig -> IO Chart
newChart ctx cfg = do
  let h = setCanvasHeight ctx 100
  opts <- toJSVal_aeson cfg
  Chart <$> js_newChart (1 / 3000) ctx opts

foreign import javascript unsafe "$1.destroy(); document.querySelectorAll(\".chartjs-hidden-iframe\").forEach(function(el) { el.remove(); });"
  destroyChart :: Chart -> IO ()

instance ToJSON ChartConfig where
  toJSON (ChartConfig pts) = object
    [ "type" .= ("scatter" :: String)
    , "data" .= object [ "datasets" .= singleton dataset ]
    , "options" .= object
      [ "maintainAspectRatio" .= False
      , "legend" .= object ["display" .= False]
      , "scales" .= object [ "xAxes" .= singleton xAxis
                           , "yAxes" .= singleton yAxis ]
      , "showLines" .= True -- Chart.js 2.7.0
      ]
    , "_height" .= chartHeight
    ]
    where
      tickFontColor = "rgba(31, 31, 31, 0.9)" :: String
      gridLineColor = "rgba(0, 0, 0, 0.1)" :: String
      bgColor = "rgba(127, 0, 0, 0.2)" :: String
      borderColor = "rgba(127, 0, 0, 0.9)" :: String
      axisHeight = 30 :: Int
      dataset = object [ "label" .= ("Elevation" :: String)
                       , "data" .= V.fromList (map toPoint pts)
                       , "borderColor" .= borderColor
                       , "backgroundColor" .= bgColor
                       , "fill" .= True
                       , "borderCapStyle" .= ("square" :: String)
                       , "borderJoinStyle" .= ("round" :: String)
                       , "borderWidth" .= (3.0 :: Double)
                       , "pointRadius" .= (0.0 :: Double)
                       , "lineTension" .= (0 :: Int) ]
      xAxis = object [ "ticks" .= object
                       [ "beginAtZero" .= True
                       , "min" .= (0 :: Int)
                       , "max" .= maxDist
                       , "fontColor" .= tickFontColor
                       ]
                     , "gridLines" .= object [ "color" .= gridLineColor ]
                     ]
      yAxis = object [ "ticks" .= object
                       [ "beginAtZero" .= False
                       , "min" .= minElev
                       , "max" .= maxElev
                       , "stepSize" .= (100 :: Int)
                       , "fontColor" .= tickFontColor
                       ]
                     ]
      toPoint (ElevPoint e s _) = object [ "x" .= (s / 1000), "y" .= e ]
      (minElev, maxElev, maxDist) = getNeatBounds pts
      -- chart height is in metres then scaled in proportion to window height
      chartHeight = axisHeight + (max 100 (maxElev - minElev))

getNeatBounds :: [ElevPoint] -> (Int, Int, Int)
getNeatBounds [] = (0, 100, 100)
getNeatBounds pts = (floor100 (minimum es), ceil100 (maximum es),
                     ceil100 (ptDist (last pts) / 1000))
  where
    es = map ptElev pts
    floor100, ceil100 :: Double -> Int
    ceil100 n = (ceiling (n / 100)) * 100
    floor100 n = (floor (n / 100)) * 100

singleton :: ToJSON a => a -> Value
singleton v = Array (V.singleton (toJSON v))
