{-# LANGUAGE Arrows, OverloadedStrings #-}

module Track (loadTrackPoints, loadCalcElev, renameTracks) where

import Data.Maybe (catMaybes)
import Safe (readMay)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.ByteString.Lazy as BLS
import Control.Monad (void)

import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

import Naqsha.Geometry
import Naqsha.Geometry.Spherical (distance)

import Types
import Simplify

data TrackPoint = TrackPoint Geo UTCTime Double deriving Show

loadTrackPoints :: FilePath -> IO [TrackPoint]
loadTrackPoints src = catMaybes <$> runX process
  where process =
          readDocument [withValidate no] src
          >>>
          processDocumentRootElement

loadCalcElev :: FilePath -> IO [ElevPoint]
loadCalcElev src = simplify . calcElev <$> loadTrackPoints src
  where simplify = enpeuck 5

calcElev :: [TrackPoint] -> [ElevPoint]
calcElev = calc 0 . filter hasElev
  where
    calc :: Double -> [TrackPoint] -> [ElevPoint]
    calc _ [] = []
    calc s [TrackPoint _ t e] = [ElevPoint e s (utcTimeToPOSIXSeconds t)]
    calc s (TrackPoint ga t' e:p@(TrackPoint gb _ _):ps) = (ElevPoint e s t:calc s' (p:ps))
      where
        s' = s + distance ga gb
        t = utcTimeToPOSIXSeconds t'
    hasElev :: TrackPoint -> Bool
    hasElev (TrackPoint _ _ n) = n > 0

processDocumentRootElement :: IOSArrow XmlTree (Maybe TrackPoint)
-- processDocumentRootElement = deep (isElem >>> hasName "trkpt" >>> getTrackPoint)
-- processDocumentRootElement = isElem >>> hasName "gpx" >>> getChildren
-- 	                     >>> isElem >>> hasName "trkseg" >>> getChildren
--                              >>> isElem >>> hasName "trkpt" >>> getTrackPoint
processDocumentRootElement = getXPathTrees "/gpx/trk/trkseg/trkpt" >>> getTrackPoint

getTrackPoint :: IOSArrow XmlTree (Maybe TrackPoint)
getTrackPoint = proc pt -> do
  slat <- getAttrValue "lat" -< pt
  slon <- getAttrValue "lon" -< pt
  sele <- textAtTag "ele" -< pt
  stime <- textAtTag "time" -< pt
  returnA -< makeTrackPoint slat slon sele stime

makeTrackPoint :: String -> String -> String -> String -> Maybe TrackPoint
makeTrackPoint slat slon sele stime = do
  lat' <- readMay slat :: Maybe Double
  lon' <- readMay slon :: Maybe Double
  ele <- readMay sele
  time <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" stime -- %z ?
  let deg = degree . toRational
      g = Geo (lat (deg lat')) (lon (deg lon'))
  return $ TrackPoint g time ele

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

renameTracks :: String -> FilePath -> FilePath -> IO ()
renameTracks n src dst = void $
  runX (readDocument [withValidate False] src
        >>>
        processTopDown (renameTracks' n)
        >>>
        writeDocument [withIndent yes] dst)

renameTracks' :: ArrowXml a => String -> a XmlTree XmlTree
renameTracks' n = replaceChildren (txt n) `when` (isElem >>> hasName "name")
