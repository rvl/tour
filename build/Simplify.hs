-- grabbed from
-- https://github.com/esmooov/haskell-utilities/blob/master/peucker.hs
-- and changed types

module Simplify
  ( enpeuck
  ) where

import Data.List

import Types

type Point = (Double,Double)
type Line = (Double,Double)

pointsToLine :: Point -> Point -> (Double,Double)
pointsToLine (x,y) (x',y') = (slope,offset) where
                             slope = (y'-y)/(x'-x)
                             offset = if isInfinite slope then x else y-(x*slope)

orthoDist :: Point -> Line -> Double
orthoDist (x,y) (slope,offset)
  | isInfinite slope = abs (x-offset)
  | slope == 0 = abs (y-offset)
  | otherwise = distance (newx,newy) (x,y)
  where newslope = -(1.0)/slope
        newoffset = y-(newslope*x)
        newx = (offset-newoffset)/(newslope-slope)
        newy = (newx*newslope)+newoffset
        distance (a,b) (a',b') = sqrt(((a'-a)^2)+((b'-b)^2))

toPoint :: ElevPoint -> Point
toPoint (ElevPoint m n _) = (m, n)

enpeuck :: Double -> [ElevPoint]-> [ElevPoint]
enpeuck _ [] = []
enpeuck thr pts
  | d < thr = [head pts,last pts]
  | otherwise = let (halfa,halfb) = break (==farPt) pts in (enpeuck thr (halfa++[farPt])) ++ (tail $ enpeuck thr halfb)
  where line = pointsToLine (toPoint $ head pts) (toPoint $ last pts)
        (farPt,d) = maximumBy (\(_, n) (m',n') -> compare n n') $ map (\x -> (x,orthoDist (toPoint x) line)) pts
