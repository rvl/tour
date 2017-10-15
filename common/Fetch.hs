{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Fetch
  ( FetchThing(..)
  , FetchCache
  , newFetchCache
  , fetchAsync
  , fetchValue
  ) where


#ifdef GHCJS_BROWSER
import Miso
import JavaScript.Web.XMLHttpRequest
import qualified Data.JSString as JS
import qualified JavaScript.RegExp as RE
import Data.Maybe (fromMaybe)
#endif

import Miso.String (MisoString, toMisoString)
import Data.Monoid
import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Control.Concurrent.Async
import Control.Exception

import Data.GADT.Compare.TH
import qualified Data.Dependent.Map as DM

import AppTypes
import Types
import TourJson
import Util

data FetchThing a where
  FInfoIndex  :: FetchThing (Map Text Tour)
  FInfoTour   :: MisoString -> FetchThing Tour
  FBlogHtml   :: TourDay -> FetchThing (Maybe MisoString)
  FTrackAll   :: FetchThing GeoData
  FTrackTour  :: MisoString -> FetchThing GeoData
  FTrackDaily :: Day -> FetchThing GeoData
  FElev       :: Day -> FetchThing [ElevPoint]

deriveGEq ''FetchThing
deriveGCompare ''FetchThing

deriving instance Show (FetchThing a)
deriving instance Eq (FetchThing a)

----------------------------------------------------------------------------
-- Fetch cache

type Cache = DM.DMap FetchThing Async

data FetchCache = FetchCache
                  { config :: Config
                  , cache :: Cache
                  }

newFetchCache :: Config -> FetchCache
newFetchCache cfg = FetchCache cfg mempty

getCache :: FetchThing a -> Cache -> Maybe (Async a)
getCache f c = DM.lookup f c

putCache :: FetchThing a -> Async a -> Cache -> Cache
putCache f a c = DM.insert f a c

fetchAsync :: FetchThing a -> FetchCache -> IO (Async a, FetchCache)
fetchAsync f FetchCache{..} = case getCache f cache of
  Just a -> pure (a, FetchCache{..})
  Nothing -> do
    a <- async (doFetch config f)
    pure (a, FetchCache { cache = putCache f a cache, ..})

fetch' ::  FetchThing a -> FetchCache -> IO (Maybe a, FetchCache)
fetch' f c = do
  (a, c') <- fetchAsync f c
  a' <- fetchValue a
  pure (a', c')

fetchValue :: Async a -> IO (Maybe a)
fetchValue a = catch (Just <$> wait a) (\ (ex :: IOException) -> logio ex >> pure Nothing)
  where logio ex = putStrLn $ "Problem fetching: " ++ show ex

----------------------------------------------------------------------------

doFetch :: Config -> FetchThing a -> IO a
doFetch cfg FInfoIndex = getJSON cfg "index"
doFetch cfg (FInfoTour name) = getJSON cfg ("tours/" <> name)
doFetch cfg (FBlogHtml day) = getBlogHtml cfg day
doFetch cfg FTrackAll = getGeoJSON cfg "all-tracks"
doFetch cfg (FTrackTour name) = getGeoJSON cfg ("tracks/" <> name)
doFetch cfg (FTrackDaily day) = getGeoJSON cfg ("daily/" <> dayUri day)
doFetch cfg (FElev day) = getJSON cfg ("elev/" <> dayUri day)

----------------------------------------------------------------------------
-- fetchers

dayUri :: Day -> MisoString
dayUri = toMisoString . formatISODate

-- | Get JSON then parse with aeson.
getJSON :: forall a. FromJSON a => Config -> MisoString -> IO a
getJSON cfg name = do
  Just resp <- jsonReq cfg name
  case eitherDecodeStrict resp :: Either String a of
    Left s -> error s
    Right j -> pure j

#ifdef GHCJS_BROWSER
-- | Fetch JSON then use browser JSON.parse to get a JSVal.
getGeoJSON :: Config -> MisoString -> IO GeoData
getGeoJSON cfg name = do
  res <- contents <$> xhr (jsonReq' cfg name)
  case res of
    Just body -> pure . GeoData . js_parse $ body
    Nothing -> error "no result"

jsonReq :: Config -> MisoString -> IO (Maybe ByteString)
jsonReq cfg name = contents <$> xhrByteString (jsonReq' cfg name)

jsonReq' cfg name = Request { reqMethod = GET
                            , reqURI = (JS.pack . show $ cfgStaticURI cfg) <> "data/" <> name <> ".json"
                            , reqLogin = Nothing
                            , reqHeaders = []
                            , reqWithCredentials = False
                            , reqData = NoData
                            }

getBlogHtml :: Config -> TourDay -> IO (Maybe MisoString)
getBlogHtml cfg TourDay{..} = (>>= munge) <$> getBlogHtml' postUrl
  where
    munge = fmap (fixImageUrls postUrl) . htmlBody
    dayBlog = Nothing -- fixme: add field to TourDay
    defaultPage = toMisoString (show dayDate <> "-tour-" <> show dayNum)
    page = fromMaybe defaultPage dayBlog
    postUrl = (JS.pack . show $ cfgBlogUrl cfg) <> page <> "/"

getBlogHtml' :: MisoString -> IO (Maybe MisoString)
getBlogHtml' postUrl = catch (contents <$> xhr req) handle
  where
    req = Request { reqMethod = GET
                  , reqURI = postUrl <> "embed.html"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
    handle = \ (ex :: XHRError) -> pure Nothing

htmlBody :: MisoString -> Maybe MisoString
-- parsing html with regexps ... now you have two problems.
-- poor regexp api ... three problems!
htmlBody = fmap getBody . matchBody . oneLine
  where
    oneLine = JS.replace "\n" ""
    matchBody t = RE.exec t bodyExp
    bodyExp = RE.create flags ".*<body[^>]*>(.*?)<\\/body>"
    flags = RE.REFlags True True -- multiline, ignore case
    getBody = head . RE.subMatched

foreign import javascript unsafe "$2.replace(new RegExp('<img src=\"([^\"]+)\"', 'g'), '<img src=\"' + $1 + '$1\"')"
  fixImageUrls :: MisoString -> MisoString -> MisoString

#else
jsonReq :: Config -> MisoString -> IO (Maybe ByteString)
jsonReq _ _ = pure Nothing

getGeoJSON :: Config -> MisoString -> IO GeoData
getGeoJSON _ = pure . GeoData . String

getBlogHtml :: Config -> TourDay -> IO (Maybe MisoString)
getBlogHtml cfg d = pure Nothing

#endif
