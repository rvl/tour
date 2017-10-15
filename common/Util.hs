{-# LANGUAGE CPP #-}

module Util where

import qualified Data.Text as T

#ifdef GHCJS_BROWSER
import GHCJS.Types (JSString, JSVal, nullRef)
import qualified Data.JSString as JS
import GHCJS.Nullable

-- | Converts a JS object into a JSON string
foreign import javascript unsafe "$r = JSON.stringify($1);"
  js_stringify :: JSVal -> JSString

-- | Loads a JSON string into a JSVal
foreign import javascript unsafe "$r = JSON.parse($1);"
  js_parse :: JSString -> JSVal

foreign import javascript unsafe "$r = document.getElementById($1);"
  js_getElementById :: JSString -> IO (Nullable JSVal)

getElementById :: JSString -> IO (Maybe JSVal)
getElementById = fmap nullableToMaybe . js_getElementById

-- | Opposite of toMisoString
misoText :: JSString -> T.Text
-- fixme: there's probably a better way
misoText = T.pack . JS.unpack

fromMisoString :: JSString -> String
fromMisoString = JS.unpack

#else
misoText :: T.Text -> T.Text
misoText = id

fromMisoString :: T.Text -> String
fromMisoString = T.unpack
#endif

doMaybe :: (a -> IO ()) -> Maybe a -> IO ()
doMaybe = maybe (pure ())

resMaybe :: (a -> IO b) -> Maybe a -> IO (Maybe b)
resMaybe act (Just a) = fmap Just (act a)
resMaybe _ Nothing    = pure Nothing

orElse :: (Eq m, Monoid m) => m -> m -> m
a `orElse` b = if a == mempty then b else a
