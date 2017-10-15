{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric      #-}

module AppTypes where

import Network.URI (URI(..), parseURI)
import GHC.Generics
import Miso.String
import Data.Time.Calendar (Day)

#ifdef GHCJS_BROWSER
import           Servant.API
import qualified Data.Text as T
import qualified Data.JSString as JS
import GHCJS.Types (JSVal)

newtype GeoData = GeoData { unGeoData :: JSVal }
instance Eq GeoData where
  _ == _ = False
instance Show GeoData where
  show _ = "<geodata>"
instance Show JSVal where
  show _ = "<jsval>"


instance FromHttpApiData JS.JSString where
  parseUrlPiece = Right . JS.pack . T.unpack

instance ToHttpApiData JS.JSString where
  toUrlPiece = T.pack . JS.unpack

#else
import Data.Aeson (Value)

newtype GeoData = GeoData { unGeoData :: Value } deriving (Eq, Show)
#endif

data Config = Config
              { cfgBaseURI     :: URI
              , cfgBlogUrl     :: URI
              , cfgStaticURI   :: URI
              , cfgMapBoxToken :: MisoString
              } deriving (Eq, Show)

initConfig :: Config
initConfig = Config{..}
  where
    cfgBaseURI = URI "" Nothing "/" "" ""
    cfgStaticURI = URI "" Nothing "/static/" "" ""
    Just cfgBlogUrl = parseURI "https://rodney.id.au/posts/"
    cfgMapBoxToken = "pk.eyJ1IjoicnZsIiwiYSI6ImMzNzdiNWQ1YTMzYTRjNzEyOTU2ZTY2NDhiNTQ5MDBhIn0.out7-ubBjWy-7C_FH4WUHQ"
