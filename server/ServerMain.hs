{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
module ServerMain where

import           Data.Proxy
import qualified Lucid                                as L
import           Lucid.Base
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           Servant
import qualified System.IO                            as IO

import           Miso
import           Miso.String

import App
import Types
import TourJson

-- this is based off haskell-miso.org example, but currently broken
-- because i can't work out types for routes with captures

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3002..."
  run 3002 $ logStdout (compress app)
    where
      compress = gzip def { gzipFiles = GzipCompress }

app :: Application
app = serve (Proxy @ API) (static :<|> serverHandlers :<|> Tagged handle404)
  where static = serveDirectory "static"

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | API type
type API = ("static" :> Raw)
  :<|> ServerRoutes
  :<|> Raw

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
      renderBS $ toHtml $ Wrapper $ the404 Model { uri = goHome }

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) =
    L.doctypehtml_ $ do
        L.head_ $ do
          L.title_ "Tour Browser"
          L.meta_ [L.charset_ "utf-8"]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "A map of our tours, with info, and descriptions."
                  ]
          cssRef bulmaRef
          cssRef fontAwesomeRef
          jsRef "static/all.js"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.5.3/css/bulma.min.css"

{-
serverHandlers ::
       Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
-}
serverHandlers = tourSummaryHandler :<|> tourDayHandler :<|> tourListHandler
  where
    send f u = pure $ Wrapper $ f Model {uri = u}
    tourSummaryHandler = send tourSummaryView goTourSummary
    tourDayHandler     = send tourDayView goTourDay
    tourListHandler    = send tourListView goTourList
