{-# LANGUAGE OverloadedStrings #-}
module DevServer where

import Network.Wai                            (Application, responseFile)
import Network.Wai.Handler.Warp               (defaultSettings, run, runSettings, setPort, setHost, setTimeout, Port)
import Network.HTTP.Types.Status              (Status(..), status200)
import Network.HTTP.Types.Header              (RequestHeaders)
import Data.Function ((&))
import Data.Monoid ((<>))
import Network.Wai.Application.Static
import WaiAppStatic.Types
import qualified Data.ByteString as LBS
import System.FilePath
import Data.String
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Rewrite
import Network.Wai.UrlMap
import Control.Applicative
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = devServerMain "127.0.0.1" 8000

-- | A @main@ for doing development.
devServerMain :: String -> Port -> IO ()
devServerMain host port = do
  putStrLn $ "Running dev server on " <> host <> ":" <> show port

  bc <- fromMaybe "frontend" <$> lookupEnv "BOWER_COMPONENTS"
  let app = logStdoutDev (devServer
                          "frontend"
                          "dist-ghcjs/build/tour/tour.jsexe"
                          (bc </> "bower_components"))

  runSettings (defaultSettings & setTimeout 3600 & setPort port & setHost (fromString host)) app

-- Serve static files and javascript from build directory.
-- Any other route will result in the SPA html.
devServer :: FilePath -> FilePath -> FilePath -> Application
devServer frontend jsexe bc = rewriteJS (mapUrls urls)
  where urls = mount "static" ( mount "data" (staticServer "_build")
                                <|> mountRoot (staticServer "_www/static") )
               <|> mount "jsexe" (staticServer jsexe)
               <|> mount "bower_components" (staticServer bc)
               <|> mountRoot (serveFile (frontend </> "index.html"))

-- | Static file server with caching disabled.
staticServer :: FilePath -> Application
staticServer path = staticApp ((defaultFileServerSettings path) & noCache)
  where
    noCache s = s { ssMaxAge = MaxAgeSeconds 0 }

serveFile :: FilePath -> Application
serveFile f req respond = respond (responseFile status200 [] f Nothing)

rewriteJS :: Middleware
rewriteJS = rewritePureWithQueries $ \(ps, qs) _ -> (rewrite ps, qs)
  where
    rewrite [p] = if T.isSuffixOf ".js" p then ["jsexe", p] else [p]
    rewrite ps = ps
