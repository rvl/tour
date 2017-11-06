import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import Data.Monoid
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BLS8
import Data.Aeson (ToJSON, encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Directory (pathIsSymbolicLink, createDirectoryIfMissing)
import Data.Maybe (fromMaybe)

import Types
import TourJson
import Track
import Data.Yaml (encodeFile, decodeFileEither)

buildDir = "_build" :: FilePath

build :: FilePath -> FilePath
build = (</>) buildDir

loadTours :: FilePath -> IO [(String, Tour)]
loadTours dir = do
  yamls <- getDirectoryFilesIO "" [dir </> "*.yaml"]
  tours <- mapM decodeFileEither yamls
  mapM_ putStrLn [yaml ++ ": " ++ show err | (yaml, Left err) <- zip yamls tours]
  return [(takeBaseName yaml, fillTour tour) | (yaml, Right tour) <- zip yamls tours]

dataFileName :: String -> FilePath
dataFileName t = "data" </> t <.> "yaml"

buildAllData :: [FilePath]
buildAllData = map build ["sha1sums", "index.json", "all-tracks.json"]

main :: IO ()
main = do
  tours <- loadTours "data"

  shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want buildAllData

    phony "clean" $ need ["clean-data", "clean-frontend"]

    phony "clean-data" $ cleanDir buildDir

    build "*.yaml" %> \out ->
      case lookup (takeBaseName out) tours of
        Just tour -> liftIO $ encodeFile out tour
        Nothing -> putQuiet "tour date not found"

    -- create distance/elevation/time dataset from daily gpx
    build "elev/*.json" %> \out -> do
      let gpx = build "daily" </> takeFileName out -<.> "gpx"
      need [gpx]
      putNormal ("Calculating elevation " ++ out)
      liftIO (loadCalcElev gpx) >>= writeJsonFile out

    -- daily gpx is a concatenation of all tracks/files for that day
    build "daily/*.gpx" %> \out -> do
      let date = takeBaseName out
      osms <- getOsmandTracks "tracks" date
      logs1 <- getDataloggerTracks "SBN" "GPS_DATA" date
      logs2 <- getDataloggerTracks "TXT" "GPS_DATA" date
      let filters = ["duplicate,location", "track,merge,sdistance=0.1k", "simplify,error=0.001k", "sort,time"]
          srcs = osms ++ logs1 ++ logs2 ++ ["data/empty.gpx"]
      need srcs
      when (not $ null srcs) $
        gpsbabel filters "gpx" "gpx" srcs out

    -- converts all gps logger files for a day into gpx
    let dailyDatalog fmt ext out = do
          sbns <- getDataloggerSources ext "GPS_DATA" (takeBaseName out)
          need sbns
          gpsbabel ["discard,sat=3"] fmt "gpx" sbns out

    -- convert locosys format sbn files
    build "datalog/SBN/*.gpx" %> dailyDatalog "sbn" "SBN"

    -- convert locosys datalogger sbp files
    build "datalog/SBP/*.gpx" %> dailyDatalog "sbp" "SBP"

    -- convert nmea format txt files
    build "datalog/TXT/*.gpx" %> dailyDatalog "nmea" "TXT"

    -- slightly lower detail geojson features
    -- contains start and end points for each day
    build "daily/*.json" %> \out -> do
      let filters = ["simplify,error=0.05k"]
          src = out -<.> "gpx"
      need [src]
      gpsbabel filters "gpx" "geojson" [src] out

    -- lower detail geojson features of complete tour
    -- each day is a single track
    build "tracks/*.json" %> \out ->
      case lookup (takeBaseName out) tours of
        Just tour -> do
          let filters = ["simplify,error=0.1k"]
              srcs = [build "daily" </> d <.> "gpx" | d <- tourDates' tour]
          need srcs
          gpsbabel filters "gpx" "geojson" srcs out
        Nothing -> return ()

    build "tracks/*.gpx" %> \out ->
      case lookup (takeBaseName out) tours of
        Just tour -> do
          let srcs = [build "daily" </> d <.> "gpx" | d <- tourDates' tour]
          need srcs
          gpsbabel [] "gpx" "gpx" srcs out
        Nothing -> return ()

    -- all tours together, low detail
    build "all-tracks.json" %> \out -> do
      let filters = ["simplify,error=1k"]
          srcs = [build "tracks" </> n <.> "gpx" | (n, _) <- tours]
      need srcs
      withTempDir $ \dir -> do
        srcs' <- forM srcs $ \src -> do
          let src' = (dir </> takeFileName src)
          putNormal $ "Renaming " ++ src ++ " -> " ++ src'
          renameTracksFaster (takeBaseName src) src src'
          return src'
        gpsbabel filters "gpx" "geojson" srcs' out

    build "stamp.data" %> \out -> do
      alwaysRerun
      writeFileChanged out (show tours)

    build "sha1sums" %> \out -> do
      let
        allDates = concatMap tourDates' (map snd tours)
        trackJson = [build "tracks" </> n <.> "json" | (n, _) <- tours]
        trackGpx = [j -<.> "gpx" | j <- trackJson]
        dailies = [build "daily" </> d <.> "json" | d <- allDates]
        elevs = [build "elev" </> d <.> "json" | d <- allDates]
        ns = trackJson ++ trackGpx ++ dailies ++ elevs
      need (build "stamp.data":ns)
      Stdout s <- cmd "sha1sum" ns
      writeFileChanged out s

    build "index.json" %> \out -> do
      need (map (dataFileName . fst) tours)
      need [build "tours" </> n <.> "json" | (n, _) <- tours]
      putNormal $ "Writing tour summary " ++ out
      writeJsonFile out (tourSummary tours)

    build "tours/*.json" %> \out -> do
      let name = takeBaseName out
      need [dataFileName name]
      case lookup name tours of
        Just tour -> do
          putNormal $ "Writing tour info " ++ out
          writeJsonFile out tour
        Nothing -> fail $ "tour " ++ name ++ " not found"

    frontendRules "_www"

cleanDir :: FilePath -> Action ()
cleanDir d = do
  putNormal $ "Cleaning files in " <> show d
  removeFilesAfter d ["//*"]

ghcjsDist = "dist-ghcjs" :: FilePath
jsexe = ghcjsDist </> "build/tour/tour.jsexe" :: FilePath

frontendRules :: FilePath -> Rules ()
frontendRules wwwDir = do
  let www = (wwwDir </>)
      frontend = ("frontend" </>)
      cabalJS args = cmd "nix-shell frontend.nix --argstr compiler ghcjsHEAD --run" ["cabal --builddir=" ++ ghcjsDist ++ " " ++ args]

      buildAllFrontend = [www "index.html"]

  want ["all-frontend"]
  phony "all-frontend" $ need buildAllFrontend

  phony "clean-frontend" $ do
    cleanDir wwwDir
    cleanDir ghcjsDist

  phony "deploy" $ do
    let target = "rodney.id.au:/srv/www/lorrimar.id.au/tour/"
    need $ buildAllData ++ buildAllFrontend

    putNormal $ "Copying frontend to " <> target
    () <- cmd "rsync -avz --exclude=static/data"
      [wwwDir <> "/", target]

    putNormal "Copying data files"
    cmd "rsync -avz --delete --include *.json --include */ --exclude=*"
      [buildDir <> "/", target <> "static/data/"]

  www "index.html" %> \out -> do
    -- site needs index.html, minified js, stylesheet, images
    need $ map www ["all.min.js.gz", "static/tour.css"]
    copyFileChanged (frontend "index-prod.html") out
    images <- getDirectoryFiles "" [frontend "static/images/*"]
    liftIO $ createDirectoryIfMissing True (www "static/images")
    mapM_ (\f -> copyFileChanged f (www "static/images" </> takeFileName f)) images

  www "static/tour.css" %> \out -> do
    let src = frontend "tour.sass"
    bc <- lookupBowerComponents
    need [src]
    cmd "sassc -I" [bc, src, out]

  ghcjsDist </> "setup-config" %> \out -> do
    need ["tour.cabal"]
    cabalJS "configure --ghcjs"

  jsexe </> "all.js" %> \out -> do
    needHaskellSources
    cabalJS "build"

  jsexe </> "*.min.js" %> \out -> do
    let maxi = dropExtension out -<.> "js"
        externs = maxi <.> "externs"
        otherExterns = "frontend/externs.js"
    need [maxi, otherExterns]
    let opt = "SIMPLE_OPTIMIZATIONS" -- ADVANCED_OPTIMIZATIONS doesn't work
    Stdout mini <- cmd "closure-compiler" [maxi, "--compilation_level=" ++ opt] "--jscomp_warning=duplicate --jscomp_warning=undefinedVars --jscomp_warning=checkVars" ["--externs=" ++ externs, "--externs=" ++ otherExterns]
    writeFileChanged out mini

  www "*.js" %> \out -> copyFile' (jsexe </> takeFileName out) out

  www "*.js.gz" %> \out -> do
    let js = dropExtension out
    need [js]
    cmd "zopfli -i1000" [js]

lookupBowerComponents :: Action FilePath
lookupBowerComponents = subdir . def <$> getEnv "BOWER_COMPONENTS"
  where
    subdir = (</> "bower_components")
    def = fromMaybe "frontend"

needHaskellSources :: Action ()
needHaskellSources = do
  sources <- getDirectoryFiles "" ["common//*.hs", "frontend//*.hs"]
  need ((ghcjsDist </> "setup-config") : sources)

gpsbabel :: [String]   -- ^ filters
         -> String     -- ^ input formaat
         -> String     -- ^ output format
         -> [FilePath] -- ^ input files
         -> FilePath   -- ^ output file
         -> Action ()
gpsbabel filters ifmt ofmt src dst = cmd "gpsbabel -t -i" [ifmt] inputFileArgs filterArgs "-o" [ofmt] "-F" [dst]
  where
    -- creates a list of -f filename for each input file
    inputFileArgs = concatMap (\f -> ["-f", f]) src
    filterArgs = concatMap (\f -> ["-x", f]) filters

getOsmandTracks :: FilePath -> String -> Action [FilePath]
getOsmandTracks dir date = getDirectoryFiles "" [dir </> date <> "*.gpx"]

getDataloggerTracks :: String -> FilePath -> String -> Action [FilePath]
getDataloggerTracks ext dir date = do
  src <- getDataloggerSources ext dir date
  return $ if null src then [] else [build "datalog" </> ext </> date <.> "gpx"]

-- fixme: using a glob to filter by date is wrong
-- because filenames have times in utc zone (i think)
getDataloggerSources :: String -> FilePath -> String -> Action [FilePath]
getDataloggerSources ext dir date = getDirectoryFiles "" [dir </> prefix <> "_" <> date' <> "*." <> ext]
  where
    prefix = "RODNEY_833000060"
    date' = undashesDate date

writeJsonFile :: ToJSON d => FilePath -> d -> Action ()
writeJsonFile f = writeFileChanged f . LT.unpack . decodeUtf8 . encode

renameTracksFaster :: String -> FilePath -> FilePath -> Action ()
renameTracksFaster name src dst = do
  Stdout s <- cmd "xml ed -N gpx=http://www.topografix.com/GPX/1/0 -u //gpx:name -v"
              [name, src]
  liftIO $ writeFile dst s
