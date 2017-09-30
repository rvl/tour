# Tour Browser

Loads GPX logs and tour data and makes a website.

## Data build

### Setup

Data files are linked in manually.
```
ln -s ~/Documents/tracks/*.gpx tracks
ln -s ~/Documents/GPS_DATA/*.SBN GPS_DATA
```

### Build

```
nix-shell --run ./build.sh
```

## Frontend build

```
nix-shell frontend.nix
cabal configure --ghcjs
cabal build
cp frontend/index.html dist/build/tour/tour.jsexe/
cd dist/build/tour/tour.jsexe
python3 -m http.server --bind localhost 8000
```
