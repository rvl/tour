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
nix-shell app.nix
cabal configure --ghcjs --builddir=dist-ghcjs
cabal build --builddir=dist-ghcjs
sassc frontend/tour.sass frontend/static/tour.css
```

## Serve frontend

```
nix-shell --run "runghc DevServer.hs"
```

## Backend build

This is not building at the moment, and "isomorphic" version of this
app doesn't really make sense anyway.

```
nix-shell app.nix --argstr compiler default
cabal configure
cabal build
```

## Regenerating nix

If adding cabal dependencies, regenerate nix derivations and restart
shells.

```
cabal2nix . > tour-ghc.nix
cabal2nix --compiler ghcjs . > tour-ghcjs.nix
```
