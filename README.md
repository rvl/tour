# Tour Browser

Loads GPX logs and tour data and makes a website.

View online: https://lorrimar.id.au/tour/

I have written a report about the development of this website and how
the code works on my blog:

http://rodney.id.au/posts/2017-10-29-tour-map/


## Data build

### Setup

Data files are linked in manually.
```
ln -s ~/Documents/tracks/*.gpx tracks
ln -s ~/Documents/GPS_DATA/*.SBN GPS_DATA
```

### Start build

```
nix-shell --run ./build.sh --keep-going
```

## Frontend build

```
nix-shell -A frontend
cabal configure --ghcjs --builddir=dist-ghcjs
cabal build --builddir=dist-ghcjs
```

## Serve frontend

```
nix-shell --run "runghc DevServer.hs"
```

## Backend build

This is not building at the moment, and "isomorphic" version of this
app doesn't really make sense anyway.

```
nix-shell frontend.nix --argstr compiler default
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
