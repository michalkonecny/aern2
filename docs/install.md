# Installation

## Installing Haskell compiler, package manager and IDE support

- The following method has been tested:
  - Install [GHCup](https://www.haskell.org/ghcup/)
  - Using GHCup, install ghc 9.02, stack, and HLS if needed for IDE support

## Installing official releases

- The packages [aern2-mp](https://hackage.haskell.org/package/aern2-mp) and [aern2-real](https://hackage.haskell.org/package/aern2-real) are available via Hackage and Stackage and also on some Linux distributions.

- A package manager will install these packages automatically if they are listed among dependencies.

## Installing from GitHub sources

- git clone `https://github.com/michalkonecny/aern2.git`
- Build the packages using stack:
  - (optional) Check that the provided `stack.yaml` meets your needs.
  - Run `stack install` in the base folder.
  - Run `stack test aern2-mp` in the base folder.
