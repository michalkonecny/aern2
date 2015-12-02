# aern2 installation

A way to install aern2:

* Install [ghc 7.8.*](https://www.haskell.org/ghc/download_ghc_7_8_4) and cabal-install (eg via [haskell-platform 2014.2.0.0](https://downloads.haskell.org/~platform/2014.2.0.0/)).
* Install [michalkonecny/haskell-mpfr](https://github.com/michalkonecny/haskell-mpfr) (a fork of [cornius/haskell-mpfr](https://github.com/comius/haskell-mpfr), which is a fork of [ekmett/rounded
](https://github.com/ekmett/rounded)):
  * > git clone git@github.com:michalkonecny/haskell-mpfr.git
  * > cd haskell-mpfr
  * Optionally, connect this folder to a cabal sandbox.
  * > cabal install
  * This package will fail to install if you do not have automake-1.15.  Nevertheless, it can be made to work with automake-1.14 (which comes with Ubuntu 14.04 LTS), eg using symbolic links for the binaries.
* Install aern2.
  * > cd ..
  * > git clone git@github.com:michalkonecny/aern2.git
  * > cd aern2/aern2-real
  * Optionally, connect this folder to a cabal sandbox.
  * > cabal install
