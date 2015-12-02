# aern2 installation

The following steps have been tested on Ubuntu 14.04:

* Install ghc 7.8.4 and cabal-install.
  * I recommend [these instructions](https://github.com/bitemyapp/learnhaskell/blob/master/install.md).
* Setup cabal sandbox
  * `> cd <an empty folder>`
  * `> cabal sandbox init`
* Install [michalkonecny/haskell-mpfr](https://github.com/michalkonecny/haskell-mpfr) (a fork of [cornius/haskell-mpfr](https://github.com/comius/haskell-mpfr), which is a fork of [ekmett/rounded
](https://github.com/ekmett/rounded)):
  * `> git clone git@github.com:michalkonecny/haskell-mpfr.git`
  * `> cd haskell-mpfr`
  * `> ln -s ../cabal.sandbox.config .`
  * ``> cabal sandbox add-source `pwd```
  * `> cabal install`
  * This package will fail to install if you do not have automake-1.15.  Nevertheless, it can be made to work with automake-1.14 (which comes with Ubuntu 14.04 LTS), eg using symbolic links for the binaries.
* Install aern2-real:
  * `> cd ..`
  * `> git clone git@github.com:michalkonecny/aern2.git`
  * `> cd aern2/aern2-real`
  * `> ln -s ../../cabal.sandbox.config .`
  * `> cabal install`
