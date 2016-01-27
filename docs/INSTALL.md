# aern2 installation

The following steps have been tested on Ubuntu 14.04:

* Install ghc 7.8.4 and cabal-install.
  * I recommend [these instructions](https://github.com/bitemyapp/learnhaskell/blob/master/install.md).
* Setup cabal sandbox
  * `> cd <an empty folder>`
  * `> cabal sandbox init`
* Install [cornius/haskell-mpfr](https://github.com/comius/haskell-mpfr), a fork of [ekmett/rounded
](https://github.com/ekmett/rounded):
  * `> git clone git@github.com:cornius/haskell-mpfr.git`
  * `> cd haskell-mpfr`
  * `> ln -s ../cabal.sandbox.config .`
  * `> cabal install`
* Install aern2-num:
  * `> cd ..`
  * `> git clone git@github.com:michalkonecny/aern2.git`
  * `> cd aern2/aern2-num`
  * `> ln -s ../../cabal.sandbox.config .`
  * `> cabal install`
