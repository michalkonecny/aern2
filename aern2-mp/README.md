# aern2-mp

This package provides the following types:
  
  * `Dyadic`:  variable-precision floats with exact ring operations
  .
  * `MPBall`: float ± error bound with field & elementary interval-like operations
  
The types have instances of both [MixedTypeNumPrelude](https://hackage.haskell.org/package/mixed-types-num) type classes as well as with traditional Prelude type classes.

There is a plan to add an Integer-only backend so that aern2-mp can be used without MPFR.
