# aern2-mp

## Numeric data types

* `Dyadic`:  variable-precision floats with exact ring operations
  
* `MPBall`: arbitrary-precision float ± error bound with field & elementary interval-like operations
  
The type `MPBall` has instances of both [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) type classes such as `CanAdd`, `CanSqrt` as well as with traditional Prelude type classes such as `Ord`, `Num` and `Floating`.
The type `Dyadic` also has an appropriate subset of such instances.  

### Internal types and backends

The type `MPBall` internally uses the type:

* `MPFloat`: arbitrary-precision floats with both upwards and downwards-rounded arithmetic operations such as `*^` and `*.`

The package uses [this fork](https://github.com/michalkonecny/cdar) of [cdar](https://github.com/jensblanck/cdar) as its backend for `Dyadic` and `MPFloat`.

In previous versions there was an MPFR backend via [rounded](https://hackage.haskell.org/package/rounded).  This may be added again in future.

## Specifications and tests

This package also provides a fairly complete hspec/QuickCheck specification of algebraic properties for the above types.  

For `MPFloat`, the properties are given mostly as approximate versions of algebraic equalities with a small rounding error tolerance.  

For `MPBall`, the properties are given mostly as (interval) set over-approximations of the usual algebraic equalities.
