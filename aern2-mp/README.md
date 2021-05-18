# aern2-mp  <!-- omit in toc -->

Variable-precision interval arithmetic

API documentation available on the [Hackage page](https://hackage.haskell.org/package/aern2-mp).

## Table of contents <!-- omit in toc -->

- [1. Numeric data types](#1-numeric-data-types)
- [2. Interval arithmetic with Prelude](#2-interval-arithmetic-with-prelude)
- [3. Interval arithmetic with MixedTypesNum](#3-interval-arithmetic-with-mixedtypesnum)
- [4. Internal types and backends](#4-internal-types-and-backends)
- [5. Specifications and tests](#5-specifications-and-tests)

## 1. Numeric data types

This package provides the following two data types:

- `Dyadic`:  variable-precision floats with exact ring operations
  
- `MPBall`: variable-precision interval (float centre ± error bound) with field & elementary interval operations
  
The type `MPBall` has instances of both [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) type classes such as `CanAdd`, `CanSqrt` as well as with traditional Prelude type classes such as `Ord`, `Num` and `Floating`.
The type `Dyadic` also has an appropriate subset of such instances.

Package [aern2-real](../aern2-real) provides an arithmetic of exact real numbers as converging lazy sequences of `MPBall`s of increasing precision.  Exact real numbers offer additional convenience and readability to validated numeric programming.

## 2. Interval arithmetic with Prelude

First, let us load the package with **Prelude** operations:

```Text
$ stack ghci aern2-mp:lib --no-load --ghci-options AERN2.MP
*AERN2.MP> import Prelude
*AERN2.MP Prelude>
```

We can work with `MPBall`s whose center is computed with a given **precision**, roughly corresponding to the number of significant bits:

```Text
...> pi100 = piBallP (prec 100)
...> pi10000 = piBallP (prec 10000)

...> sin pi100
[0.0000000000000000000000000000001694818351060767... ± ~7.8925e-31 ~2^(-99)]

...> sin pi10000
[0.0000000000000000000000000000000000000000000000... ± ~0.0000 ~2^(-9999)]
(0.07 secs, 64,466,432 bytes)
```

The Prelude power operator works only for integral types:

```Text
...> pi100 ^ 2
[9.8696044010893586188344909998725639610631902560... ± ~8.1120e-30 ~2^(-96)]

...> pi100 ^ pi100
<interactive>:18:1: error:
    • No instance for (Integral MPBall) arising from a use of ‘^’
```

Numerical order cannot be decided when the compared intervals overlap:

```Text
...> pi100 > 0
True

...> pi100 == pi100
*** Exception: Failed to decide equality of MPBalls.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of MPBalls returns Kleenean instead of Bool.
```

## 3. Interval arithmetic with MixedTypesNum

We see that some things do not work with Prelude. Let us use **MixedTypesNumPrelude** operations instead:

```Text
$ stack ghci aern2-mp:lib --no-load --ghci-options AERN2.MP
*AERN2.MP> import MixedTypesNumPrelude
*AERN2.MP MixedTypesNumPrelude>
```

We get a more general power operator:

```Text
...> pi100 = piBallP (prec 100)
...> pi10000 = piBallP (prec 10000)

...> pi100 ^ pi100
[36.4621596072079117709908260226198218149834948802... ± ~1.8696e-28 ~2^(-92)]

...> pi10000 ^ pi10000
[36.4621596072079117709908260226921236663655084022... ± ~0.0000 ~2^(-9992)]
(0.27 secs, 204,657,248 bytes)
```

Interval comparison now returns a `Kleenean` instead of `Bool`, supporting undecided comparisons:

```Text
...> pi100 > 0
CertainTrue

...> pi100 == pi100
TrueOrFalse
```

## 4. Internal types and backends

The type `MPBall` internally uses the type:

- `MPFloat`: arbitrary-precision floats with both upwards and downwards-rounded arithmetic operations such as `*^` and `*.`

The package uses [cdar-mBound](https://hackage.haskell.org/package/cdar-mBound), a fork of [cdar](https://github.com/jensblanck/cdar) as its backend for `Dyadic` and `MPFloat`.

In previous versions, there was an MPFR backend via [rounded](https://hackage.haskell.org/package/rounded).  This may be added again in future.

## 5. Specifications and tests

This package also provides a fairly complete hspec/QuickCheck specification of algebraic properties for the above types.  

For `MPFloat`, the properties are given mostly as approximate versions of algebraic equalities with a small rounding error tolerance.  

For `MPBall`, the properties are given mostly as (interval) set over-approximations of the usual algebraic equalities.
