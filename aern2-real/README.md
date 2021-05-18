# aern2-real <!-- omit in toc -->

Exact real arithmetic

API documentation available on the [Hackage page](https://hackage.haskell.org/package/aern2-real).

## Table of contents <!-- omit in toc -->

- [1. Numeric data types](#1-numeric-data-types)
- [2. Basic usage with Prelude](#2-basic-usage-with-prelude)
- [3. Basic usage with MixedTypesNumPrelude](#3-basic-usage-with-mixedtypesnumprelude)
- [4. Partial functions and error handling](#4-partial-functions-and-error-handling)
- [5. Limits](#5-limits)
- [6. Multivalued selection](#6-multivalued-selection)
  - [6.1. Parallel branching](#61-parallel-branching)
  - [6.2. Multi-valued selection](#62-multi-valued-selection)
- [7. Specification and tests](#7-specification-and-tests)

## 1. Numeric data types

This package provides the following two data types:

- `CReal`:  Exact real numbers via lazy sequences of interval approximations
  
- `CKleenean`: Lazy Kleeneans, naturally arising from comparisons of `CReal`s
  
The type `CReal` has instances of both [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) type classes such as `CanAdd`, `CanSqrt` as well as with traditional Prelude type classes such as `Ord`, `Num` and `Floating`.
The type `CKleenean` supports the usual Boolean operations.

## 2. Basic usage with Prelude

First, let us load the package with **Prelude** operations:

```Text
$ stack ghci aern2-real:lib --no-load --ghci-options AERN2.Real
*AERN2.MP> import Prelude hiding (pi)
*AERN2.MP Prelude>
```

We can obtain approximations of a real number with any **requested accuracies**:

```Text
...> pi ? (bits 1000)
[3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117... ± ~0.0000 ~2^(-1230)]

...> pi ? (bits 1000000)
[3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117... ± ~0.0000 ~2^(-1028468)]
(4.12 secs, 270,972,152 bytes)
```

Instead of accuracy, we can request that the computation is performed with a certain **precision**, which roughly corresponds to the number of significant bits.  This usually trades speed with guaranteed accuracy:

```Text
...> (sin pi) ? (bits 10000) -- guaranteed accuracy at least 10000
[-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000... ± ~0.0000 ~2^(-13539)]
(0.27 secs, 196,580,192 bytes)

...> (sin pi) ? (prec 10000) -- no guaranteed accuracy
[-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000... ± ~0.0000 ~2^(-13539)]
(0.21 secs, 107,844,784 bytes)
```

When formatting a real number, a **default precision** is used:

```Text
...> pi
{?(prec 36): [3.141592653584666550159454345703125 ± ~1.4552e-11 ~2^(-36)]}
```

The Prelude power operator works only for integral types:

```Text
...> pi ^ 2
[9.8696044010893586188344909998725639610631902560... ± ~8.1120e-30 ~2^(-96)]
{?(prec 36): [9.8696044009993784129619598388671875 ± ~1.4964e-10 ~2^(-32)]}

...> pi ^ pi
<interactive>:18:1: error:
    • No instance for (Integral CReal) arising from a use of ‘^’
```

Numerical order cannot be decided when the two numbers are equal:

```Text
...> pi > 0
True

...> pi == pi
*** Exception: Failed to decide equality of Sequences.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of Sequences returns CSequence Kleenean or similar instead of Bool.
```

Prelude comparison fails to determine also inequality when the two numbers are very close:

```Text
...> pi == pi + 0.00000000000000000000000000000000001
False

...> pi == pi + 0.00000000000000000000000000000000000001
*** Exception: Failed to decide equality of Sequences.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of Sequences returns CSequence Kleenean or similar instead of Bool.
```

## 3. Basic usage with MixedTypesNumPrelude

We see that some things do not work with Prelude. Let us use **MixedTypesNumPrelude** operations instead:

```Text
$ stack ghci aern2-real:lib --no-load --ghci-options AERN2.Real
*AERN2.MP> import MixedTypesNumPrelude
*AERN2.MP MixedTypesNumPrelude>
```

We get a more general power operator:

```Text
...> 2^0.5
{?(prec 36): [1.414213562371930730340852514178195642186126256312482171419747717302107387071785637999710161238908... ± ~1.0305e-11 ~2^(-36)]}

...> pi ^ pi
{?(prec 36): [36.462159605538498632520418490483602438877178488347481362195878876490337527904728176508797332644462... ± ~2.7112e-9 ~2^(-28)]}

...> (pi ^ pi) ? (bits 10000)
[36.462159607207911770990826022692123666365508402228818738709335922934074368881699904620079875706774... ± ~0.0000 ~2^(-13532)]
(0.90 secs, 631,865,912 bytes)
```

Real comparison now returns a `CKleenean` instead of `Bool`, supporting undecided comparisons and comparisons with a specified precision:

```Text
...> pi > 0
{?(prec 36): CertainTrue}

...> pi == pi
{?(prec 36): TrueOrFalse}

...> pi == pi + 2^(-100)
{?(prec 36): TrueOrFalse}

...> (pi == pi + 2^(-100)) ? (prec 1000)
CertainFalse
```

## 4. Partial functions and error handling

Since comparisons can be only semi-decided, also errors such as division by zero or logarithm of a negative number can be only semi-detected.
Therefore, an invalid input leads to a normal `CReal` value, and the error is demonstrated only when we extract an approximation, and sometimes an error cannot be determined with certainty:

```Text
...> bad1 = pi/0
...> bad1 ? (prec 100)
{{ERROR: division by 0}}}

...> bad2 = 1/(pi-pi)
...> bad2 ? (prec 100)
{{POTENTIAL ERROR: division by 0}}
```

When we are sure that potential errors are harmless, we can clear them:

```Text
...> ok3 = sqrt (pi-pi)
...> ok3 ? (prec 10)
[0.022097086912079610143710452219156792352805496193468570709228515625 ± ~2.2097e-2 ~2^(-5)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}

...> ok4 = clearPotentialErrors $ sqrt (pi-pi)
...> ok4 ? (prec 10)
[0.022097086912079610143710452219156792352805496193468570709228515625 ± ~2.2097e-2 ~2^(-5)]
```

## 5. Limits

Computing a limit of a fast converging sequence of numbers or functions is one of the most fundamental operations for real numbers.
For example, we can compute `e` as the limit of the partial sums of terms `1/n!` for `n` ranging from `0` onwards:

```Text
... MixedTypesNumPrelude> fact n = creal $ product [1..n]
... MixedTypesNumPrelude> e_sum n = sum $ map (recip . fact) [0..n]
```

TODO

## 6. Multivalued selection

When a comparison is needed for branching, its semi-decidability becomes a challenge.  As an example, consider the task of defining the `abs` function by cases.
We have two ways to overcome the challenge:

### 6.1. Parallel branching

```Text
... MixedTypesNumPrelude> abs1 x = if x < 0 then -x else x
... MixedTypesNumPrelude> abs1 (pi - pi)
{?(prec 36): [0 ± ~2.9104e-11 ~2^(-35)]}
```

This simple definition works even when x = 0 because AERN2 has redefined the if-then-else operator for a `CKleenean` condition and real number branches in such a way that in situations where the condition is inconclusive, both branches are computed and the results safely merged.  This is convenient, but can lead to inefficient code if the number of branches that need to be considered grows large.

### 6.2. Multi-valued selection

TODO

## 7. Specification and tests

The approximations obtained using `? (bits n)` or `? (prec p)` are intervals of type `CN MPBall` from package [aern2-mp](../aern2-mp/README.md).  This type is also used internally for all `CReal` arithmetic.  The `MPBall` arithmetic is tested against a fairly complete hspec/QuickCheck specification of algebraic properties.
