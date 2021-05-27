# aern2-real <!-- omit in toc -->

Exact real arithmetic

API documentation is available on the [Hackage page](https://hackage.haskell.org/package/aern2-real).

The remainder of this text is an introductory tutorial.  The code for the examples contained here is also available in file [Introduction.hs](src/AERN2/Real/Introduction.hs).

## Table of contents <!-- omit in toc -->

- [1. Data types](#1-data-types)
- [2. Usage with Prelude](#2-usage-with-prelude)
- [3. Usage with MixedTypesNumPrelude](#3-usage-with-mixedtypesnumprelude)
- [4. Partial functions and error handling](#4-partial-functions-and-error-handling)
- [5. Limits](#5-limits)
- [6. Multivalued selection](#6-multivalued-selection)
  - [6.1. Parallel branching](#61-parallel-branching)
  - [6.2. Multi-valued selection](#62-multi-valued-selection)
- [7. Specification and tests](#7-specification-and-tests)

## 1. Data types

This package provides the following two data types:

- `CReal`:  Exact real numbers via lazy sequences of interval approximations
  
- `CKleenean`: Lazy Kleeneans, naturally arising from comparisons of `CReal`s
  
The type `CReal` has instances of both [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) type classes such as `CanAdd`, `CanSqrt` as well as with traditional Prelude type classes such as `Ord`, `Num` and `Floating`.
The type `CKleenean` supports the usual Boolean operations.

Real numbers are represented by converging sequences of dyadic intervals:

```Haskell
type CReal = CSequence MPBall
```

A `CSequence` is a list of approximations computed with increasing *precision*.
Precision here does *not* guarantee a certain *accuracy*.
Precision roughly corresponds to the number of *significant digits* used
in all intermediate computations.
With increasing precision the intervals eventually converge to exact values.

The elements of a `CSequence` use the `CN` error-collecting wrapper.
A convergent sequence must be error-free from some point onwards.
A sequence is allowed not to converge, but only if all its elements contain the same error.  
Such a sequence can be thought of as converging to this error.

## 2. Usage with Prelude

First, let us load the package with **Prelude** operations:

```Text
$ stack ghci aern2-real:lib --no-load --ghci-options "AERN2.Real -Wno-type-defaults"
*AERN2.Real> import Prelude hiding (pi)
*AERN2.Real Prelude>
```

We can obtain approximations of a real number with a chosen *precision*:

```Text
...> (sin 1 ::CReal) ? (prec 120)
[0.84147098480789650665250232... ± ~4.6644e-35 ~2^(-114)]

...> (sin 1 ::CReal) ? (prec 10000)
[0.84147098480789650665250232... ± ~0.0000 ~2^(-13530)]
```

Notice that sometimes the accuracy of the interval is lower than the working precision.  Instead of precision, we can request that the computation is performed with a certain *guaranteed accuracy*:

```Text
...> (sin 1 ::CReal) ? (bits 120)
[0.84147098480789650665250232... ± ~2.2431e-55 ~2^(-181)]

Nevertheless, this sometimes comes with a performance penalty, since internally the computation may need to be restarted with a higher accuracy:

...> sumSines n = sum [sin (creal i) | i <- [1..n::Integer]]

...> sumSines 100 ? (prec 120)
[-0.12717101366042011543675217... ± ~2.8393e-33 ~2^(-108)]
(0.03 secs, 26,203,776 bytes)

...> sumSines 100 ? (bits 120)
[-0.12717101366042011543675217... ± ~1.2220e-53 ~2^(-175)]
(0.05 secs, 60,537,128 bytes)
```

Which can be obtained faster if directly guessing that we need precision at least 130:

```Text
...> (sumSines1 100) ? (prec 130)
[-0.12717101366042011543675217... ± ~1.2220e-53 ~2^(-175)]
(0.03 secs, 35,209,088 bytes)
```

When formatting a real number, a *default precision* is used:

```Text
...> pi
{?(prec 36): [3.14159265358466655015945434... ± ~1.4552e-11 ~2^(-36)]}
```

The **Prelude** power operator works only for integral types:

```Text
...> pi ^ 2
{?(prec 36): [9.86960440099937841296195983... ± ~1.4964e-10 ~2^(-32)]}

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

## 3. Usage with MixedTypesNumPrelude

We see that some things do not work with Prelude. Let us use [MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num) operations instead:

```Text
$ stack ghci aern2-real:lib --no-load --ghci-options AERN2.Real
*AERN2.Real> import MixedTypesNumPrelude
*AERN2.Real MixedTypesNumPrelude>
```

First, our Prelude expressions

- `(sin 1 :: CReal)`
- `sum [sin (creal i) | i <- [1..n::Integer]]`

can now be simplified as follows:

```Text
...> :t sin 1
sin 1 :: CReal

...> sumSines n = sum [sin i | i <- [1..n]]
...> :t sumSines
sumSines :: Integer -> CReal
```

Moreover, we get a more general power operator:

```Text
...> 2^0.5
{?(prec 36): [1.41421356237193073034085251... ± ~1.0305e-11 ~2^(-36)]}

...> pi ^ pi
{?(prec 36): [36.46215960553849863252041849... ± ~2.7112e-9 ~2^(-28)]}

...> (pi ^ pi) ? (bits 10000)
[36.46215960720791177099082602... ± ~0.0000 ~2^(-13532)]
(0.83 secs, 631,232,904 bytes)
```

Real comparison now returns a `CKleenean` instead of `Bool`, where

```Haskell
type CKleenean = CSequence Kleenean
```

As a three-value truth type, `Kleenean` supports undecided comparisons.  Being a sequence, `CKleenean` supports comparisons with a specified precision:

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

When the numbers are known exactly, an equality test succeeds:

```Test
...> (creal 0) == 0
{?(prec 36): CertainTrue}
```

## 4. Partial functions and error handling

Normally in Haskell, computation such as `1/0` or `sqrt (-1)` result in **NaN** or run-time exceptions.
Since `CReal` uses the [CN wrapper](https://hackage.haskell.org/package/collect-errors), for `CReal` these expressions instead return special values that describe the error.

Since comparisons can be only semi-decided, also such errors can be only semi-detected.
Therefore, an invalid input leads to a normal `CReal` value, and the error is demonstrated only when we extract an approximation:

```Text
...> bad1 = sqrt (-1)
...> bad1 ? (prec 100)
{{ERROR: out of domain: negative sqrt argument}}
```

and sometimes an error cannot be determined with certainty:

```Text
...> a_third = creal (1/3)

...> bad2 = 1/(a_third-a_third)
...> bad2 ? (prec 100)
{{POTENTIAL ERROR: division by 0}}

...> bad2 ? (bits 100)
{{POTENTIAL ERROR: numeric error: failed to find an approximation with sufficient accuracy}}
```

A query for guaranteed precision may take a long time because before it fails, the computation is attempted iteratively for higher and higher precisions, up to precision around 5,000,000 bits:

```Text
...> bad3 = 1/(pi-pi)
...> bad3 ? (prec 100)
{{POTENTIAL ERROR: division by 0}}

...> bad3 ? (bits 100)
-- TAKES A VERY LONG TIME
```

When we are sure that potential errors are harmless, we can clear them:

```Text
...> ok4 = sqrt (pi-pi)
...> ok4 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}}

...> ok5 = clearPotentialErrors $ sqrt (pi-pi)
...> ok5 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]
```

Attempting to clear a certain error is harmless:

```Text
...> bad6 = clearPotentialErrors (sqrt (pi-pi-1))
...> bad6 ? (prec 100)
{{ERROR: out of domain: negative sqrt argument}}
```

But clearing a potential error which is a real error is unsound:

```Text
...> bad7 = clearPotentialErrors (sqrt (pi-pi-2^(-1000)))
...> bad7 ? (prec 100)
[0.00000000000000000061331736... ± ~6.1332e-19 ~2^(-60)]
...> bad7 ? (prec 1000)
{{ERROR: out of domain: negative sqrt argument}}
```

Errors can be investigated, eg as follows:

```Text
...> detectCN r = if not (CN.hasError r) then Just r else Nothing

...> detectCN (sqrt (-1) ? (prec 100))
Nothing

...> detectCN (sqrt 0 ? (prec 100))
Just [0 ± 0]
```

There is also `CN.hasCertainError` which ignores potential errors.

## 5. Limits

Computing a limit of a fast converging sequence of numbers or functions is one of the most fundamental operations for real numbers.
A sequence `a_n` is fast converging if each
`a_n` is no more than `0.5^n` distant from the limit.

For example, we can compute `e` as the limit of the partial sums of terms `1/n!` for `n` ranging from `0` onwards:

```Text
... MixedTypesNumPrelude> fact n = creal $ product [1..n]
... MixedTypesNumPrelude> e_sum n = sum $ map (recip . fact) [0..n]
```

The difference between `e` and `e_sum n` is no more than `3/(fact (n+1))` which is less than `0.5^(n-2)`.
Thus the sequence `\n -> e_sum (n+2)` is fast converging and the following limit is valid:

```Text
...> my_e = limit $ \(n :: Integer) -> e_sum (n+2)

...> my_e ? (prec 1000)
[2.71828182845904523536028747... ± ~0.0000 ~2^(-1217)]
```

The type declaration for `n` is required because `limit` is generic and works also for sequences indexed by `Int` or even positive rational numbers.

## 6. Multivalued selection

When a comparison is needed for branching, its semi-decidability becomes a challenge.  As an example, consider the task of defining the `abs` function by cases.
We have two ways to overcome the challenge:

### 6.1. Parallel branching

```Text
...> absR1 x = if x < 0 then -x else x

...> absR1 (pi - pi)
{?(prec 36): [0 ± ~2.9104e-11 ~2^(-35)]}
```

This simple definition works even when x = 0 because AERN2 has redefined the if-then-else operator for a `CKleenean` condition and real number branches in such a way that in situations where the condition is inconclusive, both branches are computed and the results safely merged.  This is convenient, but can lead to inefficient code if the number of branches that need to be considered grows large.

### 6.2. Multi-valued selection

A more general mechanism for dealing with branching based on semi-decidable conditions such as real-number comparisons is non-deterministic `select`. If given two lazy Kleeneans, `select` will enquire them concurrently with increasing precisions until one of them becomes `CertainTrue`.  By convention `select` returns a `Bool` which is `True` if the first branch succeeds and `False` if the second branch succeeds.  

Here we use `select` to implement a *soft* sign test with some tolerance `eps` and define `absR2` to be the limit of a sequence of approximate implementations of `abs` with different `eps`:

```Text
...> absR2_approx x (q :: Rational) = if select (x > -q) (x < q) then x else -x

...> absR2 x = limit $ absR2_approx x

...> absR2 (pi - pi)
{?(prec 36): [0 ± ~4.3656e-11 ~2^(-34)]}
```

## 7. Specification and tests

Most `CReal` operations are simply lifts of the corresponding `CN MPBall` operations, which are tested in package [aern2-mp](../aern2-mp/README.md) against a fairly complete hspec/QuickCheck specification of algebraic properties.

There are also tests specific to `CReal`, mostly checking that `? (bits n)` queries return sufficiently accurate approximations.  There is are tests for `select` and `limit`.
