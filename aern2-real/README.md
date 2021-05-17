# aern2-real

Exact real arithmetic

## Numeric data types

This package provides the following two data types:

* `CReal`:  Exact real numbers via lazy sequences of interval approximations
  
* `CKleenean`: Lazy Kleeneans, naturally arising from comparisons of `CReal`s
  
The type `CReal` has instances of both [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num) type classes such as `CanAdd`, `CanSqrt` as well as with traditional Prelude type classes such as `Ord`, `Num` and `Floating`.
The type `CKleenean` supports the usual Boolean operations.

### Examples

First, let us test exact real arithmetic with Prelude operations:

    $ stack ghci aern2-real:lib --no-load --ghci-options AERN2.Real
    *AERN2.MP> import Prelude hiding (pi)
    *AERN2.MP Prelude>

    ...> pi
    {?(prec 36): [3.141592653584666550159454345703125 ± ~1.4552e-11 ~2^(-36)]}

    ...> pi ? (bits 1000)
    [3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117... ± ~0.0000 ~2^(-1230)]

    ...> pi ? (bits 1000000)
    [3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117... ± ~0.0000 ~2^(-1028468)]
    (4.12 secs, 270,972,152 bytes)

    ...> pi ^ 2
    [9.8696044010893586188344909998725639610631902560... ± ~8.1120e-30 ~2^(-96)]
    {?(prec 36): [9.8696044009993784129619598388671875 ± ~1.4964e-10 ~2^(-32)]}

    ...> pi ^ pi
    <interactive>:18:1: error:
        • No instance for (Integral CReal) arising from a use of ‘^’

    ...> sin pi
    {?(prec 36): [0.000000000005126565838509122841060161590576171875 ± ~1.4559e-11 ~2^(-35)]}

    ...> (sin pi) ? (bits 10000) -- guaranteed accuracy at least 10000
    [-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000... ± ~0.0000 ~2^(-13539)]
    (0.21 secs, 196,580,192 bytes)

    ...> (sin pi) ? (prec 10000) -- no guaranteed accuracy
    [-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000... ± ~0.0000 ~2^(-13539)]
    (0.13 secs, 107,844,784 bytes)

    ...> pi > 0
    True

    ...> pi == pi
    *** Exception: Failed to decide equality of Sequences.  If you switch to MixedTypesNumPrelude instead of Prelude, comparison of Sequences returns CSequence Kleenean or similar instead of Bool.

Some things do not work with Prelude. Let us try using MixedTypesNumPrelude operations:

    $ stack ghci aern2-real:lib --no-load --ghci-options AERN2.Real
    *AERN2.MP> import MixedTypesNumPrelude
    *AERN2.MP MixedTypesNumPrelude>

    ...> pi ^ pi
    {?(prec 36): [36.462159605538498632520418490483602438877178488347481362195878876490337527904728176508797332644462... ± ~2.7112e-9 ~2^(-28)]}

    ...> (pi ^ pi) ? (bits 10000)
    [36.462159607207911770990826022692123666365508402228818738709335922934074368881699904620079875706774... ± ~0.0000 ~2^(-13532)]
    (0.81 secs, 631,865,912 bytes)

    ...> pi > 0
    {?(prec 36): CertainTrue}

    ...> pi == pi
    {?(prec 36): TrueOrFalse}

    ...> pi == pi + 2^(-100)
    {?(prec 36): TrueOrFalse}

    ...> (pi == pi + 2^(-100)) ? (prec 1000)
    CertainFalse

    ...> 2^0.5
    {?(prec 36): [1.414213562371930730340852514178195642186126256312482171419747717302107387071785637999710161238908... ± ~1.0305e-11 ~2^(-36)]}

## Partial functions and error handling

Errors due to invalid input, such as division by zero or logarithm of a negative number can be only semi-detected in the same way as comparisons can be only semi-decided.
Therefore, an invalid input gives a `CReal` leads to errors or potential errors only when extracting an approximation:

    ...> bad1 = pi/0
    ...> bad1 ? (prec 100)
    {{ERROR: division by 0}}}

    ...> bad2 = 1/(pi-pi)
    ...> bad2 ? (prec 100)
    {{POTENTIAL ERROR: division by 0}}

When we are sure that potential errors are harmless, we can clear them:

    ...> ok3 = sqrt (pi-pi)
    ...> ok3 ? (prec 10)
    [0.022097086912079610143710452219156792352805496193468570709228515625 ± ~2.2097e-2 ~2^(-5)]{{POTENTIAL ERROR: out of domain: negative sqrt argument}}
    ...> clearPotentialErrors $ ok3 ? (prec 10)
    [0.022097086912079610143710452219156792352805496193468570709228515625 ± ~2.2097e-2 ~2^(-5)]

## Specification and tests

The approximations obtained using `? (bits n)` or `? (prec p)` are intervals of type `CN MPBall` from package [aern2-mp](../aern2-mp/README.md).  This type is also used internally for all `CReal` arithmetic.  The `MPBall` arithmetic is tested against a fairly complete hspec/QuickCheck specification of algebraic properties.
