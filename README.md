# AERN2

A set of Haskell packages for exact real number computation.

## A quick start guide

  * [Installation guide](docs/install.md)

  * run `stack repl aern2/aern2-demos/src/Introduction.hs`

  * follow [documentation for MixedTypesNumPrelude](https://hackage.haskell.org/package/mixed-types-num/docs/MixedTypesNumPrelude.html)

  * read and play with [Introduction.hs](aern2-demos/src/Introduction.hs).

## Library packages

  * [aern2-mp](https://hackage.haskell.org/package/aern2-mp): Multiple-precision ball arithmetic based on MPFR (fairly complete)

  * [aern2-real](http://hackage.haskell.org/package/aern2-real): Cauchy real numbers (fairly complete)

  * aern2-fun: Real functions and their polynomial enclosures (work in progress, not built by default)

  * aern2-fun-plot: Plotting of real functions in browser (currently broken)

## Executables

  * [aern2-fnreps](https://github.com/michalkonecny/aern2/tree/master/aern2-fnreps): Benchmark comparison of representations of continuous functions

  * `aern2-bench-chart`: Produce performance charts using data from a csv file.  Used internally, eg in `aern2-fnreps` and `aern2-fun`.

## Significant dependencies

  * [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num):
    Bottom-up-typed numerical expressions

  * [hmpfr](https://hackage.haskell.org/package/hmpfr):
    A binding to the C [MPFR](http://www.mpfr.org/) (Multiple-Precision Floating-point computations with correct Rounding) library

  * [rounded](https://github.com/michalkonecny/rounded):
    A newer and faster binding to [MPFR](http://www.mpfr.org/).
    See [aern2-mp](https://hackage.haskell.org/package/aern2-mp) for information on how to switch to this backend.

## Other references

  * [haskell-reals-comparison](https://github.com/michalkonecny/haskell-reals-comparison):
    Benchmark(s) comparing the performance of Haskell libraries for exact real computation (work in progress)
