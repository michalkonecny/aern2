# AERN2

A set of Haskell packages for exact real number computation.

## A quick start guide

  * [Installation guide](docs/install.md)

  * run `stack repl aern2/aern2-demos/src/Demos.hs`

  * follow [documentation for MixedTypesNumPrelude](https://michalkonecny.github.io/mixed-types-num/MixedTypesNumPrelude.html)

  * read and play with [Demos.hs](aern2-demos/src/Demos.hs).

## Library packages

  * [aern2-mp](http://michalkonecny.github.io/aern2/aern2-mp-0.1.0.0/): Multiple-precision ball arithmetic based on MPFR (fairly complete)

  * [aern2-real](http://michalkonecny.github.io/aern2/aern2-real-0.1.0.0/): Cauchy real numbers (fairly complete)

  * [aern2-fun](http://michalkonecny.github.io/aern2/aern2-fun-0.1.0.0/): Real functions and their polynomial enclosures (work in progress, not built by default)

  * [aern2-fun-plot](http://michalkonecny.github.io/aern2/aern2-fun-plot-0.1.0.0/): Plotting of real functions in browser (currently broken)

## Executables

  * [aern2-fnreps](https://github.com/michalkonecny/aern2/tree/master/aern2-fnreps): Benchmark comparison of representations of continuous functions

  * `aern2-bench-chart`: Produce performance charts using data from a csv file.  Used internally, eg in `aern2-fnreps` and `aern2-fun`.

## Significant dependencies

  * [mixed-types-num](https://michalkonecny.github.io/mixed-types-num/MixedTypesNumPrelude.html):
    Bottom-up-typed numerical expressions

  * [hmpfr](https://hackage.haskell.org/package/hmpfr):
    A binding to the C [MPFR](http://www.mpfr.org/) (Multiple-Precision Floating-point computations with correct Rounding) library

  * [rounded](https://github.com/michalkonecny/rounded):
    A newer and faster binding to [MPFR](http://www.mpfr.org/).
    See [aern2-mp](http://michalkonecny.github.io/aern2/aern2-mp-0.1.0.0/) for information on how to switch to this backend.

## Other references

  * [haskell-reals-comparison](https://github.com/michalkonecny/haskell-reals-comparison/):
    Benchmark(s) comparing the performance of Haskell libraries for exact real computation (work in progress)
