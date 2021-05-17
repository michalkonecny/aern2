# AERN2

A set of Haskell packages for exact real number computation.

## A quick start guide

  * [Installation guide](docs/install.md)

  * read and follow examples in [aern2-real README](aern2-real/README.md)

## Library packages

  * [aern2-mp](aern2-mp/README.md): Multiple-precision ball arithmetic based on MPFR (fairly complete)

  * [aern2-real](aern2-real/README.md): Cauchy real numbers (fairly complete)

  * aern2-fun: Real functions (work in progress, not built by default)

  * aern2-fun-univariate: Continuous real functions of one variable and their polynomial enclosures (work in progress, not built by default)

## Executables

  * aern2-fnreps: Benchmarks evaluating various representations of univariate continuous real functions (see branch [fnreps2020](https://github.com/michalkonecny/aern2/tree/fnreps2020/aern2-fnreps))

  * `aern2-bench-chart`: Produce performance charts using data from a csv file.  Used internally, eg in `aern2-fnreps` and `aern2-fun`.

## Significant dependencies

  * [collect-errors](https://hackage.haskell.org/package/collect-errors):
    Type wrapper `CN` for gracefully propagating numerical errors through expressions

  * [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num):
    Bottom-up-typed numerical expressions

  * [cdar-mBound](https://hackage.haskell.org/package/cdar-mBound):
    Exact real arithmetic built on floating-point-like intervals based on Haskell `Integer`s.  AERN2 currently uses the types `Dyadic` and `Approx`.

## Other references

  * [haskell-reals-comparison](https://github.com/michalkonecny/haskell-reals-comparison):
    Benchmark(s) comparing the performance of Haskell libraries for exact real computation (work in progress)
