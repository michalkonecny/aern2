# AERN2

A set of Haskell packages for exact real number computation.

[Installation guide](docs/install.md)

For a crude mini-crash course, see [Demos.hs](aern2-demos/src/Demos.hs).

Library packages:

  * [aern2-mp](http://michalkonecny.github.io/aern2/aern2-mp-0.1.0.0/): Multiple-precision ball arithmetic based on MPFR (fairly complete)

  * [aern2-real](http://michalkonecny.github.io/aern2/aern2-real-0.1.0.0/): Cauchy real numbers (fairly complete)

  * [aern2-fun](http://michalkonecny.github.io/aern2/aern2-fun-0.1.0.0/): Real functions and their polynomial enclosures (work in progress)

  * [aern2-fun-plot](http://michalkonecny.github.io/aern2/aern2-fun-plot-0.1.0.0/): Plotting of real functions in browser (currently broken)

Executables:

  * [aern2-fnreps](https://github.com/michalkonecny/aern2/tree/master/aern2-fnreps): Benchmark comparison of representations of continuous functions

  * aern2-bench-chart: Produce performance charts using data from a csv file.  Used within aern2-fnreps and aern2-fun.

Significant dependency:

  * [mixed-types-num](https://michalkonecny.github.io/mixed-types-num/Numeric-MixedTypes.html):
    Bottom-up-typed numerical expressions
