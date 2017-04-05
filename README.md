# AERN2

A set of Haskell packages for exact real number computation.

Provided library packages:

  * [aern2-mp](http://michalkonecny.github.io/aern2/aern2-mp/): Multiple-precision ball arithmetic based on MPFR

  * [aern2-real](http://michalkonecny.github.io/aern2/aern2-real/): Cauchy real numbers

  * [aern2-fun](http://michalkonecny.github.io/aern2/aern2-fun/): Real functions and their polynomial enclosures (work in progress)

  * [aern2-fun-plot](http://michalkonecny.github.io/aern2/aern2-fun-plot/): Plotting of real functions in browser (work in progress)

Provided executables:

  * [aern2-fnreps](https://github.com/michalkonecny/aern2/tree/master/aern2-fnreps): Benchmark comparison of representations of continuous functions

  * aern2-bench-chart: Produce performance charts using data from a csv file.  Used within aern2-fnreps and aern2-fun.

Significant dependency:

  * [mixed-types-num](https://michalkonecny.github.io/mixed-types-num/Numeric-MixedTypes.html):
    Bottom-up-typed numerical expressions
