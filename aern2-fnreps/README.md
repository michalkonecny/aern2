# Comparing representations of exact unary continuous real functions

_This is work in progress._

## Benchmark setup

Source code of the benchmark tasks: [Main.hs](https://github.com/michalkonecny/aern2/blob/master/aern2-fnreps/main/Main.hs)

The benchmark timings are obtained on a Dell Inspiron 15R with 8GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.8.4 with -O2.

Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%.

## Benchmark results

### A well-behaved analytic function with a number of local extrema

![nested-sine](http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2)))

#### Integration over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

![sinesine-integrate.png](plots/sinesine-integrate.png?raw=true)

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

### An analytic function with singularities near the origin

![steep-fraction](http://latex.codecogs.com/gif.latex?{\\frac{1}{100x^2+1}})

#### Integration over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

### A non-smooth function

![abs](http://latex.codecogs.com/gif.latex?|x|)

#### Integration over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])
