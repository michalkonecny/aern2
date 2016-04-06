# Comparing representations of exact univariate continuous real functions

_This is work in progress._

For several representations of univariate continuous real functions, we implement several function operators and benchmark their performance.

<!-- TODO: add TOC, eg using https://github.com/thlorenz/doctoc/ -->

## Representations

### Fun 

<!-- line breaks are forced by 2 trailing spaces -->

A function 
![](http://latex.codecogs.com/gif.latex?f)
is given by a procedure that for any dyadic interval/ball 
![](http://latex.codecogs.com/gif.latex?B)
in the domain of
![](http://latex.codecogs.com/gif.latex?f)
returns an interval/ball that contains the set
![](http://latex.codecogs.com/gif.latex?f(B)).
Moreover, as
![](http://latex.codecogs.com/gif.latex?B)
converges to a real number
![](http://latex.codecogs.com/gif.latex?x), 
the returned intervals converge to
![](http://latex.codecogs.com/gif.latex?f(x)).  
In this representation we use the type MPBall of balls with an arbitrary-precision dyadic center
and a double-precision radius.  
Equivalently, the function could be given by an evaluator over dyadics and a bound on its modulus of continuity.


### Poly

<!-- line breaks are forced by 2 trailing spaces -->

A function
![](http://latex.codecogs.com/gif.latex?f)
is given by a collection of polynomial approximations to the function, 
each with an explicit error bound.  
The error bounds are valid over a given interval domain, the same domain for all approximations.  
The collection is parametrised by polynomial degree and coefficient precision.
When these parameters are increased arbitrarily, the error bound converges to zero.

## Benchmark setup

Source code of the benchmark tasks: [Main.hs](https://github.com/michalkonecny/aern2/blob/master/aern2-fnreps/main/Main.hs)

The benchmark timings are obtained on a Dell Inspiron 15R with 8GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.8.4 with -O2.

Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%.

## Benchmark results

### A well-behaved analytic function with a number of local extrema

![sine+cos](http://latex.codecogs.com/gif.latex?\\sin(10x)+\\cos(20x))
<img src="plots/sine+cos-plot.png?raw=true" width="150">

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

<img src="plots/sine+cos-max.png?raw=true" width="400">
_([on Plotly](https://plot.ly/~mikkonecny/18/time-s-vs-accuracy-bits/))_

#### Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

<img src="plots/sine+cos-integrate.png?raw=true" width="400">
_([on Plotly](https://plot.ly/~mikkonecny/17/time-s-vs-accuracy-bits/))_

### A nested sine function

![sinesine](http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2)))
<img src="plots/sinesine.png?raw=true" width="150">

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

<img src="plots/sinesine-max.png?raw=true" width="400">
_([on Plotly](https://plot.ly/~mikkonecny/20/time-s-vs-accuracy-bits/))_

#### Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

<img src="plots/sinesine-integrate.png?raw=true" width="400">
_([on Plotly](https://plot.ly/~mikkonecny/11/time-s-vs-accuracy-bits/))_

<!--

### An analytic function with singularities near the origin

![steep-fraction](http://latex.codecogs.com/gif.latex?{\\frac{1}{100x^2+1}})

#### Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

### A non-smooth function

![abs](http://latex.codecogs.com/gif.latex?|x|)

#### Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])

#### Maximum over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1])
-->