# Comparing representations of exact univariate continuous real functions

_This is work in progress._

_An older version of this page corresponding to the [CCA 2016](http://cca-net.de/cca2016/) talk
"Representations for feasibly approximable functions"
is [here](https://github.com/michalkonecny/aern2/tree/preJuly2016/aern2-fnreps)_

We benchmark the performance of several data types of (exact) univariate continuous real functions
on a set of tasks.

Each task has 2 phases:

  1. Constructing a function on the domain
  ![](http://latex.codecogs.com/gif.latex?[-1,1])
  from a variable
  ![](http://latex.codecogs.com/gif.latex?x)
  and common operations such as sine and multiplication.

  2. Computing a real number from the function either by
  integrating or maximising the function over its domain.

Thus the tasks are formulated without reference to the implementation of the data type
of continuous functions, facilitating a reasonably fair comparison.

<!-- TODO: add TOC, eg using https://github.com/thlorenz/doctoc/ -->

## Representations

### Fun

A function
![](http://latex.codecogs.com/gif.latex?f)
is given by an evaluation procedure and a modulus of continuity.
For any dyadic
![](http://latex.codecogs.com/gif.latex?x)
in the domain of
![](http://latex.codecogs.com/gif.latex?f)
the Fun evaluation procedure returns the real number
![](http://latex.codecogs.com/gif.latex?f(x)).
A modulus of continuity has 2 parameters:
a ball ![](http://latex.codecogs.com/gif.latex?B)
and an integer ![](http://latex.codecogs.com/gif.latex?i).
A modulus of continuity returns an integer
![](http://latex.codecogs.com/gif.latex?j)
such that for any
![](http://latex.codecogs.com/gif.latex?x,y\\in [-1,1])
with
![](http://latex.codecogs.com/gif.latex?|x-y|\\leq 2^{-j})
it holds
![](http://latex.codecogs.com/gif.latex?|f(x)-f(y)|\\leq 2^{-i}).


### Ball

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


### DBall

<!-- line breaks are forced by 2 trailing spaces -->

A function
![](http://latex.codecogs.com/gif.latex?f)
is given by a pair of Fun representations:
one for
![](http://latex.codecogs.com/gif.latex?f)
and one for its weak (interval) derivative
![](http://latex.codecogs.com/gif.latex?f').
This representation facilitates a trapezoidal
interval quadrature.


### Poly


<!-- line breaks are forced by 2 trailing spaces -->

A function
![](http://latex.codecogs.com/gif.latex?f)
is given by a collection of polynomial approximations to the function,
each with an explicit error bound.  
The error bounds are valid over a given interval domain, the same domain for all approximations.  
The collection is parametrised by polynomial degree and coefficient precision.
When these parameters are increased arbitrarily, the error bound converges to zero.

The polynomials used in this representation are unary polynomials in the Chebyshev basis with MPFR dyadic coefficients.
The Chebyshev basis facilitates efficient Chebyshev truncation of terms whose degree is too high
or whose coefficient is of a negligible magnitude.

### PPoly

A piece-wise version of Poly.  The domain is partitioned into rational interval segments.
There is one Poly enclosure on each segment.  

An initial partition is usually determined when performing a pointwise division of one function by another.
When combining several functions, the partitions of the functions are unified.

<!-- line breaks are forced by 2 trailing spaces -->


## Benchmark setup

Source code of the benchmark tasks: [fnreps-ops.hs](https://github.com/michalkonecny/aern2/blob/master/aern2-fnreps/main/fnreps-ops.hs)

The benchmark timings are obtained on a Dell Inspiron 15R with 16GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.10.3 with -O2.

<!--
Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%.
-->

## Benchmark results

### A well-behaved analytic function with a number of local extrema

![sine+cos](http://latex.codecogs.com/gif.latex?\\sin(10x)+\\cos(20x))
<img src="plots/sine+cos.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/sine+cos-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/sine+cos-integrate-time.png?raw=true" width="300"> |

### A nested sine function

![sinesine](http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2)))
<img src="plots/sinesine.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/sinesine-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/sinesine-integrate-time.png?raw=true" width="300"> |  

![sinesine+sin](http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2))+\\sin(10x))
<img src="plots/sinesine+sin.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/sinesine+cos-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/sinesine+cos-integrate-time.png?raw=true" width="300"> |  


### An analytic function with singularities near the origin

![runge](http://latex.codecogs.com/gif.latex?{\\frac{1}{100x^2+1}})
<img src="plots/runge.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/runge-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/runge-integrate-time.png?raw=true" width="300"> |

![rungeX](http://latex.codecogs.com/gif.latex?{\\frac{x}{100x^2+1}})
<img src="plots/rungeX.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/rungeX-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/rungeX-integrate-time.png?raw=true" width="300"> |

### An analytic function with singularities near the origin and with multiple maxima

![fracSin](http://latex.codecogs.com/gif.latex?{\\frac{1}{10(\\sin(7x))^2+1}})
<img src="plots/fracSin.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/fracSin-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/fracSin-integrate-time.png?raw=true" width="300"> |


<!--
### A very simple non-smooth function

![hat](http://latex.codecogs.com/gif.latex?1-|x+1/3|)
<img src="plots/hat.png?raw=true" width="150">

| Maximum  over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) | Integral over ![unit-interval](http://latex.codecogs.com/gif.latex?[-1,1]) |
| :---: | :---: |
| <img src="benchresults/charts/hat-max-time.png?raw=true" width="300"> | <img src="benchresults/charts/hat-integrate-time.png?raw=true" width="300"> |


### A non-smooth function with multiple maxima

![bumpy](http://latex.codecogs.com/gif.latex?\\max(\\sin(10x),\\cos(11x)))
<img src="plots/bumpy.png?raw=true" width="150">

_(coming soon)_
-->
