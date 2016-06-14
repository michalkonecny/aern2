***********************************************************************
Comparing representations of exact univariate continuous real functions
***********************************************************************

For several representations of univariate continuous real functions, we
implement several function operators and benchmark their performance.

.. contents:: Table of Contents

Representations
---------------

Fun
~~~

A function |L_f| is given by a procedure that for any dyadic interval/ball
|L_B| in the domain of |L_f| returns an interval/ball that contains the set
|L_B|.
Moreover, as |L_B| converges to a real number |L_x|,
the returned intervals converge to |L_fx|.
In this representation we use the type MPBall, ie a ball with an
arbitrary-precision dyadic center and a double-precision radius.


DFun
~~~~

A function |L_f| is given by a pair of Fun representations:
one for |L_f| and one for its derivative |L_fD|
This representation is currently used for integration
with an adaptive interval trapezoidal quadrature.

Poly
~~~~

A function |L_f| is given by a collection of polynomial approximations 
to the function, each with an explicit error bound.
The error bounds are valid over a given interval domain, the same
domain for all approximations.
The collection is parametrised by polynomial degree and coefficient
precision.
When these parameters are increased arbitrarily, the error bound
converges to zero.

The polynomials used in this representation are unary polynomials in
the Chebyshev basis with MPFR dyadic coefficients.
The Chebyshev basis facilitates efficient Chebyshev truncation of
terms whose degree is too high
or whose coefficient is of a negligible magnitude.

PPoly
~~~~~

A piece-wise version of Poly. The domain is partitioned into rational
interval segments.
There is one Poly enclosure on each segment.

An initial partition is usually determined when performing a pointwise
division of one function by another.
When combining several functions, the partitions of the functions are
unified.


Benchmark setup
---------------

Source code of the benchmark tasks:
`fnreps-ops.hs <aern2-fnreps/main/fnreps-ops.hs>`_

The benchmark timings are obtained on a Dell Inspiron 15R with 8GB
RAM, Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.8.4 with -O2.

..  Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%.

Benchmark results
-----------------

A well-behaved analytic function with a number of local extrema
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. figure:: plots/sine+cos.png?raw=true
  :width: 200px
  :alt: sin+cos
  :align: center
  :figclass: align-center
  
  |L_sine+cos|


+--------------------------------+---------------------------------+
| Maximum over |unit-interval|   | Integral over |unit-interval|   |
+================================+=================================+
| |chart-sin+cos-max|            | |chart-sin+cos-integral|        |
+--------------------------------+---------------------------------+

.. |chart-sin+cos-max| image:: benchresults/charts/sine+cos-max.png?raw=true
  :width: 300px

.. |chart-sin+cos-integral| image:: benchresults/charts/sine+cos-integrate.png?raw=true
  :width: 300px


A nested sine function
~~~~~~~~~~~~~~~~~~~~~~


.. figure:: plots/sinesine.png?raw=true
  :width: 200px
  :height: 100px
  :alt: sinesine
  :align: center
  :figclass: align-center
  
  |L_sinesine|


+--------------------------------+---------------------------------+
| Maximum over |unit-interval|   | Integral over |unit-interval|   |
+================================+=================================+
+--------------------------------+---------------------------------+


.. raw:: html

   <!--
   ### A non-smooth function with multiple maxima

   ![bumpy](http://latex.codecogs.com/gif.latex?\\max(\\sin(10x),\\cos(11x)))
   <img src="plots/bumpy.png?raw=true" width="150">

   _(coming soon)_
   -->


.. |L_f| image:: http://latex.codecogs.com/gif.latex?f
.. |L_fD| image:: http://latex.codecogs.com/gif.latex?f'
.. |L_B| image:: http://latex.codecogs.com/gif.latex?B
.. |L_f(B)| image:: http://latex.codecogs.com/gif.latex?f(B)
.. |L_x| image:: http://latex.codecogs.com/gif.latex?x
.. |L_fx| image:: http://latex.codecogs.com/gif.latex?f(x)
.. |L_sine+cos| image:: http://latex.codecogs.com/gif.latex?\\sin(10x)+\\cos(20x)
.. |L_unit-interval| image:: http://latex.codecogs.com/gif.latex?[-1,1]
.. |L_sinesine| image:: http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2))
.. |L_sinesine+sin| image:: http://latex.codecogs.com/gif.latex?\\sin(10x+\\sin(20x^2))+\\sin(10x)
.. |L_runge| image:: http://latex.codecogs.com/gif.latex?{\\frac{1}{100x^2+1}}
.. |L_rungeX| image:: http://latex.codecogs.com/gif.latex?{\\frac{x}{100x^2+1}}
.. |L_fracSin| image:: http://latex.codecogs.com/gif.latex?{\\frac{1}{10(\\sin(7x))^2+1}}
.. |L_hat| image:: http://latex.codecogs.com/gif.latex?1-|x+1/3|
