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

A function :math:`f` is given by a procedure that for any dyadic interval/ball
:math:`B` in the domain of :math:`f` returns an interval/ball that contains the set
:math:`f(B)`.
Moreover, as :math:`B` converges to a real number :math:`x`,
the returned intervals converge to :math:`f(x)`.
In this representation we use the type MPBall, ie a ball with an
arbitrary-precision dyadic center and a double-precision radius.


DFun
~~~~

A function :math:`f` is given by a pair f Fun representations:
one for :math:`f` and one for its derivative :math:`f'`
This representation is currently used for integration
with an adaptive interval trapezoidal quadrature.

Poly
~~~~

A function :math:`f` is given by a collection of polynomial approximations 
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
  
  :math:`\sin(10x)+\cos(20x)`


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
  :hieght: 100px
  :alt: sinesine
  :align: center
  :figclass: align-center
  
  |sinesine|


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


.. |unit-interval| replace:: :math:`[-1,1]` 
.. |sinesine| replace:: :math:`\sin(10x+\sin(20x^2))`
