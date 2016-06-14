.. role:: haskell(code)
   :language: haskell


*****
aern2
*****

**A Haskell library for Approximating Exact Real Numbers (AERN) and Interval Methods**

*This code is experimental and often fast evolving.*

`Installation instructions <docs/INSTALL.md>`_

.. contents:: Table of Contents

Exact real number computation
=============================

Computing numbers with a target accuracy, the accuracy can be arbitrarily high.  
For example:

.. code-block:: plain

    > cd aern2-num
    > cabal repl
    ...
    *AERN2.Num> :t pi
    pi :: CauchyReal
    *AERN2.Num> :set +s
    *AERN2.Num> cauchyReal2ball pi (bits 10)
    [3.1416015625 ± 4.8828125e-4]
    (0.01 secs, 0 bytes)
    *AERN2.Num> cauchyReal2ball pi (bits 100)
    [3.141592653589793 ± 1.793662034335766e-43]
    (0.01 secs, 0 bytes)
    *AERN2.Num> cauchyReal2ball pi (bits 1000)
    [3.141592653589793 ± 7.197089707547862e-481]
    (0.01 secs, 14994488 bytes)
    *AERN2.Num> cauchyReal2ball pi (bits 10000)
    [3.141592653589793 ± 3.370757064448901e-3295]
    (0.01 secs, 0 bytes)
    *AERN2.Num> cauchyReal2ball pi (bits 100000)
    [3.141592653589793 ± 4.653678253156862e-36543]
    (0.04 secs, 6274272 bytes)
    ...
    *AERN2.Num> cauchyReal2ball (cos (pi/3) + sin(pi/3)) (bits 100000)
    [1.366025403784439 ± 7.042664940643378e-36543]
    (0.67 secs, 59789960 bytes)


Exact real function computation
===============================

Arbitrary continuous real functions can be treated as first-class objects
that can be evaluated, integrated, combined etc.

aern2 supports a few representations for unary real functions
over the interval :math:`[-1,1]`.
These representations are in package aern2-fnreps,
whose purpose is to benchmark the representations against each other.
See `README in aern2-fnreps <aern2-fnreps>`_ for an overview of the representations
and the benchmarks.


Alternative approach to Haskell numerical types
===============================================

Bottom-up type derivation
-------------------------

aern2 introduces and by default uses fixed-type numerical literals 
and arithmetic expressions whose type is derived bottom-up:

.. code-block:: plain

    > cd aern2-num
    > cabal repl
    ...
    *AERN2.Num> :t 1
    1 :: Integer
    *AERN2.Num> :t 1+1/3
    1+1/3 :: Rational
    *AERN2.Num> [1..100] !! (int 3)
    4
    *AERN2.Num> integer (length [1,2]) + 1/3
    7 % 3


This is beneficial, for example, to avoid type signatures and conversions, such as: 

========================= ===============================================================
Prelude                   :haskell:`let n = 1 :: Integer in 1/(fromInteger n) :: Rational`.
Bottom-up type derivation :haskell:`let n = 1 in 1/n`
========================= ===============================================================
       
One may argue that this issue never arises if one consistently uses one
and the same numerical type, such as Double or CauchyReal.
Nevertheless, there are situations where this would be impractical, eg:

* When using matrix or limit sequence indices in numerical expressions

* When working with functions as first-class objects because
  it is inefficient to turn each scalar constant first into function object

* When programming at the level of dyadic intervals or balls of various precisions 
  because it is not clear what precision to use for integer and rational constants. 

       
Arrow-generic expressions
-------------------------

Arrow-generic expressions have an explicit DAG structure. 
Such an expression can be evaluated in several "real number computation" arrows.
Currently, the following arrows are supported (in various degrees of completeness):

* :haskell:`(->)`, ie normal Haskell lazy evaluation

* lazy evaluation + logging of intermediate approximate values

* parallel lazy evaluation

* distributed lazy evaluation

Comparison with symbolic expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A similar level of flexibility of evaluation methods can be achieved using symbolic expressions
and interpreting them using the above methods.
Nevertheless: 

* symbolic expressions are less flexible in the number of operations and operators supported

* one cannot easily indicate which common sub-expressions should be shared and which not
