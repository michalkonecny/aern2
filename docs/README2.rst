.. role:: haskell(code)
   :language: haskell


*****
aern2
*****

**A Haskell library for Approximating Exact Real Numbers (AERN) and Interval Methods**

*This code is experimental and often fast evolving.*

.. contents:: Table of Contents

Installation instructions
=========================

`are documented here <INSTALL.md>`_

Main ideas
==========

Exact real number computation
-----------------------------

Computing numbers with a target accuracy,
the accuracy can be arbitrarily high.  For example:

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
-------------------------------

Ie arbitrary continuous real functions as first-class objects,
which can be evaluated, integrated, combined etc.
Aern2 supports several representations, including polynomial approximations.

Alternative approach to Haskell numerical types
-----------------------------------------------

Bottom-up type derivation
^^^^^^^^^^^^^^^^^^^^^^^^^

Aern2 introduces and by default uses fixed-type numerical literals 
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

.. topic:: Prelude

    :haskell:`let n = 1 :: Integer in 1/(fromInteger n) :: Rational`.
    
.. topic:: Bottom-up type derivation:

    :haskell:`let n = 1 in 1/n`.
       
Arrow-generic expressions
^^^^^^^^^^^^^^^^^^^^^^^^^

Numerical expressions with explicit DAG structure, supporting
different evaluation strategies, such as:

* normal Haskell lazy evaluation
* lazy evaluation + logging of intermediate approximate values
* parallel lazy evaluation
* distributed lazy evaluation

