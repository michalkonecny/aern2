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

`here <INSTALL.md>`_

Main ideas
==========

Exact real number computation
-----------------------------

Ie computing numbers with a target accuracy,
the accuracy can be arbitrarily high.

.. <docs/ERA.md>

Exact real function computation
-------------------------------

ie arbitrary continuous real functions as first-class objects,
which can be evaluated, integrated, combined etc;
can use polynomial approximations

.. <docs/EFA.md>

Alternative approach to Haskell numerical types
-----------------------------------------------

Mixed-type operators
^^^^^^^^^^^^^^^^^^^^

eg :haskell:`let n = 1 :: Integer in 1/n :: Rational`

.. <docs/mixedtypeops.md>

       
Arrow-generic expressions
^^^^^^^^^^^^^^^^^^^^^^^^^

Numerical expressions with explicit DAG structure, supporting
different evaluation strategies, such as:
* normal Haskell lazy evaluation
* lazy evaluation + logging of intermediate approximate values
* parallel lazy evaluation
* distributed lazy evaluation

.. <docs/arrowgeneric.md>

