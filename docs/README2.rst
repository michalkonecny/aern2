aern2
=====

**A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation**

*This code is experimental and often fast evolving.*

* [Installation instructions](docs/INSTALL.md)
* Main ideas
  * [Exact real number computation](docs/ERA.md)
    ie computing numbers with a target accuracy,
    the accuracy can be arbitrarily high
  * [Exact real function computation](docs/EFA.md)
    ie arbitrary continuous real functions as first-class objects,
    which can be evaluated, integrated, combined etc;
    can use polynomial approximations
  * Alternative approach to Haskell numerical types
    * [Mixed-type operators](docs/mixedtypeops.md)
       eg `let n = 1 :: Integer in 1/n :: Rational`
    * [Arrow-generic expressions](docs/arrowgeneric.md)
       ie, different evaluation strategies 
       (eg normal Haskell, logged, parallel, distributed)
       for explicit DAG expressions

