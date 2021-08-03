# Change log for aern2-real

* v 0.2.8 2021-08-04
  * compatibility with ghc 9.0.1
* v 0.2.7 2021-06-02
  * add RealNumber type class
* v 0.2.6 2021-05-29
  * adapt to new ppow operations
* v 0.2.5 2021-05-27
  * CanSelect moved to aern2-mp
* v 0.2.4 2021-05-26
  * overhaul README and examples
  * stop "very inaccurate" errors breaking ? (bits n) queries
  * optimisation: ? (bits n) queries start from precision n
  * add tests for accuracy queries, limit and select
  * fix div by 0 during low-accuracy integer powers
* v 0.2.1 2021-05-18
  * add conversion from WithAnyPrec
* v 0.2.0 2021-05-17
  * moving Arrow-based functionality to package aern2-net
  * replacing Arrow-based sequences by list-based sequences
  * switch to new simplified collect-errors, mixed-types-num 0.5.0
    * got rid of EnsureCE etc.
    * not introducing CN wrapper unless at least one parameter is already CN
* v 0.1.2 2019-03-19
  * adapts to mixed-types-num 0.3.2 (new divI, mod)
* v 0.1.1.0 2017-12-06
  * disable aern2-generate-netlog-elm for now
* v 0.1.0.3 2017-12-06
  * remove further upper bounds
* v 0.1.0.2 2017-11-14
  * remove most upper bounds, building with ghc 8.2
* v 0.1.0.1 2017-09-12
  * first release on Hackage
  * fast convergent sequences indexed by AccuracySG
  * arrow-based networks of nodes communicating via query-answer protocols
  * networks executable with cached and parallel strategies
  * network execution can be visualised in browser using an Elm frontend
