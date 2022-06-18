# Change log for aern2-mp

* current
  * more instances for CN Kleenean
  * Num MPFloat (unspecified rounding mode)
* v 0.2.8 2021-08-04
  * compatibility with ghc 9.0.1
  * add limits for CN (WithCurrentPrec p (CN MPBall))
* v 0.2.7 2021-06-02
  * make (WithCurrentPrec p (CN MPBall)) a Field
  * simplify WithCurrentPrec, swap its parameters
* v 0.2.6 2021-05-29
  * adapt to new ppow operations
* v 0.2.5 2021-05-27
  * add generic multivalued select + Kleenean instances
  * WithCurrentPrec: add MixedTypesNumPrelude class instances
* v 0.2.4 2021-05-26
  * use endpoint multiplication in integer power to avoid crossing 0
* v 0.2.3 2021-05-22
  * make MPBall "Very inaccurate" a *potential* error (important for CReal accuracy queries)
* v 0.2.2 2021-05-21
  * move Kleenean from mixed-types-num to here
* v 0.2.1 2021-05-18
  * add WithAnyPrec existential type wrapper for WithCurrentPrec
* v 0.2.0 2021-05-17
  * switch to new simplified collect-errors, mixed-types-num 0.5.0
    * got rid of EnsureCE etc.
    * not introducing CN wrapper unless at least one parameter is already CN
  * using CDAR backend only, no MPFR for now
  * WithCurrentPrec for specifying default precision via types
* v 0.1.4 2019-03-19
  * CDAR-based Integer-only backend
    * needs the mBound branch of CDAR
  * adapts to mixed-types-num 0.3.2 (new divI, mod)
* v 0.1.3.1 2018-11-21
  * small fixes, mainly documentation
* v 0.1.3.0 2018-11-20
  * only one MPFR backend - rounded
  * reduce backend-specific code
* v 0.1.2.0 2017-11-14
  * fix compilation with haskell-mpfr
* v 0.1.1.0 2017-11-14
  * using Claude Heiland-Allen's Numeric.Rounded.Simple
* v 0.1.0.1 2017-09-12
  * first release on Hackage
  * backends: hmpfr and (tweaked) rounded
