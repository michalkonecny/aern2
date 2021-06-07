# AERN2 <!-- omit in toc -->

A set of Haskell packages for approximating exact real numbers and other validated numerical computation.

- [A quick start](#a-quick-start)
- [Libraries](#libraries)
- [Other components](#other-components)
- [Significant dependencies](#significant-dependencies)
- [Other references](#other-references)

## A quick start

- [Installation guide](docs/install.md)
- Examples in [aern2-real README](aern2-real/README.md)

## Libraries

- [aern2-mp](aern2-mp):
  Multiple-precision interval arithmetic (fairly complete)

- [aern2-real](aern2-real):
  Cauchy real numbers (fairly complete)
- [aern2-fun](aern2-fun):
  Generic operations for real functions
- aern2-fun-univariate:
  Continuous real functions of one variable and their polynomial enclosures (currently broken, working version on branch [fnreps2020](https://github.com/michalkonecny/aern2/tree/fnreps2020/aern2-fun))

## Other components

- [aern2-erc](aern2-erc):
  A prototype implementation of the language ERC (Exact Real Computation) as defined in [Park, Sewon, et al. "Foundation of Computer (Algebra) ANALYSIS Systems: Semantics, Logic, Programming, Verification." arXiv e-prints (2016): arXiv-1608](https://arxiv.org/abs/1608.05787).
  
- aern2-fnreps:
  Benchmarks evaluating various representations of univariate continuous real functions (currently broken, working version on branch [fnreps2020](https://github.com/michalkonecny/aern2/tree/fnreps2020/aern2-fnreps))

## Significant dependencies

- [collect-errors](https://hackage.haskell.org/package/collect-errors):
  Type wrapper `CN` for gracefully propagating numerical errors through expressions

- [mixed-types-num](https://hackage.haskell.org/package/mixed-types-num):
  Bottom-up-typed numerical expressions

- [cdar-mBound](https://hackage.haskell.org/package/cdar-mBound):
  Exact real arithmetic built on floating-point-like intervals based on Haskell `Integer`s.  AERN2 currently uses the types `Dyadic` and `Approx`.

## Other references

- [haskell-reals-comparison](https://github.com/michalkonecny/haskell-reals-comparison):
    Benchmark(s) comparing the performance of Haskell libraries for exact real computation (work in progress)
