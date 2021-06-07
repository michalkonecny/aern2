# AERN2 ERC

A prototype implementation of the language ERC (Exact Real Computation) as defined in [Park, Sewon, et al. "Foundation of Computer (Algebra) ANALYSIS Systems: Semantics, Logic, Programming, Verification." arXiv e-prints (2016): arXiv-1608](https://arxiv.org/abs/1608.05787).

The examples from the paper are implemented in module [Examples](src/ERC/Examples.hs).  To run the examples, please first [install AERN2](../docs/install.md) and then follow the comments in the module.

This implementation of ERC is a shallow embedding into Haskell.  ERC syntax has been slightly modified to avoid clashes with Haskell syntax.  The differences are cosmetic, the abstract syntax is unchanged.
