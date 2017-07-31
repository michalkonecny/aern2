# haskell-reals-comparison

__A comparison of Haskell exact real number implementations__

_This is work in progress._

_Everyone is welcome to contribute, especially authors of Haskell exact real software._

### Benchmark setup

The source of the benchmark tasks:  
* [Tasks.PreludeOps](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/PreludeOps.hs) assuming a Prelude [Floating](https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Floating) instance
* [Tasks.AERN2Ops](https://github.com/michalkonecny/haskell-reals-comparison/blob/master/src/Tasks/AERN2Ops.hs) assuming an AERN2 [ArrowReal](https://github.com/michalkonecny/aern2/blob/master/aern2-num/src/AERN2/Num/Operations.hs) instance

The benchmark timings are obtained on a Dell Inspiron 15R with 8GB RAM,
Intel(R) Core(TM) i7-3632QM CPU @ 2.20GHz running Ubuntu 14.04.

The benchmarks have been compiled using ghc-7.8.4 with -O2.

Each benchmark has been executed repeatedly until 3 consecutive times the results have fluctuated for less than 5%.

### Implementations

| Implementation | Notable dependencies | Status | Reliability | Release date of the version used here |
| ----- | ----- | ----- | ----- | ----- |
| [ireal](https://hackage.haskell.org/package/ireal) | _(pure Haskell)_ | fairly complete, on Hackage | well tested | 2015-10-31 |
| [aern2](https://github.com/michalkonecny/aern2) | [haskell-mpfr](https://github.com/comius/haskell-mpfr) | experimental, on GitHub | not tested yet | 2016-03-07 |


### Benchmark results

| Implementation | real data type | logistic0 (n=100) | logistic1 (n=1000)  | logistic2 (n=10000)  | logistic3 (n=100000) |
| -------- | ------ | ---- | ---- | ---- | ---- |
| ireal | IReal, Prelude operations | 0.04 s | 19.8 s / 19 MB | > 2 hours, > 500MB |  |
| aern2 | CauchyReal, Prelude operations | 0.01 s | 0.08 s / 11 MB | 1.7 s / 155 MB | > 3.5GB |
| aern2 | CauchyReal, mixed-type operations | 0.01 s | 0.08 s / 12 MB | 2.6 s / 230 MB | > 3.5GB |
| aern2 | iRRAM-style MPBall, Prelude operations | 0.01 s | 0.05 s / 4 MB | 4.4 s / 11 MB | 521 s / 49 MB|
| aern2 | iRRAM-style MPBall, mixed-type operations | 0.02 s | 0.18 s / 4 MB | 6.2 s / 13 MB | 550 s / 106 MB |

### TODO
* include package [exact-real](https://hackage.haskell.org/package/exact-real)
* include package [haskell-fast-reals](https://github.com/comius/haskell-fast-reals)
* add more benchmarks
* (ongoing) regularly update for newer versions
