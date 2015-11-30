# aern2
A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation. 

*This is a work in progress.*

The following is an example repl session:

```
> cd aern2-real
> cabal repl
...
*AERN2.Real.Examples> :t 1
1 :: Integer
*AERN2.Real.Examples> :t 1+1/3
1+1/3 :: Rational
*AERN2.Real.Examples> [1..100] !! (int 3)
4
...
*AERN2.Real.Examples> :set +s
*AERN2.Real.Examples> cauchyReal2ball pi 10
[3.1416015625 ± 4.8828125e-4]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi 100
[3.141592653589793 ± 1.793662034335766e-43]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi 1000
[3.141592653589793 ± 7.197089707547862e-481]
(0.01 secs, 14994488 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi 10000
[3.141592653589793 ± 3.370757064448901e-3295]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi 100000
[3.141592653589793 ± 4.653678253156862e-36543]
(0.04 secs, 6274272 bytes)
```

You can also start by cloning the module [AERN2.Real.Examples](https://github.com/michalkonecny/aern2/blob/master/aern2-real/src/AERN2/Real/Examples.hs).

