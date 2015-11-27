# aern2
A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation. 

*This is a work in progress.*

The following is an example repl session:

```
> cd aern2-real
> cabal repl
...
*AERN2.Real.Examples> :t 1+1
1+1 :: Integer
*AERN2.Real.Examples> :t 1+1/3
1+1/3 :: Rational
*AERN2.Real.Examples> piBallUsingPrecision (prec 100)
[3.141592653589793 ± 3.155443620884047e-30]
*AERN2.Real.Examples> piBallUsingPrecision (prec 10)
[3.14453125 ± 3.90625e-3]
*AERN2.Real.Examples> piBallUsingPrecision (prec 1000000)
[3.141592653589793 ± 4.040136236792121e-301030]
*AERN2.Real.Examples> piBallUsingPrecision (prec 10000000)
[*** Exception: Precision must be between 2 and 1000000 (given: p=10000000).
```

You can also start by cloning the module [AERN2.Real.Examples](https://github.com/michalkonecny/aern2/blob/master/aern2-real/src/AERN2/Real/Examples.hs).

