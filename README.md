# aern2
A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation. 

*This is a work in progress.*

The following is an example repl session:

```
> cd aern2-real
> cabal repl
...
*AERN2.Real.Operations> :module +AERN2.Real.CauchyReal 
*AERN2.Real.Operations AERN2.Real.CauchyReal> 1 + 1
2
*AERN2.Real.Operations AERN2.Real.CauchyReal> :t 1 + 1
1 + 1 :: Integer
*AERN2.Real.Operations AERN2.Real.CauchyReal> :t 1 + 1/3
1 + 1/3 :: Rational
*AERN2.Real.Operations AERN2.Real.CauchyReal> cauchyReal2ball (rational2CauchyReal (1/3)) 100
[3.333333333333333e-1 ± 1.469367938527859e-39]
*AERN2.Real.Operations AERN2.Real.CauchyReal> cauchyReal2ball (rational2CauchyReal (1/3)) 10
[3.333358764648438e-1 ± 7.62939453125e-6]
```

You can also start by cloning the module [AERN2.Real.Examples](https://github.com/michalkonecny/aern2/blob/master/aern2-real/src/AERN2/Real/Examples.hs).

