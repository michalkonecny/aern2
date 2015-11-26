# aern2
A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation. 

*This is a work in progress.*

Currently, the best place to start is the module [AERN2.Real.Examples](https://github.com/michalkonecny/aern2/blob/master/aern2-real/src/AERN2/Real/Examples.hs).
The following is an example repl session using this module:

```
> cd aern2-real
> cabal repl
...
*AERN2.Real.Operations> :module +AERN2.Real.Examples
*AERN2.Real.Operations AERN2.Real.Examples> cauchyThirdWithAccuracy 100
[3.333333333333333e-1 ± 1.469367938527859e-39]
*AERN2.Real.Operations AERN2.Real.Examples> cauchyThirdWithAccuracy 10
[3.333358764648438e-1 ± 7.62939453125e-6]
*AERN2.Real.Operations AERN2.Real.Examples>
```

