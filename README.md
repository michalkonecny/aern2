# aern2
A Haskell library for Approximating Exact Real Numbers (AERN) based on interval computation.

*This is a work in progress.*

Links: [installation instructions](docs/INSTALL.md)

The following is an example interactive session:

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
*AERN2.Real.Examples> cauchyReal2ball (rational2CauchyReal (1/3)) (bits 1000)
[3.333333333333333e-1 ± 8.996362134434827e-482]
*AERN2.Real.Examples> cauchyReal2ball (rational2CauchyReal (1/3)) Exact
*** Exception: convergent2Cauchy: the sequence either converges too slowly or it does not converge
*AERN2.Real.Examples> cauchyReal2ball (rational2CauchyReal 0.125) Exact
[1.25e-1 ± 0]
...
*AERN2.Real.Examples> :t pi
pi :: CauchyReal
*AERN2.Real.Examples> :set +s
*AERN2.Real.Examples> cauchyReal2ball pi (bits 10)
[3.1416015625 ± 4.8828125e-4]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi (bits 100)
[3.141592653589793 ± 1.793662034335766e-43]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi (bits 1000)
[3.141592653589793 ± 7.197089707547862e-481]
(0.01 secs, 14994488 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi (bits 10000)
[3.141592653589793 ± 3.370757064448901e-3295]
(0.01 secs, 0 bytes)
*AERN2.Real.Examples> cauchyReal2ball pi (bits 100000)
[3.141592653589793 ± 4.653678253156862e-36543]
(0.04 secs, 6274272 bytes)
...
*AERN2.Real.Examples> cauchyReal2ball (cos (pi/3) + sin(pi/3)) (bits 100000)
[1.366025403784439 ± 7.042664940643378e-36543]
(0.67 secs, 59789960 bytes)

```

You can also start by cloning the module [AERN2.Real.Examples](https://github.com/michalkonecny/aern2/blob/master/aern2-real/src/AERN2/Real/Examples.hs).
