# Introduction

*Exact real computation* are methods to execute numerical algorithms, such as 

~~~ Python
def exp(x) = series_sum(n, x^n/n!)
~~~

100% correctly, to an arbitrarily high accuracy.  The parameter `x` and the result of function `exp` are real numbers. 

There are different ways to realise exact real computation, corresponding to different encodings of the real numbers and different algorithms for its basic operations. The most common way to encode a real number `x` is as a function that for any (rational) tolerance `ε>0` returns an interval `[a,b]` that contains `x` and is at most `ε` wide.  The endpoints `a`,`b` are dyadic (*e.g.*, floating-point) numbers.

A similar approach can be used to encode other continuous objects, such as continuous real functions over a compact real interval or compact subsets of ℝ<sup>n</sup>.  Here, intervals are replaced by appropriate finite enclosures of these infinite objects.  The library [AERN2](https://github.com/michalkonecny/aern2) supports exact real numbers as well as exact continuous real functions as first-class objects, enclosed by arbitrarily small polynomial intervals, *a.k.a.* Taylor Models.
