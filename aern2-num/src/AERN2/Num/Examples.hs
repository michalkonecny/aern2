module AERN2.Num.Examples 
    (module AERN2.Num.Examples,
     module AERN2.Num)
where

import AERN2.Num

import Control.Arrow

{---------- examples from MAP2016 talk -----------}

{--- ordinary function arrow versions ---}


twiddle :: (Integer, Integer) -> Complex CauchyReal
twiddle(k,n) =  exp(-2*k*complex_i*pi/n)

{--- arrow-generic versions ---}

twiddleA :: (RealPredA to r) => (Integer, Integer) -> () `to` (Complex r)
twiddleA(k,n) = $(exprA[| let [i]=vars in exp(-2*k*i*pi/n)|]) <<< complex_iA
