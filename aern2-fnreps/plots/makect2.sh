#! /bin/bash

function makefn
{
    ct2file=$1.ct2
    sed "s!FUNCTION!$2!" function.ct2.template > $ct2file
    ctioga2 -f $ct2file
}

makefn "sine+cos" "'sin(10*x)+cos(20*x)'"
makefn "sine+cospi" "'sin(10*x)+cos(7*4*atan(1)*x)'"
makefn "sinesine" "'sin((10*x)+sin(7*4*atan(1)*x**2))'"
makefn "sinesine+cos" "'sin((10*x)+sin(7*4*atan(1)*x**2))+cos(10*x)'"
makefn "runge" "'1/(100*x**2+1)'"
makefn "rungeSC" "'(sin(10*x)+cos(7*4*atan(1)*x))/(100*x**2+1)'"
makefn "fracSin" "'1/(10*(sin(7*x))**2+1)'"
makefn "fracSinSC" "'(sin(10*x)+cos(7*4*atan(1)*x))/(10*(sin(7*x))**2+1)'"
# makefn "bumpy" "'((x**2/2) > sin(10*x) + cos(7*4*atan(1)*x) ? (x**2/2) : sin(10*x) + cos(7*4*atan(1)*x))'"
makefn "bumpy" "'(sin(10*x) > cos(11*x) ? sin(10*x) : cos(11*x))'"
makefn "bumpy2" "'((x**2/2) > (sin(10*x)+cos(7*4*atan(1)*x))/(10*(sin(7*x))**2+1) ? (x**2/2) : 1/(10*(sin(7*x))**2+1))'"
