#!/bin/bash

resultscsv=$1
if [ "$resultscsv" == "" ]; then echo "usage: $0 <results csv file name>"; exit 1; fi

fnrepsmain=./aern2-fnreps-ops

# put headers in the results csv file if it is new:
if [ ! -f $resultscsv ]
    then echo "Time,Op,Fn,FnRepr,Parameters,Accuracy(bits),UTime(s),STime(s),Mem(kB)" > $resultscsv
    if [ $? != 0 ]; then exit 1; fi
fi 

function runOne
# parameters: 
#  $dir where to put individual logs
#  $op
#  $fn
#  $repr
#  $params 
{
    runlog="$dir/run-$op-$fn-$repr-${params// /_}.log"
    echo -n /usr/bin/time -v $fnrepsmain $op $fn $repr $params
    if [ ! -f $runlog ] || grep -q "terminated" $runlog
        then
            echo " (running and logging in $runlog)"
            /usr/bin/time -v $fnrepsmain $op $fn $repr $params >& $runlog
            if [ $? != 0 ]; then rm $runlog; exit 1; fi
            utime=`grep "User time (seconds)" $runlog | sed 's/^.*: //'`
            stime=`grep "System time (seconds)" $runlog | sed 's/^.*: //'`
            mem=`grep "Maximum resident set size (kbytes)" $runlog | sed 's/^.*: //'`
            bits=`grep "accuracy: Bits " $runlog | sed 's/accuracy: Bits //'`
            now=`date`
            echo "$now,$op,$fn,$repr,$params,$bits,${utime/0.00/0.01},${stime/0.00/0.01},$mem" >> $resultscsv
                                                           # avoid 0 in log-scale charts
        else
            echo " (skipping due to existing log $runlog)"
    fi
}

#################
### sine+cos
#################

function sinecosFun
{
    repr=fun
    fn=sine+cos
    dir=$fn
    
    op=max
    for params in 10 15 20 25 30
    do
        runOne
    done
    
    op=integrate
    for params in 10 12 14
    do
        runOne
    done
}

function sinecosDFun
{
    repr=dfun
    fn=sine+cos
    dir=$fn
    
    op=max
    for params in 10 15 20 25 30 35
    do
        runOne
    done
    
    op=integrate
    for params in 10 12 14 16 18 20
    do
        runOne
    done
}

function sinecosPoly
{
    repr=poly
    fn=sine+cos
    dir=$fn
    
    op=max
    for deg in 34 36 38 40 42 44 
    do
        params="100 $deg 0 100"
        runOne
    done
    
    op=integrate
    for deg in 32 34 36 38
    do
        params="100 $deg 0 100"
        runOne
    done
}

#################
### sinesine
#################

function sinesineFun
{
    repr=fun
    fn=sinesine
    dir=$fn
    
    op=max
    for params in 10 30 50 70 90 95
    do
        runOne
    done
    
    op=integrate
    for params in 5 10 15
    do
        runOne
    done
}

function sinesineDFun
{
    repr=dfun
    fn=sinesine
    dir=$fn
    
    op=max
    for params in 10 30 50 70 90 95
    do
        runOne
    done
    
    op=integrate
    for params in 5 10 15 20 25 30
    do
        runOne
    done
}

function sinesinePoly
{
    repr=poly
    fn=sinesine
    dir=$fn
    
    op=max
    for params in "100 100 150 100" "100 100 200 100" "100 100 300 100" "100 100 400 100" "200 100 400 200" "200 100 500 200" "400 150 600 400"
    do
        runOne
    done
    
    op=integrate
    for params in "100 100 150 100" "100 100 200 100" "100 100 300 100" "100 100 400 100" "200 100 400 200" "200 100 500 200" "400 150 600 400"
    do
        runOne
    done
}

#################
### sinesine+cos
#################

function sinesine+cosFun
{
    repr=fun
    fn=sinesine+cos
    dir=$fn
    
    op=max
    for params in 10 20 30 40 45
    do
        runOne
    done
    
    op=integrate
    for params in 5 10 15
    do
        runOne
    done
}

function sinesine+cosDFun
{
    repr=dfun
    fn=sinesine+cos
    dir=$fn
    
    op=max
    for params in 10 20 30 40 45
    do
        runOne
    done
    
    op=integrate
    for params in 5 10 15 20 25 30
    do
        runOne
    done
}

function sinesine+cosPoly
{
    repr=poly
    fn=sinesine+cos
    dir=$fn
    
    op=max
    for params in "100 100 150 100" "100 100 200 100" "100 100 300 100" "100 100 400 100" "200 100 400 200" "200 100 500 200" "400 150 600 400"
    do
        runOne
    done
    
    op=integrate
    for params in "100 100 150 100" "100 100 200 100" "100 100 300 100" "100 100 400 100" "200 100 400 200" "200 100 500 200" "400 150 600 400"
    do
        runOne
    done
}


#################
### runge
#################

function rungeFun
{
    repr=fun
    fn=runge
    dir=$fn
    
    op=max
    for params in 05 35 65 100
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15 20
    do
        runOne
    done
}

function rungeDFun
{
    repr=dfun
    fn=runge
    dir=$fn
    
    op=max
    for params in 05 35 65 100
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15 20 25 30 35
    do
        runOne
    done
}

function rungePoly
{
    repr=poly
    fn=runge
    dir=$fn
    
    op=max
    for params in "100 64 0 100" "120 128 0 100" "150 128 0 100" "200 256 0 100" "300 256 0 200"
    do
        runOne
    done
    
    op=integrate
    for params in "100 64 0 100" "120 128 0 100" "150 128 0 100" "200 256 0 100" "300 256 0 200"
    do
        runOne
    done

}


function rungePPoly
{

    repr=ppoly
    fn=runge
    dir=$fn
    
    op=max
    for params in "100 0 0.1 1 100" "100 0 0.1 2 100" "150 0 0.1 3 150" "150 0 0.01 3 150" "300 0 0.05 4 300" "200 0 0.001 3 200" "300 0 0.01 4 300"
    do
        runOne
    done
    
    op=integrate
    for params in "100 0 0.1 1 100" "100 0 0.1 2 100" "150 0 0.1 3 150" "150 0 0.01 3 150" "300 0 0.05 4 300" "200 0 0.001 3 200" "300 0 0.01 4 300"
    do
        runOne
    done

}

#################
### rungeX
#################

function rungeXFun
{
    repr=fun
    fn=rungeX
    dir=$fn
    
    op=max
    for params in 05 10 15 20 25 30 35 40
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15 20
    do
        runOne
    done
}

function rungeXDFun
{
    repr=dfun
    fn=rungeX
    dir=$fn
    
    op=max
    for params in 05 10 20 30 40 50 60 80
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15 20 25 30 35 40
    do
        runOne
    done
}

function rungeXPoly
{
    repr=poly
    fn=rungeX
    dir=$fn
    
    op=max
    for params in "100 64 0 100" "120 128 0 100" "150 128 0 100" "200 256 0 100"
    do
        runOne
    done
    
    op=integrate
    for params in "100 64 0 100" "120 128 0 100" "150 128 0 100" "200 256 0 100"
    do
        runOne
    done
}


function rungeXPPoly
{
    repr=ppoly
    fn=rungeX
    dir=$fn
    
    op=max
    for params in "100 0 0.1 1 100" "100 0 0.1 2 100" "150 0 0.1 3 150" "150 0 0.01 3 150" "300 0 0.05 4 300" "200 0 0.001 3 200" "300 0 0.01 4 300"
    do
        runOne
    done
    
    op=integrate
    for params in "100 0 0.1 1 100" "100 0 0.1 2 100" "150 0 0.1 3 150" "150 0 0.01 3 150" "300 0 0.05 4 300" "200 0 0.001 3 200" "300 0 0.01 4 300"
    do
        runOne
    done
}


#################
### fracSin
#################

function fracSinFun
{
    repr=fun
    fn=fracSin
    dir=$fn
    
    op=max
    for params in 05 10 15 20 25 30
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15
    do
        runOne
    done
}

function fracSinDFun
{
    repr=dfun
    fn=fracSin
    dir=$fn
    
    op=max
    for params in 05 10 15 20 25 30
    do
        runOne
    done
    
    op=integrate
    for params in 05 10 15 20 25 30
    do
        runOne
    done
}

function fracSinPoly
{
    repr=poly
    fn=fracSin
    dir=$fn
    
    op=max
    for params in "100 64 64 100" "120 128 128 100"
#     "500 256 256 100"
    do
        runOne
    done
    
    op=integrate
    for params in "100 64 64 100" "120 128 128 100"
#     "500 256 256 100"
    do
        runOne
    done
}

function fracSinPPoly
{
    repr=ppoly
    fn=fracSin
    dir=$fn
    
    op=max
    for params in "100 20 0.5 1 100" "100 30 0.1 1 100" "100 30 0.01 1 100" "100 30 0.1 2 100" "100 40 0.01 2 100" "100 40 0.001 2 100" "800 100 0.1 3 200"
    do
        runOne
    done
    
    op=integrate
    for params in "100 20 0.5 1 100" "100 30 0.1 1 100" "100 30 0.01 1 100" "100 30 0.1 2 100" "100 40 0.01 2 100" "100 40 0.001 2 100" "800 100 0.1 3 200"
    do
        runOne
    done
}

#################
### hat
#################

function hatFun
{
    repr=fun
    fn=hat
    dir=$fn
    
    op=max
    for params in 4 10 20 30 
    do
        runOne
    done
    
    op=integrate
    for params in 4 6 8 10 12 14 16 18
    do
        runOne
    done
}

function hatDFun
{
    repr=dfun
    fn=hat
    dir=$fn
    
    op=max
    for params in 4 10 20 30 
    do
        runOne
    done
    
    op=integrate
    for params in 4 10 16
    do
        runOne
    done
}

function hatPoly
{
    repr=poly
    fn=hat
    dir=$fn
    
    op=max
    for params in "100 32 0 100" "100 64 0 100" "150 128 0 100"
    do
        runOne
    done
    
    op=integrate
    for params in "100 32 0 100" "100 64 0 100" "150 128 0 100"
    do
        runOne
    done
}

function hatPPoly
{
    repr=ppoly
    fn=hat
    dir=$fn
    
    op=max
    for params in "5 0 0 0 10" "35 0 0 0 35"
    do
        runOne
    done
    
    op=integrate
    for params in "5 0 0 0 0" "35 0 0 0 0"
    do
        runOne
    done
}


sinecosFun
sinecosDFun
sinecosPoly

sinesineFun
sinesineDFun
sinesinePoly

sinesine+cosFun
sinesine+cosDFun
sinesine+cosPoly

rungeFun
rungeDFun
rungePoly
rungePPoly

rungeXFun
rungeXDFun
rungeXPoly
rungeXPPoly

fracSinFun
fracSinDFun
fracSinPoly
fracSinPPoly

hatFun
hatDFun
hatPoly
hatPPoly
