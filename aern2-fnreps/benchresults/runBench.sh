#!/bin/bash

resultscsv=$1
if [ "$resultscsv" == "" ]; then echo "usage: $0 <results csv file name>"; exit 1; fi

fnrepsmain=aern2-fnreps-ops

reuselogs="true"

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
            getDataFromRunlog
        else
          if [ "$reuselogs" = "" ]
            then
              echo " (skipping due to existing log $runlog)"
            else
              echo " (reusing existing log $runlog)"
              getDataFromRunlog
          fi
    fi
}

function getDataFromRunlog
{
  utime=`grep "User time (seconds)" $runlog | sed 's/^.*: //'`
  stime=`grep "System time (seconds)" $runlog | sed 's/^.*: //'`
  mem=`grep "Maximum resident set size (kbytes)" $runlog | sed 's/^.*: //'`
  exact=`grep -i "accuracy: Exact" $runlog | sed 's/accuracy: Exact/exact/'`
  bits=`grep -i "accuracy: bits " $runlog | sed 's/accuracy: [bB]its //'`
  now=`date`
  echo "$now,$op,$fn,$repr,$params,$exact$bits,${utime/0.00/0.01},${stime/0.00/0.01},$mem" >> $resultscsv
}

#################
### sine+cos
#################

function sinecosBallFun
{
    repr=ball
    fn=sine+cos
    dir=$fn

    op=max
    for params in 10 15 20 25
    do
        runOne
    done

    op=integrate
    for params in 10 12 14
    do
        runOne
    done
}

function sinecosDBallFun
{
    repr=dball
    fn=sine+cos
    dir=$fn

    op=max
    for params in 10 15 20 25 30
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
    for params in 10 15 20 25 30
    do
        runOne
    done

    op=integrate
    for params in 10 12 14 16 18 20
    do
        runOne
    done
}

#################
### sinesine
#################

function sinesineBallFun
{
    repr=ball
    fn=sinesine
    dir=$fn

    op=max
    for params in 10 15 20 25 30 35
    do
        runOne
    done

    op=integrate
    for params in 5 10 15
    do
        runOne
    done
}

function sinesineDBallFun
{
    repr=dball
    fn=sinesine
    dir=$fn

    op=max
    for params in 10 15 20 25 30 35
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
    for params in 10 15 20 25 30 35
    do
        runOne
    done

    op=integrate
    for params in 5 10 15 20 25 30
    do
        runOne
    done
}

#################
### sinesine+cos
#################

function sinesine+cosBallFun
{
    repr=ball
    fn=sinesine+cos
    dir=$fn

    op=max
    for params in 10 20 30
    do
        runOne
    done

    op=integrate
    for params in 5 10 15
    do
        runOne
    done
}

function sinesine+cosDBallFun
{
    repr=dball
    fn=sinesine+cos
    dir=$fn

    op=max
    for params in 10 15 20 25 30 35
    do
        runOne
    done

    op=integrate
    for params in 10 15 20 25 30
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
    for params in 10 15 20 25 30 35
    do
        runOne
    done

    op=integrate
    for params in 10 15 20 25 30
    do
        runOne
    done
}


#################
### runge
#################

function rungeBallFun
{
    repr=ball
    fn=runge
    dir=$fn

    op=max
    for params in 05 35 65 100 120
    do
        runOne
    done

    op=integrate
    for params in 05 10 15 20
    do
        runOne
    done
}

function rungeDBallFun
{
    repr=dball
    fn=runge
    dir=$fn

    op=max
    for params in 05 35 65 100
    do
        runOne
    done

    op=integrate
    for params in 05 10 15 20 25 30
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
    for params in 00 01 10
    do
        runOne
    done

    op=integrate
    for params in 00 01 10
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
    for params in 10 20 40 80
    do
        runOne
    done

    op=integrate
    for params in 10 20 40
    do
        runOne
    done
}

function rungeFrac
{
    repr=frac
    fn=runge
    dir=$fn

    op=max
    for params in 10 20 40 80 120
    do
        runOne
    done
}

#################
### rungeX
#################

function rungeXBallFun
{
    repr=ball
    fn=rungeX
    dir=$fn

    op=max
    for params in 05 10 15 20 25 30 35
    do
        runOne
    done

    op=integrate
    for params in 05 10 15 20
    do
        runOne
    done
}

function rungeXDBallFun
{
    repr=dball
    fn=rungeX
    dir=$fn

    op=max
    for params in 05 10 20 30 40 50 60 80 100
    do
        runOne
    done

    op=integrate
    for params in 05 10 15 20 25 30 35
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
    for params in -01 01 04
    do
        runOne
    done

    op=integrate
    for params in -01 01 04
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
    for params in 10 20 40 80
    do
        runOne
    done

    op=integrate
    for params in 10 20 40 80
    do
        runOne
    done
}

function rungeXFrac
{
    repr=frac
    fn=rungeX
    dir=$fn

    op=max
    for params in 10 20 40 80 120
    do
        runOne
    done
}


#################
### fracSin
#################

function fracSinBallFun
{
    repr=ball
    fn=fracSin
    dir=$fn

    op=max
    for params in 05 15 25 35 45 55
    do
        runOne
    done

    op=integrate
    for params in 05 10 15
    do
        runOne
    done
}

function fracSinDBallFun
{
    repr=dball
    fn=fracSin
    dir=$fn

    op=max
    for params in 05 15 25 35 45 50
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
    for params in 01 03
    do
        runOne
    done

    op=integrate
    for params in 01 03
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
    for params in 10 20 40
    do
        runOne
    done

    op=integrate
    for params in 10 20 40
    do
        runOne
    done
}

function fracSinFrac
{
    repr=frac
    fn=fracSin
    dir=$fn

    op=max
    for params in 10 20 40 60
    do
        runOne
    done
}

#################
### hat
#################

# function hatBallFun
# {
#     repr=ball
#     fn=hat
#     dir=$fn
#
#     op=max
#     for params in 4 10 20 30
#     do
#         runOne
#     done
#
#     op=integrate
#     for params in 4 6 8 10 12 14 16 18
#     do
#         runOne
#     done
# }
#
# function hatDBallFun
# {
#     repr=dball
#     fn=hat
#     dir=$fn
#
#     op=max
#     for params in 4 10 20 30
#     do
#         runOne
#     done
#
#     op=integrate
#     for params in 4 10 16
#     do
#         runOne
#     done
# }
#
# function hatPoly
# {
#     repr=poly
#     fn=hat
#     dir=$fn
#
#     op=max
#     for params in "100 32 0 100" "100 64 0 100" "150 128 0 100"
#     do
#         runOne
#     done
#
#     op=integrate
#     for params in "100 32 0 100" "100 64 0 100" "150 128 0 100"
#     do
#         runOne
#     done
# }
#
# function hatPPoly
# {
#     repr=ppoly
#     fn=hat
#     dir=$fn
#
#     op=max
#     for params in "5 0 0 0 10" "35 0 0 0 35"
#     do
#         runOne
#     done
#
#     op=integrate
#     for params in "5 0 0 0 0" "35 0 0 0 0"
#     do
#         runOne
#     done
# }


sinecosBallFun
sinecosDBallFun
sinecosPoly

sinesineBallFun
sinesineDBallFun
sinesinePoly

sinesine+cosBallFun
sinesine+cosDBallFun
sinesine+cosPoly

rungeBallFun
rungeDBallFun
rungePoly
rungePPoly
rungeFrac

rungeXBallFun
rungeXDBallFun
rungeXPoly
rungeXPPoly
rungeXFrac

fracSinBallFun
fracSinDBallFun
fracSinPoly
fracSinPPoly
fracSinFrac

# hatBallFun
# hatDBallFun
# hatPoly
# hatPPoly
