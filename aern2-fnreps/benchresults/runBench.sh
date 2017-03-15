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

function sinecosModFun
{
    repr=fun
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

function sinesineModFun
{
    repr=fun
    fn=sinesine
    dir=$fn

    op=max
    for params in 10 15 20
    do
        runOne
    done

    op=integrate
    for params in 5 7
    do
        runOne
    done
}

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

function sinesine+cosModFun
{
    repr=fun
    fn=sinesine+cos
    dir=$fn

    op=max
    for params in 10 15 20
    do
        runOne
    done

    op=integrate
    for params in 5 7
    do
        runOne
    done
}

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

function rungeModFun
{
    repr=fun
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

    op=integrate
    for params in 8 16 32
    do
        runOne
    done
}

#################
### rungeX
#################

function rungeXModFun
{
    repr=fun
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

    op=integrate
    for params in 8 16 32 64
    do
        runOne
    done
}

#################
### rungeSC
#################

function rungeSCModFun
{
    repr=fun
    fn=rungeSC
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

function rungeSCBallFun
{
    repr=ball
    fn=rungeSC
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

function rungeSCDBallFun
{
    repr=dball
    fn=rungeSC
    dir=$fn

    op=max
    for params in 05 10 20 30 40 50 52
    do
        runOne
    done

    op=integrate
    for params in 05 10 15 20 25 30
    do
        runOne
    done
}

function rungeSCPoly
{
    repr=poly
    fn=rungeSC
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


function rungeSCPPoly
{
    repr=ppoly
    fn=rungeSC
    dir=$fn

    op=max
    for params in 8 16 32
    do
        runOne
    done

    op=integrate
    for params in 8 16 32
    do
        runOne
    done
}

function rungeSCFrac
{
    repr=frac
    fn=rungeSC
    dir=$fn

    op=max
    for params in 10 20 40 80 120
    do
        runOne
    done

    op=integrate
    for params in 10 20 40 80
    do
        runOne
    done
}


#################
### fracSin
#################

function fracSinModFun
{
    repr=fun
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

    op=integrate
    for params in 10 20 40
    do
        runOne
    done
}


#################
### fracSinSC
#################

function fracSinSCBallFun
{
    repr=ball
    fn=fracSinSC
    dir=$fn

    op=max
    for params in 05 10 15
    do
        runOne
    done

    op=integrate
    for params in 05 10 15
    do
        runOne
    done
}

function fracSinSCDBallFun
{
    repr=dball
    fn=fracSinSC
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

function fracSinSCPoly
{
    repr=poly
    fn=fracSinSC
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

function fracSinSCPPoly
{
    repr=ppoly
    fn=fracSinSC
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

function fracSinSCFrac
{
    repr=frac
    fn=fracSinSC
    dir=$fn

    op=max
    for params in 10 20 40 60
    do
        runOne
    done

    op=integrate
    for params in 10 20 40
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


sinecosModFun
sinecosBallFun
sinecosDBallFun
sinecosPoly

sinesineModFun
sinesineBallFun
sinesineDBallFun
sinesinePoly

sinesine+cosModFun
sinesine+cosBallFun
sinesine+cosDBallFun
sinesine+cosPoly

# rungeModFun
rungeBallFun
rungeDBallFun
rungePoly
rungePPoly
rungeFrac

# rungeXModFun
rungeXBallFun
rungeXDBallFun
rungeXPoly
rungeXPPoly
rungeXFrac

# rungeSCModFun
rungeSCBallFun
rungeSCDBallFun
rungeSCPoly
rungeSCPPoly
rungeSCFrac

# fracSinModFun
fracSinBallFun
fracSinDBallFun
fracSinPoly
fracSinPPoly
fracSinFrac

fracSinSCBallFun
fracSinSCDBallFun
fracSinSCPoly
fracSinSCPPoly
fracSinSCFrac

# hatBallFun
# hatDBallFun
# hatPoly
# hatPPoly
