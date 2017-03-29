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

function runForParamss
{
  for params in $paramss
  do
    runOne
  done
}

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
  fn=sine+cos; repr=fun
  dir=$fn

  op=max; paramss="10 15 20 25"; runForParamss
  op=integrate; paramss="10 12 14"; runForParamss
}

function sinecosBallFun
{
  fn=sine+cos; repr=ball
  dir=$fn

  op=max; paramss="10 15 20 25"; runForParamss
  op=integrate; paramss="10 12 14"; runForParamss
}

function sinecosDBallFun
{
  repr=dball;fn=sine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30"; runForParamss
  op=integrate; paramss="10 12 14 16 18 20"; runForParamss
}

function sinecosPoly
{
  repr=poly; fn=sine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30"; runForParamss
  op=integrate; paramss="10 12 14 16 18 20"; runForParamss
}

function sinecosLPoly
{
  repr=lpoly; fn=sine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30"; runForParamss
  op=integrate; paramss="10 12 14 16 18 20"; runForParamss
}

#################
### sinesine
#################

function sinesineModFun
{
  repr=fun; fn=sinesine
  dir=$fn

  op=max; paramss="10 15 20"; runForParamss
  op=integrate; paramss="5 7"; runForParamss
}

function sinesineBallFun
{
  repr=ball; fn=sinesine
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function sinesineDBallFun
{
  repr=dball; fn=sinesine
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function sinesinePoly
{
  repr=poly; fn=sinesine
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function sinesineLPoly
{
  repr=lpoly; fn=sinesine
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

#################
### sinesine+cos
#################

function sinesine+cosModFun
{
  repr=fun; fn=sinesine+cos
  dir=$fn

  op=max; paramss="10 15 20"; runForParamss
  op=integrate; paramss="05 07"; runForParamss
}

function sinesine+cosBallFun
{
  repr=ball; fn=sinesine+cos
  dir=$fn

  op=max; paramss="10 20 30"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function sinesine+cosDBallFun
{
  repr=dball; fn=sinesine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function sinesine+cosPoly
{
  repr=poly; fn=sinesine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function sinesine+cosLPoly
{
  repr=lpoly; fn=sinesine+cos
  dir=$fn

  op=max; paramss="10 15 20 25 30 35"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

#################
### runge
#################

function rungeModFun
{
  repr=fun; fn=runge
  dir=$fn

  op=max; paramss="05 35 65 100 120"; runForParamss
  op=integrate; paramss="05 10 15 20"; runForParamss
}

function rungeBallFun
{
  repr=ball; fn=runge
  dir=$fn

  op=max; paramss="05 35 65 100 120"; runForParamss
  op=integrate; paramss="05 10 15 20"; runForParamss
}

function rungeDBallFun
{
  repr=dball; fn=runge
  dir=$fn

  op=max; paramss="05 35 65 100"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function rungePoly
{
  repr=poly; fn=runge
  dir=$fn

  op=max; paramss="00 01 10"; runForParamss
  op=integrate; paramss="00 01 10"; runForParamss
}

function rungePPoly
{
  repr=ppoly; fn=runge
  dir=$fn

  op=max; paramss="10 20 40 80"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function rungeFrac
{
  repr=frac; fn=runge
  dir=$fn

  op=max; paramss="10 20 40 80 120"; runForParamss
  op=integrate; paramss="8 16 32 64"; runForParamss
}

function rungeLPoly
{
  repr=lpoly; fn=runge
  dir=$fn

  op=max; paramss="2 10"; runForParamss
  op=integrate; paramss="10 80 160"; runForParamss
}

function rungeLPPoly
{
  repr=lppoly; fn=runge
  dir=$fn

  op=max; paramss="10 20 40 80"; runForParamss
  op=integrate; paramss="2 10 80"; runForParamss
}

function rungeLFrac
{
  repr=lfrac; fn=runge
  dir=$fn

  op=max; paramss="10 20 40 80"; runForParamss
  op=integrate; paramss="2 10 80"; runForParamss
}

#################
### rungeSC
#################

function rungeSCModFun
{
  repr=fun; fn=rungeSC
  dir=$fn

  op=max; paramss="05 10 15 20 25 30"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function rungeSCBallFun
{
  repr=ball; fn=rungeSC
  dir=$fn

  op=max; paramss="05 10 15 20 25 30"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function rungeSCDBallFun
{
  repr=dball; fn=rungeSC
  dir=$fn

  op=max; paramss="05 10 20 30 40 50 52"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function rungeSCPoly
{
  repr=poly; fn=rungeSC
  dir=$fn

  op=max; paramss="-01 01 04"; runForParamss
  op=integrate; paramss="-01 01 04"; runForParamss
}

function rungeSCPPoly
{
  repr=ppoly; fn=rungeSC
  dir=$fn

  op=max; paramss="8 16 32"; runForParamss
  op=integrate; paramss="8 16 32"; runForParamss
}

function rungeSCFrac
{
  repr=frac; fn=rungeSC
  dir=$fn

  op=max; paramss="10 20 40 80 120"; runForParamss
  op=integrate; paramss="4 8 12 16"; runForParamss
}

function rungeSCLPoly
{
  repr=lpoly; fn=rungeSC
  dir=$fn

  op=max; paramss="10 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function rungeSCLPPoly
{
  repr=lppoly; fn=rungeSC
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function rungeSCLFrac
{
  repr=lfrac; fn=rungeSC
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

#################
### fracSin
#################

function fracSinModFun
{
  repr=fun; fn=fracSin
  dir=$fn

  op=max; paramss="05 15 25 35 45 55"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function fracSinBallFun
{
  repr=ball; fn=fracSin
  dir=$fn

  op=max; paramss="05 15 25 35 45 55"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function fracSinDBallFun
{
  repr=dball; fn=fracSin
  dir=$fn

  op=max; paramss="05 15 25 35 45 50"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function fracSinPoly
{
  repr=poly; fn=fracSin
  dir=$fn

  op=max; paramss="01 03"; runForParamss
  op=integrate; paramss="01 03"; runForParamss
}

function fracSinPPoly
{
  repr=ppoly; fn=fracSin
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function fracSinFrac
{
  repr=frac; fn=fracSin
  dir=$fn

  op=max; paramss="10 20 40 60"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function fracSinLPoly
{
  repr=lpoly; fn=fracSin
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function fracSinLPPoly
{
  repr=lppoly; fn=fracSin
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}

function fracSinLFrac
{
  repr=lfrac; fn=fracSin
  dir=$fn

  op=max; paramss="10 20 40"; runForParamss
  op=integrate; paramss="10 20 40"; runForParamss
}


#################
### fracSinSC
#################

function fracSinSCModFun
{
  repr=fun; fn=fracSinSC
  dir=$fn

  op=max; paramss="05 10 15"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function fracSinSCBallFun
{
  repr=ball; fn=fracSinSC
  dir=$fn

  op=max; paramss="05 10 15"; runForParamss
  op=integrate; paramss="05 10 15"; runForParamss
}

function fracSinSCDBallFun
{
  repr=dball; fn=fracSinSC
  dir=$fn

  op=max; paramss="05 10 15 20 25 30"; runForParamss
  op=integrate; paramss="05 10 15 20 25 30"; runForParamss
}

function fracSinSCPoly
{
  repr=poly; fn=fracSinSC
  dir=$fn

  op=max; paramss="01 03"; runForParamss
  op=integrate; paramss="01 03"; runForParamss
}

function fracSinSCPPoly
{
  repr=ppoly; fn=fracSinSC
  dir=$fn

  op=max; paramss="8 16 24"; runForParamss
  op=integrate; paramss="8 16 24"; runForParamss
}

function fracSinSCFrac
{
  repr=frac; fn=fracSinSC
  dir=$fn

  op=max; paramss="8 16 24"; runForParamss
  op=integrate; paramss="8 16 24"; runForParamss
}


function fracSinSCLPoly
{
  repr=lpoly; fn=fracSinSC
  dir=$fn

  op=max; paramss="8 16 24 32 40"; runForParamss
  op=integrate; paramss="8 16 24 32 40"; runForParamss
}

function fracSinSCLPPoly
{
  repr=lppoly; fn=fracSinSC
  dir=$fn

  op=max; paramss="8 16 24 32 40"; runForParamss
  op=integrate; paramss="8 16 24 32 40"; runForParamss
}

function fracSinSCLFrac
{
  repr=lfrac; fn=fracSinSC
  dir=$fn

  op=max; paramss="8 16 24 32 40"; runForParamss
  op=integrate; paramss="16 24 32 40 48"; runForParamss
}


sinecosModFun
sinecosBallFun
sinecosDBallFun
sinecosPoly
sinecosLPoly

sinesineModFun
sinesineBallFun
sinesineDBallFun
sinesinePoly
sinesineLPoly

sinesine+cosModFun
sinesine+cosBallFun
sinesine+cosDBallFun
sinesine+cosPoly
sinesine+cosLPoly

# rungeModFun
rungeBallFun
rungeDBallFun
rungePoly
rungePPoly
rungeFrac
rungeLPoly
rungeLPPoly
rungeLFrac

# rungeSCModFun
rungeSCBallFun
rungeSCDBallFun
rungeSCPPoly
rungeSCPoly
rungeSCFrac
rungeSCLPoly
rungeSCLPPoly
rungeSCLFrac

# fracSinModFun
fracSinBallFun
fracSinDBallFun
fracSinPPoly
fracSinPoly
fracSinFrac
fracSinLPoly
fracSinLPPoly
fracSinLFrac

fracSinSCBallFun
fracSinSCDBallFun
fracSinSCPoly
fracSinSCPPoly
fracSinSCFrac
fracSinSCLPoly
fracSinSCLPPoly
fracSinSCLFrac
