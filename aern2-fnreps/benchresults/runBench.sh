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

function runForAllReprs
{
  if [ "$repr_fun_max_paramss" != "" ]; then
    repr=fun; op=max; paramss="$repr_fun_max_paramss"; runForParamss
  fi
  if [ "$repr_fun_itg_paramss" != "" ]; then
    repr=fun; op=integrate; paramss="$repr_fun_itg_paramss"; runForParamss
  fi
  if [ "$repr_ball_max_paramss" != "" ]; then
    repr=ball; op=max; paramss="$repr_ball_max_paramss"; runForParamss
  fi
  if [ "$repr_ball_itg_paramss" != "" ]; then
    repr=ball; op=integrate; paramss="$repr_ball_itg_paramss"; runForParamss
  fi
  if [ "$repr_dball_max_paramss" != "" ]; then
    repr=dball; op=max; paramss="$repr_dball_max_paramss"; runForParamss
  fi
  if [ "$repr_dball_itg_paramss" != "" ]; then
    repr=dball; op=integrate; paramss="$repr_dball_itg_paramss"; runForParamss
  fi
  if [ "$repr_poly_max_paramss" != "" ]; then
    repr=poly; op=max; paramss="$repr_poly_max_paramss"; runForParamss
  fi
  if [ "$repr_poly_itg_paramss" != "" ]; then
    repr=poly; op=integrate; paramss="$repr_poly_itg_paramss"; runForParamss
  fi
  if [ "$repr_ppoly_max_paramss" != "" ]; then
    repr=ppoly; op=max; paramss="$repr_ppoly_max_paramss"; runForParamss
  fi
  if [ "$repr_ppoly_itg_paramss" != "" ]; then
    repr=ppoly; op=integrate; paramss="$repr_ppoly_itg_paramss"; runForParamss
  fi
  if [ "$repr_frac_max_paramss" != "" ]; then
    repr=frac; op=max; paramss="$repr_frac_max_paramss"; runForParamss
  fi
  if [ "$repr_frac_itg_paramss" != "" ]; then
    repr=frac; op=integrate; paramss="$repr_frac_itg_paramss"; runForParamss
  fi
  if [ "$repr_lpoly_max_paramss" != "" ]; then
    repr=lpoly; op=max; paramss="$repr_lpoly_max_paramss"; runForParamss
  fi
  if [ "$repr_lpoly_itg_paramss" != "" ]; then
    repr=lpoly; op=integrate; paramss="$repr_lpoly_itg_paramss"; runForParamss
  fi
  if [ "$repr_lppoly_max_paramss" != "" ]; then
    repr=lppoly; op=max; paramss="$repr_lppoly_max_paramss"; runForParamss
  fi
  if [ "$repr_lppoly_itg_paramss" != "" ]; then
    repr=lppoly; op=integrate; paramss="$repr_lppoly_itg_paramss"; runForParamss
  fi
  if [ "$repr_lfrac_max_paramss" != "" ]; then
    repr=lfrac; op=max; paramss="$repr_lfrac_max_paramss"; runForParamss
  fi
  if [ "$repr_lfrac_itg_paramss" != "" ]; then
    repr=lfrac; op=integrate; paramss="$repr_lfrac_itg_paramss"; runForParamss
  fi
}

#################
### sine+cos
#################

function sinecosAllReprs
{
  repr_fun_max_paramss="10 15 20";
  repr_fun_itg_paramss="10 12";
  repr_ball_max_paramss="10 15 20 25";
  repr_ball_itg_paramss="10 12 14";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="10 15 20 25 30";
  repr_poly_max_paramss="10 30 50 70 90 110";
  repr_poly_itg_paramss="10 30 50 70 90 110";
  repr_lpoly_max_paramss="10 30 50 70 90 110";
  repr_lpoly_itg_paramss="10 30 50 70 90 110";

  fn=sine+cos; dir=$fn;
  runForAllReprs
}

#################
### sinesine
#################

function sinesineAllReprs
{
  repr_ball_max_paramss="10 30 50 70 90";
  repr_ball_itg_paramss="05 10 15";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25 30";
  repr_poly_max_paramss="10 20 30 40 50 70 90";
  repr_poly_itg_paramss="10 20 30 40 50 70 90";
  repr_lpoly_max_paramss="10 20 30 40 50 70 90";
  repr_lpoly_itg_paramss="10 20 30 40 50 70 90";

  fn=sinesine; dir=$fn;
  runForAllReprs
}

#################
### sinesine+cos
#################

function sinesine+cosAllReprs
{
  repr_ball_max_paramss="10 20 30";
  repr_ball_itg_paramss="05 10 13";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25 28";
  repr_poly_max_paramss="10 20 30 40 50 70 90";
  repr_poly_itg_paramss="10 20 30 40 50 70 90";
  repr_lpoly_max_paramss="10 20 30 40 50 70 90";
  repr_lpoly_itg_paramss="10 20 30 40 50 70 90";

  fn=sinesine+cos; dir=$fn;
  runForAllReprs
}

#################
### runge
#################

function rungeAllReprs
{
  repr_ball_max_paramss="10 30 50 70 90";
  repr_ball_itg_paramss="05 10 15 17";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25 30 35";
  repr_poly_max_paramss="1 8 20";
  repr_poly_itg_paramss="1 8 20";
  repr_ppoly_max_paramss="10 20 40 60 80";
  repr_ppoly_itg_paramss="10 20 40 60 80";
  repr_frac_max_paramss="10 20 40 60 80";
  repr_frac_itg_paramss="10 20 40 60 80";
  repr_lpoly_max_paramss="10 20 40 60 80 100";
  repr_lpoly_itg_paramss="10 20 40 60 80 100";
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="10 20 40 60 80 100";
  repr_lfrac_max_paramss="10 20 40 60 80 100";
  repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=runge; dir=$fn;
  runForAllReprs
}

#################
### rungeSC
#################

function rungeSCAllReprs
{
  repr_ball_max_paramss="05 10 15 20 25 30";
  repr_ball_itg_paramss="05 10 15";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25 30";
  repr_poly_max_paramss="0 8 20";
  repr_poly_itg_paramss="0 8 20";
  repr_ppoly_max_paramss="10 20 40";
  repr_ppoly_itg_paramss="10 20 40";
  repr_frac_max_paramss="10 20 40 60 80";
  repr_frac_itg_paramss="10 20 40";
  repr_lpoly_max_paramss="10 20 40 60 80 100";
  repr_lpoly_itg_paramss="10 20 40 60 80 100";
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="10 20 40 60 80 100";
  repr_lfrac_max_paramss="10 20 40 60 80 100";
  repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=rungeSC; dir=$fn;
  runForAllReprs
}

#################
### fracSin
#################

function fracSinAllReprs
{
  repr_ball_max_paramss="10 30 50 70 90";
  repr_ball_itg_paramss="05 10 15";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25 30";
  repr_poly_max_paramss="10 20";
  repr_poly_itg_paramss="10 20";
  repr_ppoly_max_paramss="10 15 20";
  repr_ppoly_itg_paramss="10 15 20";
  repr_frac_max_paramss="10 20 40 60 80 100";
  repr_frac_itg_paramss="10 15 20";
  repr_lpoly_max_paramss="10 20 40 60";
  repr_lpoly_itg_paramss="10 20 40 60";
  repr_lppoly_max_paramss="10 20 40 60 80";
  repr_lppoly_itg_paramss="10 20 40 60";
  repr_lfrac_max_paramss="10 20 40 60 80";
  repr_lfrac_itg_paramss="10 20 40 60";

  fn=fracSin; dir=$fn;
  runForAllReprs
}

#################
### fracSinSC
#################

function fracSinSCAllReprs
{
  repr_ball_max_paramss="10 20 25 30";
  repr_ball_itg_paramss="05 10 15";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25";
  repr_poly_max_paramss="10 20";
  repr_poly_itg_paramss="10 20";
  repr_ppoly_max_paramss="10 15 20";
  repr_ppoly_itg_paramss="10 15 20";
  repr_frac_max_paramss="10 20 40 60 80";
  repr_frac_itg_paramss="10 15 20";
  repr_lpoly_max_paramss="10 20 40 60";
  repr_lpoly_itg_paramss="10 20 40 60";
  repr_lppoly_max_paramss="10 20 40 60 80";
  repr_lppoly_itg_paramss="10 20 40 60";
  repr_lfrac_max_paramss="10 20 40 60 80";
  repr_lfrac_itg_paramss="10 20 40 60";

  fn=fracSinSC; dir=$fn;
  runForAllReprs
}

sinecosAllReprs
sinesineAllReprs
sinesine+cosAllReprs
rungeAllReprs
rungeSCAllReprs
fracSinAllReprs
fracSinSCAllReprs
