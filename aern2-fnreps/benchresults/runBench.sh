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
    repr=fun; op=max; paramss="$repr_fun_max_paramss"; repr_fun_max_paramss=""
    runForParamss
  fi
  if [ "$repr_fun_itg_paramss" != "" ]; then
    repr=fun; op=integrate; paramss="$repr_fun_itg_paramss"; repr_fun_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_ball_max_paramss" != "" ]; then
    repr=ball; op=max; paramss="$repr_ball_max_paramss"; repr_ball_max_paramss=""
    runForParamss
  fi
  if [ "$repr_ball_itg_paramss" != "" ]; then
    repr=ball; op=integrate; paramss="$repr_ball_itg_paramss"; repr_ball_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_dball_max_paramss" != "" ]; then
    repr=dball; op=max; paramss="$repr_dball_max_paramss"; repr_dball_max_paramss=""
    runForParamss
  fi
  if [ "$repr_dball_itg_paramss" != "" ]; then
    repr=dball; op=integrate; paramss="$repr_dball_itg_paramss"; repr_dball_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_poly_max_paramss" != "" ]; then
    repr=poly; op=max; paramss="$repr_poly_max_paramss"; repr_poly_max_paramss=""
    runForParamss
  fi
  if [ "$repr_poly_itg_paramss" != "" ]; then
    repr=poly; op=integrate; paramss="$repr_poly_itg_paramss"; repr_poly_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_ppoly_max_paramss" != "" ]; then
    repr=ppoly; op=max; paramss="$repr_ppoly_max_paramss"; repr_ppoly_max_paramss=""
    runForParamss
  fi
  if [ "$repr_ppoly_itg_paramss" != "" ]; then
    repr=ppoly; op=integrate; paramss="$repr_ppoly_itg_paramss"; repr_ppoly_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_frac_max_paramss" != "" ]; then
    repr=frac; op=max; paramss="$repr_frac_max_paramss"; repr_frac_max_paramss=""
    runForParamss
  fi
  if [ "$repr_frac_itg_paramss" != "" ]; then
    repr=frac; op=integrate; paramss="$repr_frac_itg_paramss"; repr_frac_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_lpoly_max_paramss" != "" ]; then
    repr=lpoly; op=max; paramss="$repr_lpoly_max_paramss"; repr_lpoly_max_paramss=""
    runForParamss
  fi
  if [ "$repr_lpoly_itg_paramss" != "" ]; then
    repr=lpoly; op=integrate; paramss="$repr_lpoly_itg_paramss"; repr_lpoly_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_lppoly_max_paramss" != "" ]; then
    repr=lppoly; op=max; paramss="$repr_lppoly_max_paramss"; repr_lppoly_max_paramss=""
    runForParamss
  fi
  if [ "$repr_lppoly_itg_paramss" != "" ]; then
    repr=lppoly; op=integrate; paramss="$repr_lppoly_itg_paramss"; repr_lppoly_itg_paramss=""
    runForParamss
  fi
  if [ "$repr_lfrac_max_paramss" != "" ]; then
    repr=lfrac; op=max; paramss="$repr_lfrac_max_paramss"; repr_lfrac_max_paramss=""
    runForParamss
  fi
  if [ "$repr_lfrac_itg_paramss" != "" ]; then
    repr=lfrac; op=integrate; paramss="$repr_lfrac_itg_paramss"; repr_lfrac_itg_paramss=""
    runForParamss
  fi
}

#################
### sine+cos
#################

function sinecosAllReprs
{
  repr_fun_max_paramss="10 15 20";
  repr_fun_itg_paramss="06 08 09 10";
  repr_ball_max_paramss="10 15 20 25";
  repr_ball_itg_paramss="10 12 13 14";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="10 15 20 25 30";
  repr_poly_max_paramss="30 50 70 90 110";
  repr_poly_itg_paramss="30 50 70 90 110";
  repr_lpoly_max_paramss="10 30 50 70 90";
  repr_lpoly_itg_paramss="10 30 50 70 90";

  fn=sine+cos; dir=$fn;
  runForAllReprs
}

#################
### sine+cospi
#################

function sinecospiAllReprs
{
  # repr_fun_max_paramss="10 15 20";
  # repr_fun_itg_paramss="06 08 09 10";
  repr_ball_max_paramss="10 15 20 25 30 35";
  repr_ball_itg_paramss="10 12 13";
  repr_dball_max_paramss="10 20 30 40 45 50";
  repr_dball_itg_paramss="10 15 20 21 22";
  repr_poly_max_paramss="30 50 70 90 110";
  repr_poly_itg_paramss="30 50 70 90 110";
  repr_lpoly_max_paramss="10 30 50 70 90";
  repr_lpoly_itg_paramss="10 30 50 70 90";

  fn=sine+cospi; dir=$fn;
  runForAllReprs
}

#################
### sinesine
#################

function sinesineAllReprs
{
  # repr_fun_max_paramss="10 15 20";
  # repr_fun_itg_paramss="06 08";
  repr_ball_max_paramss="10 30 50 70 90";
  repr_ball_itg_paramss="05 10 12 13";
  repr_dball_max_paramss="10 30 50 70 90";
  repr_dball_itg_paramss="05 10 15 20 25";
  repr_poly_max_paramss="30 40 50 70 90";
  repr_poly_itg_paramss="30 40 50 70 90";
  repr_lpoly_max_paramss="30 40 50 70 90";
  repr_lpoly_itg_paramss="30 40 50 70 90";

  fn=sinesine; dir=$fn;
  runForAllReprs
}

#################
### sinesine+cos
#################

function sinesine+cosAllReprs
{
  repr_ball_max_paramss="10 20 25 30 32";
  repr_ball_itg_paramss="05 10 12 13";
  repr_dball_max_paramss="10 20 30 40 42 44";
  repr_dball_itg_paramss="05 10 15 20 21";
  repr_poly_max_paramss="30 40 50 70 90";
  repr_poly_itg_paramss="30 40 50 70 90";
  repr_lpoly_max_paramss="30 40 50 70 90";
  repr_lpoly_itg_paramss="30 40 50 70 90";

  fn=sinesine+cos; dir=$fn;
  runForAllReprs
}

#################
### runge
#################

function rungeAllReprs
{
  repr_ball_max_paramss="10 20 30 40";
  repr_ball_itg_paramss="05 10 15 17";
  repr_dball_max_paramss="10 20 30 40";
  repr_dball_itg_paramss="05 10 15 20 25";
  repr_poly_max_paramss="01 08 20";
  repr_poly_itg_paramss="01 08 20";
  repr_ppoly_max_paramss="10 20 40 60 80 100";
  repr_ppoly_itg_paramss="10 20 40 60 80 100";
  repr_frac_max_paramss="10 20 40 60 80 100";
  repr_frac_itg_paramss="30 40 60 80 100";
  repr_lpoly_max_paramss="20 40 60 80 100";
  repr_lpoly_itg_paramss="20 40 60 80 100";
  repr_lppoly_max_paramss="20 40 60 80 100";
  repr_lppoly_itg_paramss="20 40 60 80 100";
  repr_lfrac_max_paramss="20 40 60 80 100";
  repr_lfrac_itg_paramss="20 40 60 80 100";

  fn=runge; dir=$fn;
  runForAllReprs
}

#################
### rungeSC
#################

function rungeSCAllReprs
{
  repr_ball_max_paramss="05 10 15 20 25 30 32";
  repr_ball_itg_paramss="05 10 12 15";
  repr_dball_max_paramss="10 20 30 40 43 46";
  repr_dball_itg_paramss="05 10 15 20 25";
  repr_poly_max_paramss="05 12";
  repr_poly_itg_paramss="05 12";
  repr_ppoly_max_paramss="10 20 40 60 80 100";
  repr_ppoly_itg_paramss="10 20 40 60 80 100";
  repr_frac_max_paramss="10 20 40 60 80 100";
  repr_frac_itg_paramss="40 60 80 100";
  repr_lpoly_max_paramss="20 40 60 80 100";
  repr_lpoly_itg_paramss="20 40 60 80 100";
  repr_lppoly_max_paramss="20 40 60 80 100";
  repr_lppoly_itg_paramss="20 40 60 80 100";
  repr_lfrac_max_paramss="20 40 60 80 100";
  repr_lfrac_itg_paramss="20 40 60 80 100";

  fn=rungeSC; dir=$fn;
  runForAllReprs
}

#################
### fracSin
#################

function fracSinAllReprs
{
  repr_ball_max_paramss="10 20 30";
  repr_ball_itg_paramss="05 10 15";
  repr_dball_max_paramss="10 20 30";
  repr_dball_itg_paramss="05 10 15 20";
  repr_poly_max_paramss="05 08 10";
  repr_poly_itg_paramss="05 08 10";
  repr_ppoly_max_paramss="10 15 20 25";
  repr_ppoly_itg_paramss="10 15 20 25";
  repr_frac_max_paramss="10 20 40 60 80 100";
  repr_frac_itg_paramss="10 15 20 25";
  repr_lpoly_max_paramss="10 20 40 60 80 100";
  repr_lpoly_itg_paramss="10 20 40 60 80 100";
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="10 20 40 60 80 100";
  repr_lfrac_max_paramss="10 20 40 60 80 100";
  repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=fracSin; dir=$fn;
  runForAllReprs
}

#################
### fracSinSC
#################

function fracSinSCAllReprs
{
  repr_ball_max_paramss="10 20 25 30";
  repr_ball_itg_paramss="05 10 12 13";
  repr_dball_max_paramss="10 20 30 40 43 46";
  repr_dball_itg_paramss="05 10 15 20 21 22";
  repr_poly_max_paramss="01 05";
  repr_poly_itg_paramss="01 05";
  repr_ppoly_max_paramss="10 15 20";
  repr_ppoly_itg_paramss="10 15 20";
  repr_frac_max_paramss="10 20 40 60 80 100";
  # repr_frac_itg_paramss="02 05 10";
    # unable to get a good result because don't know how to
    #   set effort for translation to PPoly separately from the integration effort
  repr_lpoly_max_paramss="10 20 40 60 80 100";
  repr_lpoly_itg_paramss="10 20 40 60";
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="10 20 40 60 80 100";
  repr_lfrac_max_paramss="10 20 40 60 80 100";
  repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=fracSinSC; dir=$fn;
  runForAllReprs
}

#################
### bumpy
#################

function bumpyAllReprs
{

  repr_ball_max_paramss="10 20 40 60 80 100";
  repr_ball_itg_paramss="10 12 13 14";
  repr_dball_max_paramss="10 20 40 60 80 100";
  repr_dball_itg_paramss="10 15 20 25 30";
  repr_poly_max_paramss="02 03 04";
  repr_poly_itg_paramss="02 03 04";
  repr_ppoly_max_paramss="10 20 40 60 80 100";
  repr_ppoly_itg_paramss="10 20 40 60 80 100";
  # repr_frac_max_paramss="08 09 10";
  # repr_frac_itg_paramss="08 09 10";
  repr_lpoly_max_paramss="10 20 40 60 80 100";
  repr_lpoly_itg_paramss="10 20 40 60 81 100"; # gets stuck to 80 - bug?
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="10 20 40 60 80 100";
  # repr_lfrac_max_paramss="10 20 40 60 80 100";
  # repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=bumpy; dir=$fn;
  runForAllReprs
}

#################
### bumpy2
#################

function bumpy2AllReprs
{
  repr_ball_max_paramss="10 20 25 30";
  repr_ball_itg_paramss="05 10 12 13 14";
  repr_dball_max_paramss="10 20 30 40 50 60 70 80 90 100";
  repr_dball_itg_paramss="05 10 15 20 21 22";
  # repr_poly_max_paramss="01 02";
  # repr_poly_itg_paramss="01 02";
  repr_ppoly_max_paramss="05 10 15 20 25";
  repr_ppoly_itg_paramss="05 10 15 20 25";
  # repr_frac_max_paramss="01 03";
  # repr_frac_itg_paramss="01 03";
  # repr_lpoly_max_paramss="10 20 40 60 80 100";
  # repr_lpoly_itg_paramss="10 20 40 60 80 100";
  repr_lppoly_max_paramss="10 20 40 60 80 100";
  repr_lppoly_itg_paramss="20 25 30 40 60 80 100";
  # repr_lfrac_max_paramss="10 20 40 60 80 100";
  # repr_lfrac_itg_paramss="10 20 40 60 80 100";

  fn=bumpy2; dir=$fn;
  runForAllReprs
}

sinecosAllReprs
sinecospiAllReprs
sinesineAllReprs
sinesine+cosAllReprs
# rungeAllReprs
rungeSCAllReprs
# fracSinAllReprs
fracSinSCAllReprs
bumpyAllReprs
bumpy2AllReprs
