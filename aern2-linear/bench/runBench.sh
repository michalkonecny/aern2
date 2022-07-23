#!/bin/bash

results_file=$1
if [ "$results_file" == "" ]; then echo "usage: $0 <results JS file name>"; exit 1; fi

benchmain=aern2-linear-matrix-benchmark

reuselogs="true"

# put headers in the results js file if it is new:
if [ ! -f $results_file ]
    then echo "const allData = [" > $results_file
    if [ $? != 0 ]; then exit 1; fi
fi

function runForBenchParamss
{
  for benchParams in $bparamss
  do
    for params in $precs
    do
      runOne
    done
  done
}

function runOne
# parameters:
#  $dir where to put individual logs
#  $bench
#  $benchParams
#  $method
#  $params
{
    runlog="$dir/run-$bench-$benchParams-$method-${params// /_}.log"
    echo -n /usr/bin/time -v $benchmain $bench $benchParams $method $params
    if [ ! -f $runlog ] || grep -q "terminated" $runlog
        then
            echo " (running and logging in $runlog)"
            /usr/bin/time -v $benchmain $bench $benchParams $method $params >& $runlog
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
  prec=`grep -i "ac = bits " $runlog | sed 's/^.*ac = bits \([0-9]*\).*$/\1/'`
  bits=`grep -i "accuracy" $runlog | sed 's/^.*bits \([0-9]*\).*$/\1/'`
  now=`date`
  echo "{time: \"$now\", bench: \"$bench\", param: $benchParams, method: \"$method\", prec: $prec, bits: $bits, utime: ${utime/0.00/0.01}, stime: ${stime/0.00/0.01}, mem: $mem }," >> $results_file
}

# Time,Bench,BenchParams,Method,AccuracyTarget(bits),Accuracy(bits),UTime(s),STime(s),Mem(kB)

function runForAllMethods
{
  if [ "$method_MPFloat_bparamss" != "" ]; then
    method="MPFloat"; bparamss="$method_MPFloat_bparamss" method_MPFloat_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_MPBall_bparamss" != "" ]; then
    method="MPBall"; bparamss="$method_MPBall_bparamss" method_MPBall_bparamss=""
    runForBenchParamss
  fi
  if [ "$method_MPBallViaFP_bparamss" != "" ]; then
    method="MPBallViaFP"; bparamss="$method_MPBallViaFP_bparamss" method_MPBallViaFP_bparamss=""
    runForBenchParamss
  fi
}

#################
### logistic
#################

function productAllMethods
{
  dims="10 20 30 50 80 130 210"
  precs="200 300 500 800 1300 2100";

  method_MPFloat_bparamss="$dims";
  method_MPBall_bparamss="$dims";
  method_MPBallViaFP_bparamss="$dims";

  bench="product"; dir="$bench";
  runForAllMethods
}

function solveAllMethods
{
  dims="10 20 30 50 80 130 210"
  precs="200 300 500 800 1300 2100 3400 5500";

  method_MPFloat_bparamss="$dims";
  method_MPBall_bparamss="$dims";
  method_MPBallViaFP_bparamss="$dims";

  bench="solve"; dir="$bench";
  runForAllMethods
}

productAllMethods
solveAllMethods
