#!/bin/bash

benchset=$1
benchmain=aern2-real-benchOp
# benchmain=ireal-benchOp
gnutime=/usr/bin/time
logsdir=logs
useoldlogs=true

resultscsv=$benchset.csv

if [ "$useoldlogs" == "true" -a -e "$resultscsv" ]; then
  echo "The result file $resultscsv already exists. I will not overwrite it."
  exit 1;
fi

# put headers in the results csv file:
if [ ! -f $resultscsv ]; then
  echo "Time,Op,Count,Accuracy(bits),UTime(s),STime(s),Mem(kB)" > $resultscsv
  if [ $? != 0 ]; then exit 1; fi
fi

if [ ! -e $logsdir ]; then
    mkdir $logsdir
    if [ $? != 0 ]; then exit 1; fi
fi

function runOne
# parameters:
#  $op
#  $count
#  $ac
{
    runlog="$logsdir/run-$op-$count-${ac}.log"
    command="$gnutime -v $benchmain $op $count $ac"
    echo -n $command
    if [ ! -f $runlog ] || grep -q "terminated" $runlog
        then
            echo " (running and logging in $runlog)"
            $command >& $runlog
            if [ $? != 0 ]; then rm $runlog; exit 1; fi
            newrun=true
        else
            echo " (using existing log $runlog)"
            newrun=false
    fi
    if [ "$useoldlogs" == "true" -o "$newrun" == "true" ]; then
      utime=`grep "User time (seconds)" $runlog | sed 's/^.*: //'`
      stime=`grep "System time (seconds)" $runlog | sed 's/^.*: //'`
      mem=`grep "Maximum resident set size (kbytes)" $runlog | sed 's/^.*: //'`
      # bits=`grep "accuracy: Bits " $runlog | sed 's/accuracy: Bits //'`
      now=`date`
      csvline="$now,$op,$count,$ac,${utime/0.00/0.01},${stime/0.00/0.01},$mem"
      echo "recording in $resultscsv: $csvline"
      echo $csvline >> $resultscsv
    fi
}

#################
### operations
#################

function b_exp
{
    op=exp
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}

function b_log
{
    op=log
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}

function b_sqrt
{
    op=sqrt
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}


function b_cos
{
    op=cos
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}


function b_add
{
    op=add
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}

function b_mul
{
    op=mul
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}


function b_div
{
    op=div
    count=1000
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}

function b_logistic
# parameters:
#  $count
{
    op=logistic
    for ac in 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000
    do
        runOne
    done
}


if [ "$benchset" == "fieldops" ]; then
b_add
b_mul
b_div
fi

if [ "$benchset" == "exp" ]; then
b_exp
fi

if [ "$benchset" == "ops" ]; then
b_exp
b_log
b_sqrt
b_cos
b_add
b_mul
b_div
fi

if [ "$benchset" == "logistic" ]; then
count=1000
b_logistic
count=2000
b_logistic
fi
