#!/bin/bash

resultscsv=$1
if [ "$resultscsv" == "" ]; then echo "usage: $0 <results csv file name>"; exit 1; fi

benchmain=aern2-real-benchOp
gnutime=/usr/bin/time
logsdir=logs

if [ ! -e $logsdir ]; then
    mkdir $logsdir
    if [ $? != 0 ]; then exit 1; fi
fi

# put headers in the results csv file if it is new:
if [ ! -f $resultscsv ]; then
    echo "Time,Op,Count,Accuracy(bits),UTime(s),STime(s),Mem(kB)" > $resultscsv
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
            utime=`grep "User time (seconds)" $runlog | sed 's/^.*: //'`
            stime=`grep "System time (seconds)" $runlog | sed 's/^.*: //'`
            mem=`grep "Maximum resident set size (kbytes)" $runlog | sed 's/^.*: //'`
            # bits=`grep "accuracy: Bits " $runlog | sed 's/accuracy: Bits //'`
            now=`date`
            csvline="$now,$op,$count,$ac,${utime/0.00/0.01},${stime/0.00/0.01},$mem"
            echo "recording in $resultscsv: $csvline"
            echo $csvline >> $resultscsv
        else
            echo " (skipping due to existing log $runlog)"
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

b_exp
b_log
b_sqrt
b_cos
b_add
b_mul
b_div
