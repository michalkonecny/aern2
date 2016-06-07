#!/bin/bash

fnrepsmain=./aern2-fnreps-ops
logcsv=log.csv

function runOnce
{
    aclog="$dir/runFun-$op-$fn-$repr-$ac.log"
    /usr/bin/time -v $fnrepsmain $op $fn $repr $ac >& $aclog
    utime=`grep "User time (seconds)" $aclog | sed 's/^.*: //'`
    stime=`grep "System time (seconds)" $aclog | sed 's/^.*: //'`
    mem=`grep "Maximum resident set size (kbytes)" $aclog | sed 's/^.*: //'`
    now=`date`
    echo "$now,$op,$fn,$repr,$ac,,,,$utime,$stime,$mem" >> $logcsv
}

function runge
{

repr=fun
fn=runge
dir=$fn
op=max
for ac in 05 10 15 20 25 30
do
    runOnce
done

op=integrate
for ac in 05 10 15 20
do
    runOnce
done

repr=dfun
for ac in 05 10 15 20 25 30
do
    runOnce
done

}

function xrunge
{
repr=fun
fn=xrunge
dir=$fn
op=max
for ac in 05 10 15 20 25 30
do
    runOnce
done

op=integrate
for ac in 05 10 15 20
do
    runOnce
done

repr=dfun
for ac in 05 10 15 20 25 30
do
    runOnce
done
}

runge
xrunge