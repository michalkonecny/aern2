#!/bin/bash

benchset=$1
benchmain=aern2-fun-chPoly-benchOp
resultscsv=$benchset.csv

if [ "$benchset" == "fieldops" ]; then
    mode="SummaryCSV"
#    runSet="b_add; b_mul"
#     runSet="b_mul"
#     runSet="b_div"
    runSet="b_add; b_mul; b_div"
fi

# put headers in the results csv file:
if [ ! -f $resultscsv ]; then
  if [ "$mode" == "CSV" ]; then
    echo "op,deg,prec,i,duration,ac" > $resultscsv
    if [ $? != 0 ]; then exit 1; fi
  fi
  if [ "$mode" == "SummaryCSV" ]; then
    echo "op,deg,prec,count,durPrep,durResMean,durResMax,durResStDev,acWorst,acBest" > $resultscsv
    if [ $? != 0 ]; then exit 1; fi
  fi
fi

function runOne
# parameters:
#  $mode
#  $op
#  $deg
#  $prec
#  $ac
#  $count
{
    command="$benchmain $mode $op $deg $prec $ac $count"
    $command >> $resultscsv
    if [ $? != 0 ]; then exit 1; fi
}

#################
### operations
#################

function b_add
{
    op=add
    prec=1000
    ac="exact"
    count=100
    for deg in 10 20 30 40 50 60 70 80 90 100
    do
        runOne
    done
}

function b_mul
{
    op=mul
    prec=1000
    ac="exact"
    count=100
    for deg in 10 20 30 40 50 60 70 80 90 100
    do
        runOne
    done
}


function b_div
{
    op=div
    prec=1000
    ac="10"
    count=100
    for deg in 10 20 30 40 50 60 70 80 90 100
    do
        runOne
    done
}


eval $runSet
