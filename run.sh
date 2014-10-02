#!/bin/bash
instanceset=$1
cpus=$2
datain="data.in"
dataout="data.out"
mem="4"

./parallel_experiments.sh -j solvers.jar -c instances/$instanceset/$datain -o instances/$instanceset/$dataout -m $mem"g" -v -s $cpus/:  

./addhead.sh $1
