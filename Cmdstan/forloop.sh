#!/bin/bash

# sbatch  -n 16 --mem-per-cpu=250M -t 01:00:00 -o $SCRATCH/test/test.out -J test $HOME/covid19_multi/Cmdstan/forloop.sh


export STAN_NUM_THREADS=-1

for i in {1..3}
      do
      $HOME/covid19_multi/Stan/ModeloJer2QRhosp_reduce sample data file=$HOME/covid19_multi/Cmdstan/jer_2modi.json init=$HOME/covid19_multi/Cmdstan/inits_${i}.json \
      output file=$SCRATCH/test/jer_2modi_${i}.csv
    done
