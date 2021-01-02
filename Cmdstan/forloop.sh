#!/bin/bash

export STAN_NUM_THREADS=-1

for i in {1..3}
      do
      covid19_multi/Stan/ModeloJer2QRhosp_reduce sample data file=covid19_multi/Cmdstan/jer_2modi.json init=covid19_multi/Cmdstan/inits_${i}.json \
      output file=covid19_multi/Cmdstan/jer_2modi_${i}.csv
    done
