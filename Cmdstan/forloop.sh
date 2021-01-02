#!/bin/bash

for i in {1..3}
      do
      covid19_multi/Stan/ModeloQR_reduce sample data file=covid19_multi/Cmdstan/sin_jer.json init=covid19_multi/Cmdstan/inits_${i}.json \
      output file=covid19_multi/Cmdstan/sin_jer_${i}.csv
    done
