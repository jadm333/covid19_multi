#!/bin/bash

for i in {1..3}
      do
      ./ModeloQR_reduce sample data file=/home/juandiaz/covid19_multi/Cmdstan/sin_jer.json init=/home/juandiaz/covid19_multi/Cmdstan/inits1_${i}.json \
      output file=sin_jer_${i}.csv
    done