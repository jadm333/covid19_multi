#!/bin/bash

# sbatch --array=1,2,3 --nodes=1 --ntasks-per-node=48 --mem=0 -t 16:00:00 -o $SCRATCH/sinjer/sinjer.out -J sinjer --mail-user=juan.diaz.martinez@mail.utoronto.ca --mail-type=ALL $HOME/covid19_multi/Cmdstan/forloop_sinjer.sh

echo "Job $SLURM_ARRAY_TASK_ID  start: $(date)"

export STAN_NUM_THREADS=-1

$HOME/covid19_multi/Stan/ModeloQR_reduce sample num_samples=750 num_warmup=750  data file=$HOME/covid19_multi/Cmdstan/sin_jer.json init=$HOME/covid19_multi/Cmdstan/inits_${SLURM_ARRAY_TASK_ID}.json \
output file=$SCRATCH/sinjer/sin_jer_${SLURM_ARRAY_TASK_ID}.csv

echo "Job $SLURM_ARRAY_TASK_ID  finish: $(date)"
