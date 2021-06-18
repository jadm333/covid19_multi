#!/bin/bash

# sbatch --array=1,2,3 --nodes=1 --ntasks-per-node=48 --mem=0 -t 16:00:00 -o $SCRATCH/jer1/jer1.out -J jer1 --mail-user=juan.diaz.martinez@mail.utoronto.ca --mail-type=ALL $HOME/covid19_multi/Cmdstan/forloop_jer1.sh

echo "Job $SLURM_ARRAY_TASK_ID  start: $(date)"

export STAN_NUM_THREADS=-1

$HOME/covid19_multi/Stan/ModeloJerQR_reduce sample num_samples=750 num_warmup=750  data file=$HOME/covid19_multi/Cmdstan/jer_1.json init=$HOME/covid19_multi/Cmdstan/inits_${SLURM_ARRAY_TASK_ID}.json \
output file=$SCRATCH/jer1/jer1_${SLURM_ARRAY_TASK_ID}.csv

echo "Job $SLURM_ARRAY_TASK_ID  finish: $(date)"
