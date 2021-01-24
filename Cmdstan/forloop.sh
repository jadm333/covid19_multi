#!/bin/bash

# sbatch --array=1,2,3 --nodes=1 --ntasks-per-node=48 --mem=0 -t 02:00:00 -o $SCRATCH/multi/jer2modi.out -J jer2modi --mail-user=juan.diaz.martinez@mail.utoronto.ca --mail-type=ALL $HOME/covid19_multi/Cmdstan/forloop.sh

echo "Job $SLURM_ARRAY_TASK_ID  start: $(date)"

export STAN_NUM_THREADS=-1

$HOME/covid19_multi/Stan/ModeloJer2QRhosp_reduce sample num_samples=750 num_warmup=500  data file=$HOME/covid19_multi/Cmdstan/jer_2modi.json init=$HOME/covid19_multi/Cmdstan/inits_${SLURM_ARRAY_TASK_ID}.json \
output file=$SCRATCH/multi/jer2modi_${SLURM_ARRAY_TASK_ID}.csv

echo "Job $SLURM_ARRAY_TASK_ID  finish: $(date)"
