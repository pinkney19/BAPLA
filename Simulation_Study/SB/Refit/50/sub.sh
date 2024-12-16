#!/bin/bash

#SBATCH -J SB_50_refit
#SBATCH -c 42
#SBATCH --array=1-8%2
#SBATCH -e refit_SB%.e
#SBATCH -o refit_SB%.out
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA/SB/50/refit/refit_storm.R $x
