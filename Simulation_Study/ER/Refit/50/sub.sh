#!/bin/bash

#SBATCH -J ER_50_refit
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e refit_ER%.e
#SBATCH -o refit_ER%.out
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA/ER/50/refit/refit_storm.R $x
