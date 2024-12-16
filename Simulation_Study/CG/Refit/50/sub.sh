#!/bin/bash

#SBATCH -J CG_50_refit
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e refit_CG%.e
#SBATCH -o refit_CG%.out
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA/CG/50/refit/refit_storm.R $x
