#!/bin/bash

#SBATCH -J m25
#SBATCH -c 45
#SBATCH --array=1-4%2
#SBATCH -e f%.e
#SBATCH -o f%.out
#SBATCH --mem-per-cpu=3G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA_revisions/conerg/CG_normal/Sim_Study_m25.R $x
