#!/bin/bash

#SBATCH -J SB_sims_50
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e sims_CG%.e
#SBATCH -o sims_CG%.out
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA/SB/50/sims/Sim_Study.R $x
