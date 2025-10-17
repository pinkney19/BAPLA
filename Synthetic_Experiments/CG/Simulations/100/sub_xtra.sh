#!/bin/bash

#SBATCH -J CG_xtra_sims_100
#SBATCH -c 41
#SBATCH --array=1-1%1
#SBATCH -e sims_CG%.e
#SBATCH -o sims_CG%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/CG/100/sims/Sim_Study_Extra.R $x
