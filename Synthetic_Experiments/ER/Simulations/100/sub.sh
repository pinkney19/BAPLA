#!/bin/bash

#SBATCH -J ER_sims_100
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e sims_CG%.e
#SBATCH -o sims_CG%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/ER/100/sims/Sim_Study.R $x
