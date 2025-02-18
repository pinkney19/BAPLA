#!/bin/bash

#SBATCH -J f_exp_tuning_2
#SBATCH -c 45
#SBATCH --array=1-4%2
#SBATCH -e f%.e
#SBATCH -o f%.out
#SBATCH --mem-per-cpu=3G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/f_exp/CG_gamma/tuning/Parameter_Tuning_storm.R $x
