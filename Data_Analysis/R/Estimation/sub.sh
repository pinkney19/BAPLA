#!/bin/bash

#SBATCH -J Data_Analysis
#SBATCH -c 20
#SBATCH --array=1-4%4
#SBATCH -e mb%.e
#SBATCH -o mb%.out
#SBATCH --mem-per-cpu=3G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/Data_Analysis/Estimation_STORM.R $x
