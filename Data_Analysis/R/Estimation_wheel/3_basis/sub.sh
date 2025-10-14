#!/bin/bash

#SBATCH -J DA_wheel_3
#SBATCH -c 41
#SBATCH --array=1-2%2
#SBATCH -e mb%.e
#SBATCH -o mb%.out
#SBATCH --mem-per-cpu=3G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/DA_wheel/3_basis/Estimation_STORM.R $x
