#!/bin/bash

#SBATCH -J CG_tuning_100
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e mb%.e
#SBATCH -o mb%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/CG/100/tuning/Parameter_Tuning_storm.R $x
