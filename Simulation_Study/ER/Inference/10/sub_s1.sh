#!/bin/bash

#SBATCH -J ER_inf_10
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA/ER/10/inference/s1/Inference_GAPLA_S1.R $x
