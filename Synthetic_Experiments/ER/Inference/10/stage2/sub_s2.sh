#!/bin/bash

#SBATCH -J ER_inf_10_refit
#SBATCH -c 21
#SBATCH --array=1-8%2
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=3G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/ER/10/inference/s2/Inference_BAPLA_S2.R $x
