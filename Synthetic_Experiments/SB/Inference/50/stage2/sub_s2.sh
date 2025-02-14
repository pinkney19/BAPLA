#!/bin/bash

#SBATCH -J SB_inf_50_refit
#SBATCH -c 21
#SBATCH --array=1-8%2
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/SB/50/inference/s2/Inference_BAPLA_S2.R $x
