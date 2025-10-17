#!/bin/bash

#SBATCH -J ER_inf_100_inv
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/ER/100/inf/Inference_BAPLA_S1_Exp.R $x
