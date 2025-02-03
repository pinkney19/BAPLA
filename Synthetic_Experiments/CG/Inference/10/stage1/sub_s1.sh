#!/bin/bash

#SBATCH -J CG_inf_10
#SBATCH -c 41
#SBATCH --array=1-8%2
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/CG/10/inference/Inference_BAPLA_S1.R $x
