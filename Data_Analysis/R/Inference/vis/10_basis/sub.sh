#!/bin/bash

#SBATCH -J vis10_inference
#SBATCH -c 45
#SBATCH --array=1-2%2
#SBATCH -e mb%.e
#SBATCH -o mb%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/inference_DA_invert/vis/10_basis/Inference_STORM.R $x
