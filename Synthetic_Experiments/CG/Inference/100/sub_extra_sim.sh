#!/bin/bash

#SBATCH -J CG_inf_100extra
#SBATCH -c 41
#SBATCH --array=1-1%1
#SBATCH -e inf%.e
#SBATCH -o inf%.out
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA2/CG/100/inf/extra/Inference_BAPLA_extra_sim.R $x
