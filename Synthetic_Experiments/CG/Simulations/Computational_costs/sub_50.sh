#!/bin/bash

#SBATCH -J Comp_cost_50
#SBATCH -c 41
#SBATCH --array=1-4%2
#SBATCH -e sims_CG%.e
#SBATCH -o sims_CG%.out
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=c.pinkney@lancaster.ac.uk

x=$SLURM_ARRAY_TASK_ID

srun Rscript ~/BAPLA_revisions/comp_cost/CG/50/Sim_Study_50p.R $x
