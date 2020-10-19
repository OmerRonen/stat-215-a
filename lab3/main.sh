#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --nodes=1

#SBATCH --mail-user=omer_ronen@berkeley.edu
#SBATCH --mail-type=ALL
#SBATCH -o sim.out

Rscript --no-save R/main.R sim.csv 0.8 100
