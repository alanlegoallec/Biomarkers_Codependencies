#!/bin/bash
#SBATCH -p short
#SBATCH --open-mode=truncate
#SBATCH --mail-type=FAIL
Rscript ./../Scripts/Square_postpostprocessing_correlations.R O2 $1 $2 $3

