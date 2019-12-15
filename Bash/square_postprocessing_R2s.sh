#!/bin/bash
#SBATCH -p short
#SBATCH --open-mode=truncate
#SBATCH --mail-type=FAIL
Rscript ./../Scripts/Square_postprocessing_R2s.R O2 $1 $2 $3
