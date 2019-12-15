#!/bin/bash
#SBATCH -p short
#SBATCH --open-mode=truncate
#SBATCH --mail-type=FAIL
Rscript ./../Scripts/Square_preprocessing_hc.R O2 $1 $2

