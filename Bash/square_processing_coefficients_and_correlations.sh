#!/bin/bash
#SBATCH -p short
#SBATCH --open-mode=truncate
#SBATCH --mail-type=FAIL
Rscript ./../Scripts/Square_processing_coefficients_and_correlations.R O2 $1 $2 $3

