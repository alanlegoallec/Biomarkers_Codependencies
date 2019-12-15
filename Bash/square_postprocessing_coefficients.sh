#!/bin/bash
#SBATCH -c 1
#SBATCH -t 100
#SBATCH -p priority
#SBATCH --mem-per-cpu=8G
#SBATCH --open-mode=truncate
#SBATCH --job-name=pp_coefficients
#SBATCH -o ./../errors_and_outputs/p-coefficients.out 
#SBATCH -e ./../errors_and_outputs/p-coefficients.err
#SBATCH --open-mode=truncate
#SBATCH --mail-type=END
Rscript ./../Scripts/Square_postprocessing_coefficients.R
