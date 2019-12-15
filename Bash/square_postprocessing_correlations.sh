#!/bin/bash
#SBATCH -c 1
#SBATCH -t 80
#SBATCH -p priority
#SBATCH --mem-per-cpu=8G
#SBATCH --open-mode=truncate
#SBATCH --job-name=pp_correlations
#SBATCH -o ./../errors_and_outputs/pp-correlations.out 
#SBATCH -e ./../errors_and_outputs/pp-correlations.err 
#SBATCH --mail-type=END
Rscript ./../Scripts/Square_postprocessing_correlations.R
