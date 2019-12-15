#!/bin/bash
#SBATCH -c 1
#SBATCH -t 10
#SBATCH -p priority
#SBATCH --mem-per-cpu=8G
#SBATCH --open-mode=truncate
#SBATCH --job-name=s-pre
#SBATCH -o ./../errors_and_outputs/s-pre.out 
#SBATCH -e ./../errors_and_outputs/s-pre.err
#SBATCH --open-mode=truncate
#SBATCH --mail-type=END
Rscript ./../Scripts/Square_preprocessing_sw.R
