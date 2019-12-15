#!/bin/bash
module load pandoc/2.1.1
sbatch square_postprocessing_coefficients.sh
sbatch square_postprocessing_correlations.sh
./square_postprocessing_R2s_parallel.sh

