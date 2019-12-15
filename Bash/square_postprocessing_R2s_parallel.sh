#!/bin/bash
module load pandoc/2.1.1

halves=( "train" "test" )
sides=( "1" "2" "12" )

for half in "${halves[@]}"
do
for side in "${sides[@]}"
do
job_name="r-$side-$half.job"
out_file="./../errors_and_outputs/r-$side-$half.out"
err_file="./../errors_and_outputs/r-$side-$half.err"
memory=8G
n_cores=1
sbatch --error=$err_file --output=$out_file --job-name=$job_name --mem-per-cpu=$memory -t 300 -c $n_cores square_postprocessing_R2s.sh $n_cores $half $side
done
done
