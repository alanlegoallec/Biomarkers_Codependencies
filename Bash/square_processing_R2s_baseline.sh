#!/bin/bash
demo_groups=( "all" "male" "female" "white" "hispanic" "black" "controlS1" "controlS2" "controlE1" "controlE2" )

for demo_group in "${demo_groups[@]}"
do
job_name="pr2-1-$demo_group.job"
out_file="./../errors_and_outputs/pr2-1-$demo_group.out"
err_file="./../errors_and_outputs/pr2-1-$demo_group.err"
memory=1G
n_cores=1
sbatch --error=$err_file --output=$out_file --job-name=$job_name --mem-per-cpu=$memory -t 300 -c $n_cores square_processing_R2s.sh $n_cores $demo_group 1
done

echo finished
