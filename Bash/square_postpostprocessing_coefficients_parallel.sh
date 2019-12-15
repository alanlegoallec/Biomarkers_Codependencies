#!/bin/bash
module load pandoc/2.1.1

demo_groups=( "all" "male" "female" "white" "hispanic" "black" "controlS1" "controlS2" "controlE1" "controlE2" "MvsF" "WvsH" "WvsB" "CS1vsCS2" "CE1vsCE2" )

for demo_group in "${demo_groups[@]}"
do
job_name="c-$demo_group.job"
out_file="./../errors_and_outputs/c-$demo_group.out"
err_file="./../errors_and_outputs/c-$demo_group.err"
memory=8G
n_cores=1
sbatch --error=$err_file --output=$out_file --job-name=$job_name --mem-per-cpu=$memory -t 200 -c $n_cores square_postpostprocessing_coefficients.sh $n_cores $demo_group
done

echo finished
