#!/bin/bash
module load pandoc/2.1.1

distances=( "biomarkers" "coefficients" "bmVScoef" )
demo_groups=( "all" "male" "female" "white" "hispanic" "black" "controlS1" "controlS2" "controlE1" "controlE2" "MvsF" "WvsH" "WvsB" "CS1vsCS2" "CE1vsCE2" )

for distance in "${distances[@]}"
do
for demo_group in "${demo_groups[@]}"
do
job_name="pp-cor-$distance-$demo_group.job"
out_file="./../errors_and_outputs/ppc-$distance-$demo_group.out"
err_file="./../errors_and_outputs/ppc-$distance-$demo_group.err"
memory=8G
n_cores=1
sbatch --error=$err_file --output=$out_file --job-name=$job_name --mem-per-cpu=$memory -t 200 -c $n_cores square_postpostprocessing_correlations.sh $n_cores $distance $demo_group
done
done

echo finished
