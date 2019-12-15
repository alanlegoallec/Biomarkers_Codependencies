#!/bin/bash
equal_bin_sizes=false
demo_groups=( "all" "male" "female" "white" "hispanic" "black" "controlS1" "controlS2" "controlE1" "controlE2" )

for demo_group in "${demo_groups[@]}"
do
n=61
if [ "$equal_bin_sizes" = true ] ; then
case $demo_group in
        male|female|controlS1|controlS2 ) n=31;;
        white|hispanic|black|controlE1|controlE2 ) n=13;;
esac
fi
for ((i=2;i<=$n;i++));
do
job_name="pcc-$i-$demo_group.job"
out_file="./../errors_and_outputs/pcc-$i-$demo_group.out"
err_file="./../errors_and_outputs/pcc-$i-$demo_group.err"
memory=1G
n_cores=1
sbatch --error=$err_file --output=$out_file --job-name=$job_name --mem-per-cpu=$memory -t 100 -c $n_cores square_processing_coefficients_and_correlations.sh $n_cores $demo_group $i
done
done

echo finished
