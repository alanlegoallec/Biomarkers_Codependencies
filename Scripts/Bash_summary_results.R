#!/bin/bash
cd /n/groups/patel/Alan/Aging/Square/Results
if [ -d "Square_O2_results" ]; then
rm -r Square_O2_results
fi
mkdir Square_O2_results
cd Square_O2_results
#BIOMARKERS
mkdir Biomarkers
#COEFFICIENTS
mkdir Coefficients
cd Coefficients
mkdir Baseline
mkdir Heatmaps
mkdir LinearRegressions
cd LinearRegressions
mkdir Coefficients
mkdir Coefficients_var
cd ..
mkdir Scatterplots
cd Scatterplots
mkdir Baseline
mkdir Summary
cd ..
mkdir Volcano_Age
cd Volcano_Age
mkdir Coefficients
mkdir Coefficients_var
cd ..
mkdir Volcano_Demographics
mkdir Histograms
cd Histograms
mkdir LinearRegressions
mkdir histograms
cd ..
cd ..
#R2s
mkdir R2s
cd R2s
mkdir Baseline
mkdir Heatmaps
mkdir LinearRegressions
cd LinearRegressions
mkdir R2S
mkdir R2S_var
cd ..
mkdir Scatterplots
cd Scatterplots
mkdir Baseline
mkdir Summary
cd ..
mkdir Volcano_Age
cd Volcano_Age
mkdir R2S
mkdir R2S_var
cd ..
mkdir Volcano_Demographics
mkdir Histograms
cd Histograms
mkdir LinearRegressions
mkdir histograms
cd ..
cd ..
#Correlations
mkdir Correlations
cd Correlations
mkdir Baseline
mkdir Heatmaps
mkdir LinearRegressions
cd LinearRegressions
mkdir Correlations
mkdir Correlations_var
cd ..
mkdir Scatterplots
cd Scatterplots
mkdir Baseline
mkdir Summary
cd ..
mkdir Volcano_Age
cd Volcano_Age
mkdir Correlations
mkdir Correlations_var
cd ..
mkdir Volcano_Demographics
mkdir Histograms
cd Histograms
mkdir LinearRegressions
mkdir histograms
cd ..
cd ..




#BIOMARKERS
#Histograms and Hierarchical clustering:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Preprocessing/Plots/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Biomarkers/
  
#COEFFICIENTS
#baseline:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Interactive/Heatmaps/Coefficients/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Baseline/
#flattened pdf:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Plots/Flat/Coefficients_normed/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Heatmaps/
#interactive volcano plots coefficients: 
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Interactive/Significance/Volcano/Age_differences/Coefficients/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Volcano_Age/Coefficients/
#interactive volcano plots coefficients: 
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Interactive/Significance/Volcano/Age_differences/Coefficients_var/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Volcano_Age/Coefficients_var/
#Trajectories plots coefficients:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Plots/Significance/LinearRegressions/Coefficients/all/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/LinearRegressions/Coefficients/
#Trajectories plots coefficients:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Plots/Significance/LinearRegressions/Coefficients_var/all/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/LinearRegressions/Coefficients_var/
#Scatterplots:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Interactive/Scatterplots/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Scatterplots/Baseline/
#Scatterplots summaries:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Interactive/Comparisons/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Scatterplots/Summary/
#Histograms linear regressions
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Plots/Histograms/LinearRegressions/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Histograms/LinearRegressions/   
#Histograms video
scp -r /n/groups/patel/Alan/Aging/Square/Results/Coefficients/Videos/Histograms/histograms/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Coefficients/Histograms/histograms/   
  
  
#R2s
#baseline:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Heatmaps/by_demo_groups/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Baseline
#interactive plot (by demo group):
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Heatmaps/by_demo_groups/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Heatmaps/
#Volcano interactive demographic differences:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Significance/Volcano/Demographics_differences/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Volcano_Demographics/
#Volcano interactive age differences R2S:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Significance/Volcano/Age_differences/R2S/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Volcano_Age/R2S/
#Volcano interactive age differences R2S_var:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Significance/Volcano/Age_differences/R2S_var/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Volcano_Age/R2S_var/
#Trajectories plots R2S
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Plots/Significance/LinearRegressions/R2S/all/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/LinearRegressions/R2S/
#Trajectories plots R2S_var
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Plots/Significance/LinearRegressions/R2S_var/all/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/LinearRegressions/R2S_var/
#Scatterplots:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Scatterplots/test/12/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Scatterplots/Baseline/
#Scatterplots summaries:
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Interactive/Scatterplots/Summary/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Scatterplots/Summary/
#Histograms linear regressions
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Plots/Histograms/LinearRegressions/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Histograms/LinearRegressions/   
#Histograms video
scp -r /n/groups/patel/Alan/Aging/Square/Results/R2s/Videos/Histograms/histograms/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/R2s/Histograms/histograms/   

  
#CORRELATIONS (BIOMARKERS)
#baseline:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Heatmaps/biomarkers/Correlations/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Baseline
#flattened pdf: 
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Plots/Flat/biomarkers/Correlations_normed/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Heatmaps/
#Interactive Volcano plots: Demographics significance
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Significance/Volcano/Demographics_differences/biomarkers/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Volcano_Demographics/
#Interactive Volcano plots: Age significance correlations
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Significance/Volcano/Age_differences/Correlations/biomarkers/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Volcano_Age/Correlations/
#Interactive Volcano plots: Age significance correlations_var
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Significance/Volcano/Age_differences/Correlations_var/biomarkers/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Volcano_Age/Correlations_var/
#Trajectories plots correlations:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Plots/Significance/LinearRegressions/Correlations/biomarkers/all/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/LinearRegressions/Correlations/
#Trajectories plots correlations_var:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Plots/Significance/LinearRegressions/Correlations_var/biomarkers/all/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/LinearRegressions/Correlations_var/
#Scatterplots:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Scatterplots/biomarkers/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Scatterplots/Baseline/
#Scatterplots summaries:
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Interactive/Comparisons/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Scatterplots/Summary/   
#Histograms linear regressions
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Plots/Histograms/LinearRegressions/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Histograms/LinearRegressions/   
#Histograms video
scp -r /n/groups/patel/Alan/Aging/Square/Results/Correlations/Videos/Histograms/histograms/* /n/groups/patel/Alan/Aging/Square/Results/Square_O2_results/Correlations/Histograms/histograms/   


#compress
cd /n/groups/patel/Alan/Aging/Square/Results/
tar -czvf Square_O2_results.tar.gz Square_O2_results


