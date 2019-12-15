#Square project postpostprocessing correlations, sliding window

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_postpostprocessing_correlations.R machine n_cores distance demo_group
# machine must be either O2 or local, n_cores > 0

#extract and use info from command line
args = commandArgs(trailingOnly=TRUE)

#default, if no command line received
if(length(args) == 0){args <- c("O2", "1","biomarkers", "all")}

if (length(args) != 4)
{
  stop("usage: Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group age_window#    use 4 arguments")
} else {
  #pick parameters for model to run
  machine <- args[1]
  n_cores <- as.numeric(args[2])
  distance <- args[3]
  demo_group <- args[4]
  ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
  #load helpers, libraries, parameters
  source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
  source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))
}

print(paste("Distance = ", distance, ", Demographic group = ", demo_group, ".", sep = ""))

#plot
#print("Plotting correlations.")
#generate_correlations_plots_sw(distance, demo_group)
#flatten video
print("Flattening correlations videos.")
flatten_videos_correlations(distance, demo_group)
# #plot flatten correlations
# print("Plotting flattened correlations.")
# plot_flat_correlations_heatmaps_sw(distance, demo_group)
# print("Calculating and plotting significances correlations.")
# Correlations_significances(distance, demo_group)
# #test significance of changes between correlations
print("Testing significance of age related changes in correlations.")
test_significance_changes_correlations_sw(distance, demo_group)
print("Testing significance of age related changes in correlations_var.")
test_significance_changes_correlations_variances_sw(distance, demo_group)
print("Generating sliding histogram.")
sliding_histogram_correlations(distance, demo_group)
# print("Plotting gifs.")
# generate_gifs_correlations(distance, demo_group)
print("Job finished.")

