#Square project postprocessing, sliding window, Coefficients

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_postpostprocessing_Coefficients.R machine n_cores demo_group
# machine must be either O2 or local, n_cores > 0

#extract and use info from command line
args = commandArgs(trailingOnly=TRUE)

#default, if no command line received
#if(length(args) == 0){args <- c("local", "1","all")}
if(length(args) == 0){args <- c("O2", "1","all")}


if (length(args) != 3)
{
  stop("usage: Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group   use 4 arguments")
} else {
  #pick parameters for model to run
  machine <- args[1]
  n_cores <- as.numeric(args[2])
  demo_group <- args[3]
  ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
  #load helpers, libraries, parameters
  source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
  source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))
}

print(paste("Demographic group = ", demo_group, ".", sep = ""))


#COEFFICIENTS
# #plot
# print("Plotting coefficients.")
# plot_coefficients_sw(demo_group)
print("Flattening videos.")
flatten_videos_coefficients(demo_group)
# print("Plotting flattened videos.")
# plot_flat_coefficients_heatmaps_sw(demo_group)
# print("Testing demographics significances.")
# coefficients_significances(demo_group)
print("Testing age significance coefficients.")
test_significance_changes_coefficients_sw(demo_group)
print("Testing age significance coefficients var.")
test_significance_changes_coefficients_variances_sw(demo_group)
print("Generating sliding histogram.")
sliding_histogram_coefficients(demo_group)
# print("Plotting gifs.")
# generate_gifs_coefficients(demo_group)
print("Job finished.")

