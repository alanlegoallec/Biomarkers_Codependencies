#Square project postprocessing, sliding window, R2s

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_postpostprocessing_correlations.R machine n_cores half side
# machine must be either O2 or local, n_cores > 0

#extract and use info from command line
args = commandArgs(trailingOnly=TRUE)

#default, if no command line received
#if(length(args) == 0){args <- c("local", "1","test", "12")}
if(length(args) == 0){args <- c("O2", "1","test", "12")}


if (length(args) != 4)
{
  stop("usage: Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores half side    use 4 arguments")
} else {
  #pick parameters for model to run
  machine <- args[1]
  n_cores <- as.numeric(args[2])
  half <- args[3]
  side <- args[4]
  ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
  #load helpers, libraries, parameters
  source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
  source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))
}

print(paste("Half = ", half, ", Side = ", side, ".", sep = ""))


#R2S
print("Done loading libraries.")
print("Generating normed R2")
R2s_generate_normed_squares(half, side)
print("Compiling R2s by demo_groups.")
R2s_compile_by_demo_groups(half, side)
print("Compiling R2s by age_ranges.")
R2s_compile_by_age_ranges(half, side)
# print("Calculating and plotting R2s significances.")
# R2s_significances(half, side)
# #plots
# print("Plotting R2s by demo_groups.")
# R2s_plot_by_demo_groups(half, side) 
# print("Plotting R2s by age_ranges.")
# R2s_plot_by_age_ranges(half, side)
print("Plotting R2s correlations.")
R2s_plot_correlations(half, side)
# #test significance of changes between correlations
print("Testing significance of age related changes in R2s.")
test_significance_changes_R2s_sw(half, side)
print("Testing significance of age related changes in R2s variances.")
test_significance_changes_R2s_variances_sw(half, side)
print("Generating sliding histogram.")
sliding_histogram_R2s(half, side)
# #generate gifs
# print("Generating R2s gifs.")
# generate_gifs_R2s(half, side)
print("Job finished.")

