#Square project postprocessing, sliding window, Correlations

args <- c("O2", 1)
machine <- "O2"
n_cores <- 1

#load helpers, libraries, parameters
ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))

#CORRELATIONS
#generate comparisons
print("Done loading libraries. Generating correlations. Generating correlations: groups comparisons.")
generate_correlations_groups_comparisons_sw()
print("Generating correlations: distances comparisons.")
generate_correlations_distances_comparisons_sw()
print("Done. Job finished.")





