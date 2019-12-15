#Square project postprocessing, sliding window, Coefficients

args <- c("O2", 1)
machine <- "O2"
n_cores <- 1

#load helpers, libraries, parameters
ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))

#COEFFICIENTS. post-processing
print("Generating coefficients.")
generate_coefficients_sw()
print("Plotting scatterplots.")
scatterplot_coefficients_sw()

