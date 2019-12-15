#Square project processing, sliding window
#generate squares, coefficients, and R2.

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group age_window
# machine must be either O2 or local, n_cores > 0, demo_group in 1-5 (demo_groups), age_window# in 1-52


#extract and use info from command line
args = commandArgs(trailingOnly=TRUE)

#default, if no command line received
if(length(args) == 0){args <- c("local", "1","all", "1")}

if (length(args) != 4)
{
  stop("usage: Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group age_window#    use 4 arguments")
} else {
  #pick parameters for model to run
  machine <- args[1]
  n_cores <- as.numeric(args[2])
  demo_group <- args[3]
  ifelse(machine == "O2", path <- "/home/al311/Aging/", path <- "/Users/Alan/Desktop/Aging/")
  #load helpers, libraries, parameters
  source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
  source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))
  age_range <- Dictionnary_sw[as.numeric(args[4]), "age_ranges"]
}

#to serialize, mostly for debugging purposes (uncomment the }}} at the end too.)
#for (demo_group in demo_groups_sw){for (i in seq(length(age_mins))){

print(paste("Demographic group = ", demo_group, ", Age range = ", age_range, ", generating coefficients and correlation squares.", sep = ""))

#COEFFICIENTS AND CORRELATION SQUARES
generate_coefficients_and_correlations_sw(demo_group, age_range)
print("Generated coefficients and correlation matrices, generating predictions for R2.")

#R2S
#generate predictions for R2 calculations purposes
generate_predictions_sw(demo_group, age_range)
print("Generated predictions. Calculating R2s.")
#generate R2s
generate_R2_sw(demo_group, age_range)
print("R2s calculated. Job completed.")

#}}


  