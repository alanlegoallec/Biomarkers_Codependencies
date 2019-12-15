#Square project processing, sliding window
#generate squares, coefficients, and R2.

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group age_window
# machine must be either O2 or local, n_cores > 0, demo_group in 1-5 (demo_groups), age_window# in 1-52


#extract and use info from command line
args = commandArgs(trailingOnly=TRUE)

#default, if no command line received
#if(length(args) == 0){args <- c("local", "1","all", "1")}
if(length(args) == 0){args <- c("O2", "1","all", "1")}

if (length(args) != 4)
{
  stop("usage: Rscript ./Square/Scripts/Square_processing_sw.R machine n_cores demo_group age_window#    use 4 arguments")
} else {
  #pick parameters for model to run
  machine <- args[1]
  n_cores <- as.numeric(args[2])
  demo_group <- args[3]
  n <- as.numeric(args[4])
  ifelse(machine == "O2", path <- "/n/groups/patel/Alan/Aging/", path <- "/Users/Alan/Desktop/Aging/")
  #load helpers, libraries, parameters
  source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
  source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))
  age_range <- Age_ranges_R2s[[demo_group]][n]
}

#to serialize, mostly for debugging purposes (uncomment the }}} at the end too.)
#for (demo_group in demo_groups_sw){for (i in seq(length(age_mins))){

print(paste("Demographic group = ", demo_group, ", Age range = ", age_range, ", generating R2s.", sep = ""))

#R2S
#generate predictions for R2 calculations purposes
generate_predictions_sw(demo_group, age_range)
print("Generated predictions. Calculating R2s.")
#generate R2s
generate_R2_sw(demo_group, age_range)
print("R2s calculated. Job completed.")


#}}


  