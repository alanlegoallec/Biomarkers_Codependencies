#Square project postprocessing, sliding window, Coefficients

# to run the script use command line: 
# Rscript ./Square/Scripts/Square_preprocessing_hc.R machine n_cores demo_group
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

#cluster and display labels
data <- redRDS("Square/Data/Preprocessing/data_square", demo_group)
for (biomarker in biomarkers)
{
  data_b <- data
  names(data_b)[which(names(data_b)==biomarker)] <- "y"
  if(demo_group=="all")
  {
    model <- lm(y~age_factor+female+hispanic+black+other, data_b, weights=data$weights)
  } else if (demo_group %in% c("male", "female")) {
    model <- lm(y~age_factor+hispanic+black+other, data_b, weights=data$weights)
  } else {
    model <- lm(y~age_factor+female, data_b, weights=data$weights)
  }
  data[,biomarker] <- model$residuals
}
data <- data[, biomarkers]
plot_hc(data, demo_group, r_boot_hc)

