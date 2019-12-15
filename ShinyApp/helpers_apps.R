#Helpers for apps

set.seed(0)

#libraries
# library(rsconnect)
# options(rsconnect.max.bundle.size=3145728000)
# options(rsconnect.max.bundle.files=200000)
# options(warn=1)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyLP)
library(DT)
Sys.setenv(RGL_USE_NULL = TRUE)
library(rgl)
library(ggplot2)
library(plotly)
library(metafor)
library(shinyWidgets)
library(reshape2)
library(pvclust)
library(caTools)
library(plotrix)


folder = "Preprocessing"
#figures parameters
generate_figures = TRUE
size_dots = 5
size_lines = 1
size_bars = 1
size_titles = 15
size_axis = 30
size_ticks = 20
size_titles_flatheatmaps <- 20
size_axis_flatheatmaps <- 10
size_ticks_flatheatmaps_x <- 5
size_ticks_flatheatmaps_y <- 6

if(generate_figures)
{
  size_axis = 30
  size_labels_heatmaps = 8
  
} else {
  size_axis = 25
  size_labels_heatmaps = 5
}

#loading functions
file_name <- function(name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  args = c(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4)
  args <- args[which(args != "")]
  file_name <- args[1]
  for (arg in args[-1])
  {
    file_name <- paste(file_name, arg, sep = "_")
  }
  return(file_name)
}

redRDS <- function(folder = "", name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  return(readRDS(paste("data/max_data/", folder, "/", file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".Rda", sep = "")))
}

redRDS_equalbins <- function(folder = "", name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  return(readRDS(paste("data/equal_bins/", folder, "/", file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".Rda", sep = "")))
}

savtiff <- function(p, name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  tiff(paste("figures/", file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".tiff", sep = ""))
  p
  dev.off()
}

#initiate storing matrices
initiate_store <- function(rows, columns)
{
  data <- data.frame(matrix(0, length(rows), length(columns)))
  rownames(data) <- rows
  names(data) <- columns
  return(data)
}

#function to convert the matrix of coefficients into a vector
concatenate_coefficients <- function(coefs)
{
  concatenated_coefficients <- c()
  for (i in seq(length(biomarkers_hc)))
  {
    concatenated_coefficients <- c(concatenated_coefficients, coefs[-i,i])
  }
  return(concatenated_coefficients)
}

jumbotron_size <- function(header , content, size_header=1, buttonID, button = TRUE, ...){
  if(!(size_header%%1==0)){print("size is the html size of the headar and must be an integer. 1 is the tallest.")}
  button_label = c(...)
  if (button){
    HTML(paste("<div class='jumbotron'>
                <h", size_header, "> ", header, " </h", size_header, ">
                <p> ", content ," </p> ",
                "<p><a class='btn btn-primary btn-lg' button id=", buttonID,"> ", button_label, " </a></p>
                </div>", sep = "") )
  } else {
    HTML(paste("<div class='jumbotron'>
                <h", size_header, "> ", header, " </h", size_header, ">
                <p> ", content ," </p> ",
                "</div>", sep = ""))
  }
}


#variables
#decompress data if not already done (makes it easier to upload on the server)
if(!dir.exists("data"))
{
  untar('data.tar.gz')
  file.remove('data.tar.gz')
  message("done")
}
#define
n_lambdas <- 100
n_folds_CV <- 10
R_boot <- 100
R_boot_coefficients <- 100
R_boot_R2 = 100
R_boot_correlations <- 100
r_boot_hc <- 1000
bins_Coefficients <- "maxdata"
#bins_Coefficients <- "equalbins"
#bins_R2s <- "maxdata"
bins_R2s <- "equalbins"
bins_Correlations <- "maxdata"
#bins_Correlations <- "equalbins"
sex_demographics <- c("male", "female", "controlS1", "controlS2")
ethnicity_demographics <- c("white", "hispanic", "black", "controlE1", "controlE2")
sex_comps <- c("MvsF", "CS1vsCS2")
ethnicity_comps <- c("WvsH", "WvsB", "CE1vsCE2")
demo_groupS <- list(c("all"), sex_demographics, ethnicity_demographics)
compS <- list(c(), sex_comps, ethnicity_comps)
demo_and_compS <- list(c("all"), c(sex_demographics, sex_comps), c(ethnicity_demographics, ethnicity_comps))
demographic_categories <- c("general", "sexes", "ethnicities")
demographics_correctors <- c("female", "hispanic", "black")
demographics_correctors_plus <- c(demographics_correctors, "other")
distances <- c("biomarkers", "coefficients")
distances_comps <- c("bmVScoef")
groups_comps <- c("YvsO", "MvsF", "CS1vsCS2", "WvsH", "WvsB", "CE1vsCE2")
demo_groups_sw <- c("all", "male", "female", "controlS1", "controlS2", "white", "hispanic", "black", "controlE1", "controlE2")
groups_comps_sw <- c("MvsF", "CS1vsCS2", "WvsH", "WvsB", "CE1vsCE2")
demo_groups_swS <- c(demo_groups_sw, groups_comps_sw)
distancesS <- c("biomarkers", "coefficients", "bmVScoef")
demo_groups_extra <- c("all", "young", "old", "male", "female", "controlS1", "controlS2", "white", "hispanic", "black", "controlE1", "controlE2")
trajectories_coefficients <- c("increase_-_-", "increase_-_+", "increase_+_+", "decrease_+_+", "decrease_+_-", "decrease_-_-")
trajectories_correlations <- c("increase_-_-", "increase_-_+", "increase_+_+", "decrease_+_+", "decrease_+_-", "decrease_-_-")
trajectories_R2s <- c("increase", "decrease")
R2s_types <- c("sm", "sw") #sm stands for Single Model (train a single model on all the samples, then predict on each age bin), sw stands for sliding window (train and test the model on the age bin)
#window sizes
window_sizes <- initiate_store(demo_groups_sw, c("Demographics", "Window"))
window_sizes$Demographics <- demo_groups_sw
window_sizes["all","Window"] <- 1
window_sizes[sex_demographics,"Window"] <- 2
window_sizes[ethnicity_demographics,"Window"] <- 5
#load
biomarkers <- redRDS(folder, "biomarkers")
labels <- redRDS(folder, "labels")
n_predictors <- length(biomarkers) + length(demographics_correctors) - 1
n_samples_sw <- redRDS(folder, "n_samples_sw")
n_samples_equalbins_full <- redRDS(folder, "n_samples_equalbins")
#load labels files
biomarkers_hc <- redRDS(folder, "biomarkers_hc", "all")
labels_hc <- redRDS(folder, "labels_hc", "all")
Dictionnary_names_labels <- redRDS(folder, "Dictionnary_names_labels")
square_names_hc <- redRDS(folder, "square_names_hc")
square_labels_hc <- redRDS(folder, "square_labels_hc")
melted_names_hc <- redRDS(folder, "melted_names_hc")
melted_labels_hc <- redRDS(folder, "melted_labels_hc")
concatenated_names_target_hc <- redRDS(folder, "concatenated_names_target_hc")
concatenated_labels_target_hc <- redRDS(folder, "concatenated_labels_target_hc")
concatenated_names_predictor_hc <- redRDS(folder, "concatenated_names_predictor_hc")
concatenated_labels_predictor_hc <- redRDS(folder, "concatenated_labels_predictor_hc")
#load sliding window parameters
Age_ranges <- redRDS(folder, "Age_ranges")
Age_means <- redRDS(folder, "Age_means")
age_ranges <- Age_ranges[["all"]]
age_means <- Age_means[["all"]]
Age_ranges_maxdata <- redRDS(folder, "Age_ranges_maxdata")
Age_means_maxdata <- redRDS(folder, "Age_means_maxdata")
Age_ranges_equalbins <- redRDS(folder, "Age_ranges_equalbins")
Age_means_equalbins <- redRDS(folder, "Age_means_equalbins")
Age_ranges_Coefficients <- get(paste("Age_ranges", bins_Coefficients, sep = "_"))
Age_means_Coefficients <- get(paste("Age_means", bins_Coefficients, sep = "_"))
Age_ranges_R2s <- get(paste("Age_ranges", bins_R2s, sep = "_"))
Age_means_R2s <- get(paste("Age_means", bins_R2s, sep = "_"))
Age_ranges_Correlations <- get(paste("Age_ranges", bins_Correlations, sep = "_"))
Age_means_Correlations <- get(paste("Age_means", bins_Correlations, sep = "_"))
Age_ranges_Coefficients_equalbins <- get(paste("Age_ranges", "equalbins", sep = "_"))
Age_means_Coefficients_equalbins <- get(paste("Age_means", "equalbins", sep = "_"))
Age_ranges_R2s_equalbins <- get(paste("Age_ranges", "equalbins", sep = "_"))
Age_means_R2s_equalbins <- get(paste("Age_means", "equalbins", sep = "_"))
Age_ranges_Correlations_equalbins <- get(paste("Age_ranges", "equalbins", sep = "_"))
Age_means_Correlations_equalbins <- get(paste("Age_means", "equalbins", sep = "_"))
Dictionnary_groups_comps <- redRDS(folder, "Dictionnary_groups_comps")
Dictionnary_distances_comps <- redRDS(folder, "Dictionnary_distances_comps")
Dictionnary_groups_comps_sw <- redRDS(folder, "Dictionnary_groups_comps_sw")
list_labels <- as.list(Dictionnary_names_labels$label)
list_demo_groups <- as.list(demo_groups_extra)
list_demo_groupsS <- as.list(demo_groups_swS)
list_distancesS <- as.list(distancesS)
buttons_distancesS <- list("Pearson Correlation", "Pearson Correlation between the linear regression coefficients", "Differences between the two distances")
buttons_demographicsS <- list("All", "Males", "Females", "Sex Demographics - Control group 1", "Sex Demographics - Control group 2", "Whites", "Hispanics", "Blacks", "Ethnicity Demographics - Control group 1", "Ethnicity Demographics - Control group 2", "Males VS Females", "Sex Demographics - Control Comparison", "Whites VS Hispanics", "Whites VS Blacks", "Ethnicity Demographics - Control Comparison")
buttons_demographics <- list("All", "Males", "Females", "Sex Demographics - Control group 1", "Sex Demographics - Control group 2", "Whites", "Hispanics", "Blacks", "Ethnicity Demographics - Control group 1", "Ethnicity Demographics - Control group 2")
buttons_demographics_and_ages <- list("All", "Young", "Old", "Males", "Females", "Sex Demographics - Control group 1", "Sex Demographics - Control group 2", "Whites", "Hispanics", "Blacks", "Ethnicity Demographics - Control group 1", "Ethnicity Demographics - Control group 2")
buttons_demographics_other <- list("All", "Males", "Females", "Sex Demographics - Control group 1", "Sex Demographics - Control group 2", "Whites", "Hispanics", "Blacks", "Others", "Ethnicity Demographics - Control group 1", "Ethnicity Demographics - Control group 2")
buttons_groups_comps <- list("Males VS Females", "Sex Demographics - Control Comparison", "Whites VS Hispanics", "Whites VS Blacks", "Ethnicity Demographics - Control Comparison")
sides <-  c("1", "2")
sidesS <- c("1", "2", "12")
biomarkers_labels <- redRDS("Preprocessing", "biomarkers_labels", "all")

Dictionnary_categories <- data.frame(cbind(demo_groups_swS, c("all", rep("sexes", 4), rep("ethnicities", 5), rep("sexes", 2), rep("ethnicities", 3))), stringsAsFactors = FALSE)
names(Dictionnary_categories) <- c("group", "category")
rownames(Dictionnary_categories) <- Dictionnary_categories$group

Dictionnary_buttons_demographicsS <- data.frame(cbind(demo_groups_swS, c("All", "Males", "Females", "Sex Demographics - Control group 1", "Sex Demographics - Control group 2", "Whites", "Hispanics", "Blacks", "Ethnicity Demographics - Control group 1", "Ethnicity Demographics - Control group 2", "Males VS Females", "Sex Demographics - Control Comparison", "Whites VS Hispanics", "Whites VS Blacks", "Ethnicity Demographics - Control Comparison")), stringsAsFactors = FALSE)
names(Dictionnary_buttons_demographicsS) <- c("group", "button")
rownames(Dictionnary_buttons_demographicsS) <- Dictionnary_buttons_demographicsS$group

Dictionnary_buttons_groups_comps <- data.frame(cbind(groups_comps_sw, c("Males VS Females Comparison", "Sex Demographics - Control Comparison", "Whites VS Hispanic Comparison", "Whites VS Blacks Comparison", "Ethnicity Demographics - Control Comparison")), stringsAsFactors = FALSE)
names(Dictionnary_buttons_groups_comps) <- c("group", "button")
rownames(Dictionnary_buttons_groups_comps) <- Dictionnary_buttons_groups_comps$group

Dictionnary_buttons_demographics_preprocessing <- data.frame(cbind(c("all", "male", "female", "white", "hispanic", "black", "other"), c("All", "Males", "Females", "Whites", "Hispanics", "Blacks", "Others")), stringsAsFactors = FALSE)
names(Dictionnary_buttons_demographics_preprocessing) <- c("group", "button")
rownames(Dictionnary_buttons_demographics_preprocessing) <- Dictionnary_buttons_demographics_preprocessing$group

Dictionnary_buttons_distancesS <- data.frame(cbind(distancesS, c("Pearson Correlation", "Pearson Correlation between the linear regression coefficients", "Differences between the two distances")), stringsAsFactors = FALSE)
names(Dictionnary_buttons_distancesS) <- c("group", "button")
rownames(Dictionnary_buttons_distancesS) <- Dictionnary_buttons_distancesS$group

data_normed <- redRDS("Preprocessing", "data_square_all")
data_raw <- readRDS("./data/max_data/Raw/bigData.rds")
data_weights <- readRDS("./data/max_data/Preprocessing/data_weights.Rda")
data_raw <- data_raw[!duplicated(data_raw$SEQN),]
data_raw <- merge(data_raw, data_weights, by = "SEQN")
names(data_raw)[which(names(data_raw) == "RIDAGEYR")] <- "age_factor"
index_1vs2_sexes <- sample.split(data_raw$age_factor, SplitRatio=0.5)
index_1vs2_sexes <- data_raw$SEQN[index_1vs2_sexes]
index_1vs2_ethnicities <- sample.split(data_raw$age_factor, SplitRatio=0.5)
index_1vs2_ethnicities <- data_raw$SEQN[index_1vs2_ethnicities]
dg_names <- c("male", "female", "white", "hispanic", "black", "other")
dg_labels <- c("Males", "Females", "Whites", "Hispanics", "Blacks", "Others")
par(cex=0.7)
max_plots <- 10 #length(demo_groups_sw)

max_plots_traj <- 15 #(2 sexes + all) * (4 ethnicities + all) = 3*5 = 15



scatterplot_correlations_sw <- function(square_1, square_2, name_1, name_2, distance, demo_group, age_range, type)
{
  group <- paste(distance, demo_group, age_range, sep = ", ")
  #prepare data
  for (side in sides)
  {
    sq <- get(file_name("square", side))
    x <- sq[upper.tri(sq)]
    assign(file_name("cor", side), x)
  }
  cor_12 <- cor(cor_1, cor_2)
  text <- square_labels_hc[upper.tri(square_labels_hc)]
  n = length(text)
  data <- data.frame("cor_1" = vector(mode="numeric", length=n), "cor_2" = vector(mode="numeric", length=n), "text" = vector(mode="character", length=n))
  rownames(data) <- square_names_hc[upper.tri(square_names_hc)]
  data$cor_1 <- cor_1
  data$cor_2 <- cor_2
  if(!grepl("var", type))
  {
    data$cor_1 <- round(data$cor_1,3)
    data$cor_2 <- round(data$cor_2,3)
  }
  data$text <- text
  names(data) <- c("cor_1", "cor_2", "text")
  ifelse(grepl("var", type), lims_xy <- list(c(0, max(cor_1)), c(0, max(cor_2))), lims_xy <- list(c(-1,1), c(-1,1)))
  p <- ggplot(data, aes(cor_1, cor_2, text = text)) + geom_point(alpha=.5) + geom_abline() + coord_cartesian(xlim = lims_xy[[1]], ylim = lims_xy[[2]]) + ggtitle(paste(type, " comparison, distance = ", distance, ", demographics = ", demo_group, ", r = ", round(cor_12,3), sep = "")) + xlab(name_1) + ylab(name_2) + theme(plot.title = element_text(size=8))
  p <- ggplotly(p)
  return(p)
}


