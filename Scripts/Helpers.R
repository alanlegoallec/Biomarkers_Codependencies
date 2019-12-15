#libraries
#parameters, seeds
#helper functions

#run on local or O2 mode

if (!exists("args"))
{
  args = commandArgs(trailingOnly=TRUE)
}
ifelse(length(args) > 0, machine <- args[1], machine <- "local")
ifelse(length(args) > 0, n_cores <- as.numeric(args[2]), n_cores <- 1)

if (machine == "O2")
{
  path <- "/n/groups/patel/Alan/Aging/"
  options(bitmapType='cairo')
  #Set parameters
  n_folds_CV = 10
  n_lambdas = 30
  R_boot = 1000
  R_significance = 1000
  parallel_cv = FALSE #use TRUE/FALSE
  ifelse(n_cores > 1, parallel_boot <- "multicore", parallel_boot <- "no")
} else {
  path <- "/Users/Alan/Desktop/Aging/"
  #Set parameters
  n_folds_CV = 3
  n_lambdas = 30
  R_boot = 3
  R_significance = 3
  parallel_cv = FALSE #use TRUE/FALSE
  ifelse(n_cores > 1, parallel_boot <- "multicore", parallel_boot <- "no")
}


set.seed(0)
library(broom)
library(magrittr)
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(purrr)
library(reshape2)
library(rvest)
library(knitr)
library(mi)
library(survey)
library(glmnet)
library(stringr)
library(Amelia)
library(survival)
library(GGally)
library(caret)
library(dummies)
library(ggplot2)
library(caTools)
library(miscTools)
library(survRM2)
library(survminer)
library(compareC)
library(survcomp)
library(boot)
library(plotly)
library(scatterplot3d)
library(gdata)
library(webshot)
library(wCorr)
library(corpcor)
library(metafor)
library(pvclust)
library(doMC)
#library(doParallel)
registerDoMC(cores=n_cores)
print(paste("number of cores detected and used = ", n_cores))
analyzes <-  c("age", "surv")
analyzesS <- c("age", "surv", "truncated_surv")
#disease based survival
analyzes_D <-  c("cancer", "cardiac")
analyzesS_D <- c("truncated_cancer", "truncated_cardiac")
sides <-  c("1", "2")
sidesS <- c("1", "2", "12")

halves = c("train", "test")
age_groups <- c("all", "young", "old", "mixed1", "mixed2")
age_groups_extra <- c("all", "young", "old", "male", "female", "white", "hispanic", "black", "other", "mixed1", "mixed2")
demographics_groups <- c("neutral", "male", "female", "white", "hispanic", "black", "other")
sexes <- c("male", "female")
ethnicities <- c("white", "hispanic", "black", "other")
predictors_list <- c("baseline", "sex", "ethnicity", "sex_ethnicity")
predictors_list_surv <- c("control", "baseline", "sex", "ethnicity", "sex_ethnicity")
comparison_groups <- c("youngVSold", "mixed1VSmixed2")
comparisons <- c("full", "union", "intersection", "comparison")
comparisons_bioages <- c("BiomarkersVsTelomeres", "BiomarkersVsVO2m", "TelomeresVsVO2m")
comparison_categories <- c("both", "1", "2", "none")
significance_levels <- c("significant", "non-significant")
ages <- c("CA", "BA_age", "BA_surv")
demographic_correctors <- c("female", "hispanic", "black", "other")

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

savRDS <- function(object, name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  file = paste(path, file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".Rda", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  saveRDS(object, file)
}

redRDS <- function(name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  return(readRDS(paste(path, file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".Rda", sep = "")))
}

ggsav <- function(name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "", other_arg3 = "", other_arg4 = "")
{
  file = paste(path, file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2, other_arg3, other_arg4), ".png", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  ggsave(file)
}

ggsavpdf <- function(height = 100, name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "")
{
  file = paste(path, file_name(name, analysis, age_group, demographic_group, half, side), ".pdf", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  ggsave(file, height = height, units = "cm")
}

htmlsav <- function(p, name = "", analysis = "", age_group = "", demographic_group = "", half = "", side = "", other_arg1 = "", other_arg2 = "")
{
  file = paste(path, file_name(name, analysis, age_group, demographic_group, half, side, other_arg1, other_arg2), ".html", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  htmlwidgets::saveWidget(p, file)
}

plot_sav <- function(p, name)
{
  file = paste(path, name, ".jpg", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  jpeg(file, width = 750, height = 750)
  plot(p)
  dev.off()
}

plot_hc <- function(data, demo_group, r_boot_hc=1000)
{
  file = paste(path, file_name("Square/Results/Preprocessing/Plots/Hierchical_clustering/Hierchical_clustering_labels", demo_group), ".pdf", sep = "")
  dir_path <- dirname(file)
  if (!dir.exists(dir_path)){dir.create(dir_path, recursive = TRUE)}
  names(data) <- labels
  hc <- pvclust(data, method.dist="cor", method.hclust="average", nboot=r_boot_hc)
  savRDS(hc, "Square/Data/Preprocessing/Hierchical_clustering_labels", demo_group)
  par(cex=2.0)
  pdf(file, width = 15, height = 15)
  plot(hc)
  pvrect(hc, alpha=0.95)
  dev.off()
}

#load files
#tabDesc_clean
tabDesc_clean <- redRDS("Preprocessing/tabDesc_clean")
#demographics_var
demographics_var <- redRDS("Preprocessing/demographics_var")
#other_var
other_var <- redRDS("Preprocessing/other_var")
#aging_var
aging_var <- redRDS("Preprocessing/aging_var")
#survival_var
survival_var <- redRDS("Preprocessing/survival_var")
#data_weights
data_weights <- redRDS("Preprocessing/data_weights")
#BA_comp_correctors
BA_comp_correctors <- redRDS("Preprocessing/BA_comp_correctors")
#load biomarkers and labels
biomarkers <- redRDS("Preprocessing/biomarkers")
labels <- redRDS("Preprocessing/labels")
biomarkers_hc <- redRDS("Preprocessing/biomarkers_hc", "all")
labels_hc <- redRDS("Preprocessing/labels_hc", "all")
biomarkers_labels <- redRDS("Preprocessing/biomarkers_labels", "all")


#define aging boostrapping functions
boot_significance_age <- function(data, indices){
  data = data[indices,]
  y = data$RIDAGEYR
  x = as.matrix(data[,-which(names(data) %in% c("SEQN", "RIDAGEYR", "weights"))])
  w = unlist(data$weights)
  model <- cv.glmnet(x = x, y = y, nlambda = n_lambdas, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = parallel_cv, standardize = FALSE, weights = w)
  coefs <- as.vector(coef(model, s= "lambda.min"))
  names(coefs) <- rownames(coef(model, s= "lambda.min"))
  return(coefs)
}

corrected_significance_age <- function(data, R_boot){
  coef_p <- boot(data = data, statistic = boot_significance_age, R = R_boot, parallel = parallel_boot, ncpus=n_cores)
  coefs <- initiate_store(names(coef_p$t0), c("coef", "p", "corrected"))
  coefs$coef <- coef_p$t0
  coefs$p <- pnorm(abs(coef_p$t0)/apply(coef_p$t, 2, sd), lower.tail = FALSE)
  coefs$corrected <- coefs$p*(ncol(data) - 3)
  rownames(coefs) <- c("Intercept", names(data)[-which(names(data) %in% c("RIDAGEYR", "SEQN", "weights"))])
  coefs[is.nan(coefs$p),] <- c(0, 1, length(coef_p$t0) - 1)
  return(coefs)
}

boot_R2 <- function(data, indices, k){
  data = data[indices,]
  y = data$RIDAGEYR
  prediction = data$BA
  w = unlist(data$weights)
  weighted_mean = sum(y*w)/sum(w)
  SSR = sum(w*(y - prediction) ^ 2)
  SST = sum(w * (y - weighted_mean) ^ 2)
  r2 <- (1 - SSR/SST)
  n <- length(y)
  r2 <- 1-((1-r2)*(n-1)/(n-k-1))
  return(r2)
}

#define boostrapping survival functions
boot_significance_surv <- function(data, indices){
  data = data[indices,]
  surv <- Surv(data$PERMTH_EXM, data$MORTSTAT)
  x = as.matrix(data[,-which(names(data) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))])
  w = unlist(data$weights)
  model <- cv.glmnet(x = x, y = surv, nlambda = n_lambdas, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = parallel_cv, standardize = FALSE, weights = w, family="cox")
  coefs <- as.vector(coef(model, s= "lambda.min"))
  names(coefs) <- rownames(coef(model, s= "lambda.min"))
  return(coefs)
}

corrected_significance_surv <- function(data, R_boot){
  coef_p <- boot(data = data, statistic = boot_significance_surv, R = R_boot, parallel = parallel_boot, ncpus=n_cores)
  coefs <- initiate_store(names(coef_p$t0), c("coef", "p", "corrected"))
  coefs$coef <- coef_p$t0
  coefs$p <- pnorm(abs(coef_p$t0)/apply(coef_p$t, 2, sd), lower.tail = FALSE)
  coefs$corrected <- coefs$p*(ncol(data) - 3)
  rownames(coefs) <- names(data)[-which(names(data) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  coefs <- coefs[,which(names(coefs) %in% c("coef", "p", "corrected"))]
  coefs[is.nan(coefs$p),] <- c(0, 1, length(coef_p$t0) - 1)
  return(coefs)
}

boot_CI <- function(data, indices)
{
  data = data[indices, which(names(data) %in% c("BA", "PERMTH_EXM", "MORTSTAT", "weights"))]
  x <- data[, which(names(data) == "BA")]
  c <- concordance.index(x = x, surv.time= data$PERMTH_EXM, surv.event= data$MORTSTAT, weights = data$weights)$c.index
  return(c)
}

#comparison functions
comp_plot <- function(coef1, coef2, name1, name2, name){
  coef1 <- coef1[, which(names(coef1) %in% c("coef", "corrected"))]
  coef2 <- coef2[, which(names(coef2) %in% c("coef", "corrected"))]
  coef = data.frame(cbind(coef1, coef2))
  names(coef) = c("coef1", "corrected1", "coef2", "corrected2")
  rownames(coef) = rownames(coef1)
  coef$color = "none"
  coef$color <- as.factor(coef$color)
  levels(coef$color) <- c("none", name1, name2, "both")
  coef$color <- reorder.factor(coef$color, new.order=c(which(levels(coef$color) == "both"), which(levels(coef$color) == name1), which(levels(coef$color) == name2), which(levels(coef$color) == "none")))
  coef$color[which(coef$corrected1 < 0.05)] <- name1
  coef$color[which(coef$corrected2 < 0.05)] <- name2
  coef$color[which(coef$corrected1 < 0.05 & coef$corrected2 < 0.05)] <- "both"
  for (i in seq(nrow(coef)))
  {
    if(rownames(coef)[i] %in% biomarkers_labels$name){
      rownames(coef)[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(coef)[i])]
    }
  }
  coef$log1 <- -log(coef$corrected1)
  coef$log2 <- -log(coef$corrected2)
  coef$p_group1_group2 = paste(rownames(coef), ", -log(p) = ", name1, ":", round(coef$log1, 1), ", ", name2, ":", round(coef$log2, 1))
  cols = c("both" = "green", name1 = "blue", name2 = "red", "none" = "black")
  names(cols) <- c("both", name1, name2, "none")
  cols <- cols[which(names(cols) %in% names(table(coef$color)[which(table(coef$color) > 0)]))]
  p <- ggplot(coef, aes(coef1, coef2, text = p_group1_group2, color = color))
  p <- p + geom_point(alpha=.5) + geom_abline() + scale_colour_manual(values = cols) + ggtitle(paste("Regression coefficients, ", name, ": ", name1, " vs ", name2)) + xlab(name1) + ylab(name2)
  p
  ggsav(name)
  p <- ggplotly(p)
  htmlsav(p, name)
  return(p)
}

cor_and_comp_model <- function(coef1, coef2)
{
  Comp_coef <- initiate_store(comparisons, c("model"))
  
  index_union <- which(coef1$corrected < 0.05  | coef2$corrected < 0.05)
  index_intersection <- which(coef1$corrected < 0.05  & coef2$corrected < 0.05)
  index_only1 <- which(coef1$corrected < 0.05  & !(coef2$corrected < 0.05))
  index_only2 <- which(!(coef1$corrected < 0.05)  & coef2$corrected < 0.05)
  
  Comp_coef$model[which(rownames(Comp_coef) == "full")] <- cor(coef1$coef, coef2$coef)
  Comp_coef$model[which(rownames(Comp_coef) == "union")] <- cor(coef1$coef[index_union], coef2$coef[index_union])
  Comp_coef$model[which(rownames(Comp_coef) == "intersection")] <- cor(coef1$coef[index_intersection], coef2$coef[index_intersection])
  Comp_coef$model[which(rownames(Comp_coef) == "comparison")] <- paste(length(index_intersection), length(index_only1), length(index_only2), length(index_union), length(coef1$coef), sep = ", ")
  
  return(Comp_coef)
}

cor_and_comp <- function(coef1_neutral, coef2_neutral, coef1_male, coef2_male, coef1_female, coef2_female, coef1_white, coef2_white, coef1_hispanic, coef2_hispanic, coef1_black, coef2_black,  coef1_other, coef2_other)
{
  Comp_coef <- initiate_store(comparisons, demographics_groups)
  
  Comp_coef$neutral <- cor_and_comp_model(coef1_neutral, coef2_neutral)
  Comp_coef$male <- cor_and_comp_model(coef1_male, coef2_male)
  Comp_coef$female <- cor_and_comp_model(coef1_female, coef2_female)
  Comp_coef$white <- cor_and_comp_model(coef1_white, coef2_white)
  Comp_coef$hispanic <- cor_and_comp_model(coef1_hispanic, coef2_hispanic)
  Comp_coef$black <- cor_and_comp_model(coef1_black, coef2_black)
  Comp_coef$other <- cor_and_comp_model(coef1_other, coef2_other)
  
  names(Comp_coef$neutral) <- c("neutral")
  names(Comp_coef$male) <- c("male")
  names(Comp_coef$female) <- c("female")
  names(Comp_coef$white) <- c("white")
  names(Comp_coef$hispanic) <- c("hispanic")
  names(Comp_coef$black) <- c("black")
  names(Comp_coef$other) <- c("other")
  
  return(Comp_coef)
}


## Summary comparison coefficients plots
get_data <- function(data, rowname, colname)
{
  return(data[which(rownames(data) == rowname), which(names(data) == colname)])
}

get_data_unlist <- function(data, rowname, colname)
{
  return(data[[which(rownames(data) == rowname), which(names(data) == colname)]])
}

summary_store_coefficients <- function(coef_all_neutral, coef_all_male, coef_all_female, coef_all_white, coef_all_hispanic, coef_all_black, coef_all_other, 
                                       coef_young_neutral, coef_young_male, coef_young_female, coef_young_white, coef_young_hispanic, coef_young_black, coef_young_other,
                                       coef_old_neutral, coef_old_male, coef_old_female, coef_old_white, coef_old_hispanic, coef_old_black, coef_old_other,
                                       coef_mixed1_neutral, coef_mixed1_male, coef_mixed1_female, coef_mixed1_white, coef_mixed1_hispanic, coef_mixed1_black, coef_mixed1_other,
                                       coef_mixed2_neutral, coef_mixed2_male, coef_mixed2_female, coef_mixed2_white, coef_mixed2_hispanic, coef_mixed2_black, coef_mixed2_other)
{
  Coefficients_summary <- initiate_store_dataframe(demographics_groups, age_groups)
  for (age_group in age_groups)
  {
    for (demographic_group in demographics_groups)
    {
      Coefficients_summary[[which(rownames(Coefficients_summary) == demographic_group), which(names(Coefficients_summary) == age_group)]] <- get(file_name("coef", age_group, demographic_group))
    }
  }
  return(Coefficients_summary)
}


summary_plot_comp_coefficients <- function(Coefficients_summary_1, Coefficients_summary_2, name)
{
  N_age <- length(age_groups)

  data_1vs2 <- data.frame(matrix(0,0,2*ncol(Coefficients_summary_1[[1,1]])+1))
  for (age_group in age_groups)
  {
    data_age <- data.frame(matrix(0,0,2*ncol(Coefficients_summary_1[[1,1]])))
    for (demographic_group in demographics_groups)
    {
      coef1 <- get_data_unlist(Coefficients_summary_1, demographic_group, age_group)
      coef2 <- get_data_unlist(Coefficients_summary_2, demographic_group, age_group)
      #single plot
      comp_plot(coef1, coef2, "1", "2", file_name("Coefficients/Comparisons/Comp", name, age_group, demographic_group))
      data_demo <- cbind(coef1, coef2)
      data_demo$demo <- demographic_group
      data_age <- rbind(data_age, data_demo)
    }
    data_age$age <- age_group
    data_1vs2 <- rbind(data_1vs2, data_age)
  }
  data_1vs2 <- data_1vs2[,c(1,4,2,5,3,6,7,8)]
  names(data_1vs2) <- c("coef1", "coef2", "p1","p2", "nlp1", "nlp2", "demo", "age")
  #color
  data_1vs2$color <- "none"
  data_1vs2$color[which(data_1vs2$nlp1 < 0.05)] <- "1"
  data_1vs2$color[which(data_1vs2$nlp2 < 0.05)] <- "2"
  data_1vs2$color[which(data_1vs2$nlp1 < 0.05 & data_1vs2$nlp2 < 0.05)] <- "both"
  data_1vs2$nlp1 <- -log10(data_1vs2$nlp1)
  data_1vs2$nlp2 <- -log10(data_1vs2$nlp2)
  #format
  data_1vs2$demo <- factor(data_1vs2$demo, levels = as.factor(demographics_groups))
  data_1vs2$age <- factor(data_1vs2$age, levels = as.factor(age_groups))
  data_1vs2$color <- factor(data_1vs2$color, levels = as.factor(comparison_categories))
  #create labels
  data_1vs2$text <- rownames(data_1vs2)
  for (i in seq(nrow(data_1vs2)))
  {
    if(rownames(data_1vs2)[i] %in% biomarkers_labels$name){
      data_1vs2$text[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(data_1vs2)[i])]
    }
  }
  #calculating indexes to leave alone for text
  n_a <- nrow(data_age)
  n_b <- nrow(get_data_unlist(Coefficients_summary_1, "male", "all"))
  n_c <- nrow(get_data_unlist(Coefficients_summary_1, "female", "all"))
  index_a <- 1 + c(2*n_b, 3*n_b+n_c, 3*n_b+2*n_c, 3*n_b+3*n_c)
  index_b <- rowSums(expand.grid(index_a,(0:(N_age-1))*n_a))
  #input text for all samples
  data_1vs2$text[-index_b] <- data_1vs2$text[1:n_b]
  data_1vs2$text[1+2*n_b+(0:(N_age-1))*n_a] <- "female"
  data_1vs2$text[1+3*n_b+n_c+(0:(N_age-1))*n_a] <- "hispanic"
  data_1vs2$text[1+3*n_b+2*n_c+(0:(N_age-1))*n_a] <- "black"
  data_1vs2$text[1+3*n_b+3*n_c+(0:(N_age-1))*n_a] <- "other"
  data_1vs2$text <- paste(data_1vs2$text, ", -lp1=", round(data_1vs2$nlp1, 1), ", -lp2=", round(data_1vs2$nlp2, 1))
  #plot
  cols = c("both" = "green", "1" = "blue", "2" = "red", "none" = "black")
  names(cols) <- c("both", "1", "2", "none")
  cols <- cols[which(names(cols) %in% names(table(data_1vs2$color)[which(table(data_1vs2$color) > 0)]))]
  p <- ggplot(data_1vs2, aes(coef1, coef2, text = text, color = color))  + geom_point() + geom_abline(slope = 1, intercept = 0, color = "black") + scale_color_manual(values=cols) + xlab("Coefficients") + ylab("Coefficients") + theme(axis.text.x= element_text(size = 5), axis.text.y= element_text(size = 5)) + scale_y_continuous(position = "right") + facet_grid(demo~age, switch = "y")
  p
  ggsav("Coefficients/Comparisons/p_summary_comp", name)
  savRDS(data_1vs2, "Coefficients/Comparisons/Summary_coef_comp", name)
  p <- plotly_build(p)
  htmlsav(p, "Coefficients/Comparisons/p_summary_comp", name)
  return(p)
}

#initiate storing matrices
initiate_store <- function(rows, columns)
{
  data <- data.frame(matrix(0, length(rows), length(columns)))
  rownames(data) <- rows
  names(data) <- columns
  return(data)
}

#initiate dataframe of dataframes
initiate_store_dataframe <- function(rows, columns)
{
  data <- data.frame(matrix(list(), length(rows), length(columns)))
  rownames(data) <- rows
  names(data) <- columns
  return(data)
}

#plot coefficients from a model
plot_diff <- function(coef, name){
  coef$p <- coef$p*length(coef$p)
  coef$log_p <- -log(coef$p)/log(10)
  coef$color <- ""
  for (i in seq(nrow(coef)))
  {
    if(rownames(coef)[i] %in% biomarkers_labels$name){
      rownames(coef)[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(coef)[i])]
    }
    p_i = coef$p[i]
    if(p_i < 0.001)
    {
      coef$color[i] <- 'p < 0.001'
    }
    else if(p_i < 0.005)
    {
      coef$color[i] <- 'p < 0.005'    
    }
    else if(p_i < 0.01)
    {
      coef$color[i] <- 'p < 0.01'    
    }
    else if(p_i < 0.05)
    {
      coef$color[i] <- 'p < 0.05'
    }
    else
    {
      coef$color[i] <- 'p > 0.05'
    }
  }
  coef$text <- rownames(coef)
  cols = c("p < 0.001" = "red", "p < 0.005" = "orange", "p < 0.01" = "blue", "p < 0.05" = "green", "p > 0.05" = "black")
  names(cols) <- c("p < 0.001", "p < 0.005", "p < 0.01", "p < 0.05", "p > 0.05")
  cols <- cols[which(names(cols) %in% names(table(coef$color))[which(table(coef$color) > 0)])]
  p <- ggplot(coef, aes(coef, log_p, text = text, color = color))
  p <- p + geom_point(alpha=.5) + scale_colour_manual(values = cols) + geom_hline(aes(yintercept= -log(0.05)/log(10))) + ggtitle(paste("Regression coefficients, ", name)) + xlab('coefficient') + ylab('-log(p-value)')
  p
  ggsav(name)
  p <- ggplotly(p)
  htmlsav(p, name)
  return(p)
}

#display data frame of coefficients in a readable way
rank_coefs <- function(coef)
{
  for (i in seq(nrow(coef)))
  {
    if(rownames(coef)[i] %in% biomarkers_labels$name)
    {
      rownames(coef)[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(coef)[i])]
    }
  }
  coef <- coef[order(coef$coef),]
  coef_significant <- coef[which(coef$corrected < 0.05),]
  coef_insignificant <- coef[-which(coef$corrected < 0.05),]
  coef_significant$significant <- "TRUE"
  coef_insignificant$significant <- "FALSE"
  coef <- rbind(coef_significant, coef_insignificant)
  return(coef)
}


generate_data_age <- function(age_group, side, index_train, index_test, index_1vs2)
{
  analysis = "age"

  #load and format data
  bigData <- redRDS("Preprocessing/bigData")
  #reorganize predictors based on cluster to make the reading of the coefficients easier during post processing
  bigData[,biomarkers] <- bigData[,biomarkers][,biomarkers_hc]
  names(bigData)[which(names(bigData) %in% biomarkers)] <- biomarkers_hc
  
  #get rid of samples depending on the cohort
  if(age_group == "young")
  {
    bigData <- bigData[which(bigData$RIDAGEYR < 50),]
  } else if(age_group == "old")
  {
    bigData <- bigData[-which(bigData$RIDAGEYR < 50),]
  } else if(age_group == "mixed1")
  {
    bigData <- bigData[which(bigData$SEQN %in% index_1vs2),]
  } else if(age_group == "mixed2")
  {
    bigData <- bigData[-which(bigData$SEQN %in% index_1vs2),]
  } else if(age_group == "male")
  {
    bigData <- bigData[which(bigData$RIAGENDR == 1),]
  } else if(age_group == "female")
  {
    bigData <- bigData[which(bigData$RIAGENDR == 2),]
  } else if(age_group == "white")
  {
    bigData <- bigData[which(bigData$RIDRETH1 == 3),]
  } else if(age_group == "hispanic")
  {
    bigData <- bigData[which(bigData$RIDRETH1 %in% c(1,2)),]
  } else if(age_group == "black")
  {
    bigData <- bigData[which(bigData$RIDRETH1 == 4),]
  } else if(age_group == "other")
  {
    bigData <- bigData[which(bigData$RIDRETH1 == 5),]
  }
  
  #keep track of the training and testing indexes
  if(side == "1"){
    index_train_SEQN <- index_train
    index_test_SEQN <- index_test
  } else{
    index_train_SEQN <- index_test
    index_test_SEQN <- index_train
  }
  hist(bigData$RIDAGEYR[which(bigData$SEQN %in% index_train_SEQN)])
  hist(bigData$RIDAGEYR[which(bigData$SEQN %in% index_test_SEQN)])
  
  #create dataset without sex, ethnicity
  data_physiologicals <- bigData[, -which(names(bigData) %in% c("RIDRETH1", "RIAGENDR"))]
  
  #normalize data: physiologicals only (sex, ethnicity, interaction terms won't be normalized)
  data_physiologicals_train <- data_physiologicals[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  data_physiologicals_test <- data_physiologicals[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  wt <- wt.moments(data_physiologicals_train[, biomarkers_hc], data_physiologicals_train$weights)
  data_physiologicals_train[, biomarkers_hc] <- weighted_scaling(data_physiologicals_train[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_physiologicals_test[, biomarkers_hc] <- weighted_scaling(data_physiologicals_test[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_physiologicals <- rbind(data_physiologicals_train, data_physiologicals_test)
  
  #replace numbers by labels for sex and ethnicity and create dummy variables
  data_demographics <- bigData
  #sex (delete male, use it as baseline)
  if (!age_group %in% sexes)
  {
    data_demographics$RIAGENDR[which(data_demographics$RIAGENDR == 1)] <- "male"
    data_demographics$RIAGENDR[which(data_demographics$RIAGENDR == 2)] <- "female"
    dummy_sex <- data.frame(dummy(data_demographics$RIAGENDR))
    names(dummy_sex) <- c("female", "male")
    dummy_sex <- dummy_sex[,c(2,1)]
    dummy_sex <- data.frame(as.matrix(dummy_sex[,-which(names(dummy_sex) %in% c("male"))]))
    names(dummy_sex) <- c("female")
    dummy_sex_factor <- dummy_sex
    dummy_sex_factor[] <- lapply(dummy_sex_factor, factor)
  }
  #ethnicity (delete white, use it as baseline)
  if (!age_group %in% ethnicities)
  {
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 %in% c(1,2))] <- "hispanic"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 3)] <- "white"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 4)] <- "black"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 5)] <- "other"
  dummy_ethnicity <- data.frame(dummy(data_demographics$RIDRETH1))
  names(dummy_ethnicity) <- c("black", "hispanic", "other", "white")
  dummy_ethnicity <- dummy_ethnicity[,c(4,2,1,3)]
  dummy_ethnicity <- dummy_ethnicity[,-which(names(dummy_ethnicity) %in% c("white"))]
  dummy_ethnicity_factor <- dummy_ethnicity
  dummy_ethnicity_factor[] <- lapply(dummy_ethnicity_factor, factor)
  }
  
  #create dataset with demographics variables
  if (age_group %in% sexes) {
    data_demographics <- cbind(data_demographics[,-which(names(data_demographics) %in% c("RIAGENDR", "RIDRETH1"))], dummy_ethnicity_factor)
  } else if (age_group %in% ethnicities) {
    data_demographics <- cbind(data_demographics[,-which(names(data_demographics) %in% c("RIAGENDR", "RIDRETH1"))], dummy_sex_factor)
  } else {
    data_demographics <- cbind(data_demographics[,-which(names(data_demographics) %in% c("RIAGENDR", "RIDRETH1"))], dummy_sex_factor, dummy_ethnicity_factor)
  }
  if (!age_group %in% sexes)
  {
    dummy_sex <- data.frame(as.matrix(dummy_sex[complete.cases(data_demographics),]))
    names(dummy_sex) <- c("female")
  }
  if (!age_group %in% ethnicities)
  {
    dummy_ethnicity <- dummy_ethnicity[complete.cases(data_demographics),]
  }
  data_demographics <- data_demographics[complete.cases(data_demographics),]
  
  #normalize and save square data: do not split in train and test, use full data
  data_square <- data_demographics
  data_square <- data.frame(sapply(data_square, function (x) { x <- as.numeric(as.character(x))}))
  data_square$age_factor <- data_square$RIDAGEYR
  wt <- wt.moments(data_square[, -which(names(data_square) %in% c("SEQN", "weights", "age_factor"))], data_square$weights)
  data_square[, -which(names(data_square) %in% c("SEQN", "weights", "age_factor"))] <- weighted_scaling(data_square[, -which(names(data_square) %in% c("SEQN", "weights", "age_factor"))], wt$mean, sqrt(wt$var))
  data_square <- data.frame(sapply(data_square, function (x) { x <- as.numeric(as.character(x))}))
  if(age_group %in% c("mixed1", "mixed2"))
  {
    number <- str_sub(age_group,-1)
    for (type in paste(c("controlS", "controlE"), number, sep = ""))
    {
      savRDS(data_square, "Square/Data/Preprocessing/data_square", type)
    }
  } else {
    savRDS(data_square, "Square/Data/Preprocessing/data_square", age_group)
  }
  #split square data into train and test for R2s purpose
  #do it for 1 2 train test
  data_square <- data_demographics
  data_square <- data.frame(sapply(data_square, function (x) { x <- as.numeric(as.character(x))}))
  data_square$age_factor <- data_square$RIDAGEYR
  data_square_train <- data_square[which(data_square$SEQN %in% index_train_SEQN),]
  data_square_test <- data_square[which(data_square$SEQN %in% index_test_SEQN),]
  wt <- wt.moments(data_square_train[, -which(names(data_square_train) %in% c("SEQN", "weights", "age_factor"))], data_square_train$weights)
  data_square_train[, -which(names(data_square_train) %in% c("SEQN", "weights", "age_factor"))] <- weighted_scaling(data_square_train[, -which(names(data_square_train) %in% c("SEQN", "weights", "age_factor"))], wt$mean, sqrt(wt$var))
  data_square_test[, -which(names(data_square) %in% c("SEQN", "weights", "age_factor"))] <- weighted_scaling(data_square_test[, -which(names(data_square_test) %in% c("SEQN", "weights", "age_factor"))], wt$mean, sqrt(wt$var))
  if(age_group %in% c("mixed1", "mixed2"))
  {
    number <- str_sub(age_group,-1)
    for (type in paste(c("controlS", "controlE"), number, sep = ""))
    {
      savRDS(data_square_train, "Square/Data/Preprocessing/data_square", type, "train", side)
      savRDS(data_square_test, "Square/Data/Preprocessing/data_square", type, "test", side)
    }
  } else {
    savRDS(data_square_train, "Square/Data/Preprocessing/data_square", age_group, "train", side)
    savRDS(data_square_test, "Square/Data/Preprocessing/data_square", age_group, "test", side)
  }
  
  #normalize data_demographics
  if (age_group %in% sexes)
  {
    dummy_ethnicity <- dummy_ethnicity[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)),]
  } else if (age_group %in% ethnicities){
    dummy_sex$female <- dummy_sex[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)),]
  } else{
    dummy_sex$female <- dummy_sex[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)),]
    dummy_ethnicity <- dummy_ethnicity[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)),]
  }
  data_demographics_train <- data_demographics[which(data_demographics$SEQN %in% index_train_SEQN),]
  data_demographics_test <- data_demographics[which(data_demographics$SEQN %in% index_test_SEQN),]
  wt <- wt.moments(data_demographics_train[, biomarkers_hc], data_demographics_train$weights)
  data_demographics_train[, biomarkers_hc] <- weighted_scaling(data_demographics_train[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_demographics_test[, biomarkers_hc] <- weighted_scaling(data_demographics_test[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_demographics <- rbind(data_demographics_train, data_demographics_test)  
  
  #create sex and ethnicity interaction terms
  if (!(age_group %in% sexes | age_group %in% ethnicities))
  {
    interactions_sex_ethnicity <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 1*3))
    k = 0
    for (i in 1:dim(dummy_sex)[2])
    {
      for (j in 1:dim(dummy_ethnicity)[2])
      {
        k = k + 1
        name <- paste(names(dummy_sex)[i], names(dummy_ethnicity)[j], sep = ":")
        interactions_sex_ethnicity[,k] <- dummy_sex[,i]*dummy_ethnicity[,j]
        names(interactions_sex_ethnicity)[k] <- name
      }
    }
    interactions_sex_ethnicity$SEQN <- data_physiologicals$SEQN
  }

  #create datasets with interaction terms
  data_physiologicals_int <- data_physiologicals[which(data_physiologicals$SEQN %in% data_demographics$SEQN),-which(names(data_physiologicals) %in% c("SEQN", "RIDAGEYR", "weights"))]
  if (!age_group %in% sexes)
  {
    interactions_sex <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 1*dim(data_physiologicals_int)[2]))
  }
  if (!age_group %in% ethnicities)
  {
    interactions_ethnicity <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 3*dim(data_physiologicals_int)[2]))
  }
    
  k = 0
  l = 0
  for (i in seq(dim(data_physiologicals_int)[2]))
  {
    if (!age_group %in% sexes)
    {
      for (j in 1:dim(dummy_sex)[2])
      {
        k = k + 1
        name <- paste(names(data_physiologicals_int)[i], names(dummy_sex)[j], sep = ":")
        interactions_sex[,k] <- data_physiologicals_int[,i]*dummy_sex[,j]
        names(interactions_sex)[k] <- name
      }
    }
    if (!age_group %in% ethnicities)
    {
      for (j in 1:dim(dummy_ethnicity)[2])
      {
        l = l + 1
        name <- paste(names(data_physiologicals_int)[i], names(dummy_ethnicity)[j], sep = ":")
        interactions_ethnicity[,l] <- data_physiologicals_int[,i]*dummy_ethnicity[,j]
        names(interactions_ethnicity)[l] <- name
      }
    }
  }
  if (age_group %in% sexes)
  {
    data_ethnicity <- cbind(data_demographics[,-which(names(data_demographics) %in% c("female"))], interactions_ethnicity)
    data_sex_ethnicity <- cbind(data_demographics, interactions_ethnicity)
  } else if (age_group %in% ethnicities) {
    data_sex <- cbind(data_demographics[,-which(names(data_demographics) %in% c("hispanic", "black", "other"))], interactions_sex)
    data_sex_ethnicity <- cbind(data_demographics, interactions_sex)
  } else {
    data_sex <- cbind(data_demographics[,-which(names(data_demographics) %in% c("hispanic", "black", "other"))], interactions_sex)
    data_ethnicity <- cbind(data_demographics[,-which(names(data_demographics) %in% c("female"))], interactions_ethnicity)
    data_sex_ethnicity <- cbind(data_demographics, interactions_sex, interactions_ethnicity, interactions_sex_ethnicity[,-which(names(interactions_sex_ethnicity) %in% c("SEQN"))])
  }

  #turn into numeric (for cv.glmnet purpose)
  data_physiologicals <- data.frame(sapply(data_physiologicals, function (x) { x <- as.numeric(as.character(x))}))
  data_demographics <- data.frame(sapply(data_demographics, function (x) { x <- as.numeric(as.character(x))}))
  data_baseline = data_physiologicals
  if (!age_group %in% sexes)
  {
    data_sex <- data.frame(sapply(data_sex, function (x) { x <- as.numeric(as.character(x))}))
  }
  if (!age_group %in% ethnicities)
  {
    data_ethnicity <- data.frame(sapply(data_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  }    
  data_sex_ethnicity <- data.frame(sapply(data_sex_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  interactions_sex_ethnicity <- data.frame(sapply(interactions_sex_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  
  #split into training and testing
  data_physiologicals_train <- data_physiologicals[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  data_physiologicals_test <- data_physiologicals[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  data_demographics_train <- data_demographics[which(data_demographics$SEQN %in% index_train_SEQN),]
  data_demographics_test <- data_demographics[which(data_demographics$SEQN %in% index_test_SEQN),]
  data_baseline_train <- data_baseline[which(data_baseline$SEQN %in% index_train_SEQN),]
  data_baseline_test <- data_baseline[which(data_baseline$SEQN %in% index_test_SEQN),]
  if (!age_group %in% sexes)
  {
    data_sex_train <- data_sex[which(data_sex$SEQN %in% index_train_SEQN),]
    data_sex_test <- data_sex[which(data_sex$SEQN %in% index_test_SEQN),]
  }
  if (!age_group %in% ethnicities)
  {
    data_ethnicity_train <- data_ethnicity[which(data_ethnicity$SEQN %in% index_train_SEQN),]
    data_ethnicity_test <- data_ethnicity[which(data_ethnicity$SEQN %in% index_test_SEQN),]
  }
  data_sex_ethnicity_train <- data_sex_ethnicity[which(data_sex_ethnicity$SEQN %in% index_train_SEQN),]
  data_sex_ethnicity_test <- data_sex_ethnicity[which(data_sex_ethnicity$SEQN %in% index_test_SEQN),]
  interactions_sex_ethnicity_train <- interactions_sex_ethnicity[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  interactions_sex_ethnicity_test <- interactions_sex_ethnicity[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  
  #turn into numeric (for cv.glmnet purpose)
  data_physiologicals_train <- data.frame(sapply(data_physiologicals_train, function (x) { x <- as.numeric(as.character(x))}))
  data_physiologicals_test <- data.frame(sapply(data_physiologicals_test, function (x) { x <- as.numeric(as.character(x))}))
  data_demographics_train <- data.frame(sapply(data_demographics_train, function (x) { x <- as.numeric(as.character(x))}))
  data_demographics_test <- data.frame(sapply(data_demographics_test, function (x) { x <- as.numeric(as.character(x))}))
  data_baseline_train = data_physiologicals_train
  data_baseline_test = data_physiologicals_test
  if (!age_group %in% sexes)
  {
    data_sex_train <- data.frame(sapply(data_sex_train, function (x) { x <- as.numeric(as.character(x))}))
    data_sex_test <- data.frame(sapply(data_sex_test, function (x) { x <- as.numeric(as.character(x))}))
  }
  if (!age_group %in% ethnicities)
  {
    data_ethnicity_train <- data.frame(sapply(data_ethnicity_train, function (x) { x <- as.numeric(as.character(x))}))
    data_ethnicity_test <- data.frame(sapply(data_ethnicity_test, function (x) { x <- as.numeric(as.character(x))}))
  }
  data_sex_ethnicity_train <- data.frame(sapply(data_sex_ethnicity_train, function (x) { x <- as.numeric(as.character(x))}))
  data_sex_ethnicity_test <- data.frame(sapply(data_sex_ethnicity_test, function (x) { x <- as.numeric(as.character(x))}))
  interactions_sex_ethnicity_train <- data.frame(sapply(interactions_sex_ethnicity_train, function (x) { x <- as.numeric(as.character(x))}))
  interactions_sex_ethnicity_test <- data.frame(sapply(interactions_sex_ethnicity_test, function (x) { x <- as.numeric(as.character(x))}))
  
  # prepare data for different models
  y <- data.frame(as.matrix(data_physiologicals$RIDAGEYR))
  names(y) <- c("RIDAGEYR")
  y_train <- data.frame(as.matrix(y[which(data_physiologicals$SEQN %in% index_train_SEQN),]))
  names(y_train) <- c("RIDAGEYR")
  y_test <- data.frame(as.matrix(y[which(data_physiologicals$SEQN %in% index_test_SEQN),]))
  names(y_test) <- c("RIDAGEYR")
  
  x_physiologicals_train = data_physiologicals_train[,-which(names(data_physiologicals_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_physiologicals_test = data_physiologicals_test[,-which(names(data_physiologicals_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_demographics_train = data_demographics_train[,-which(names(data_demographics_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_demographics_test = data_demographics_test[,-which(names(data_demographics_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_baseline_train = data_baseline_train[,-which(names(data_baseline_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_baseline_test = data_baseline_test[,-which(names(data_baseline_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  if (!age_group %in% sexes)
  {
    x_sex_train = data_sex_train[,-which(names(data_sex_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
    x_sex_test = data_sex_test[,-which(names(data_sex_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  }
  if (!age_group %in% ethnicities)
  {
    x_ethnicity_train = data_ethnicity_train[,-which(names(data_ethnicity_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
    x_ethnicity_test = data_ethnicity_test[,-which(names(data_ethnicity_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  }
  x_sex_ethnicity_train = data_sex_ethnicity_train[,-which(names(data_sex_ethnicity_train) %in% c("SEQN", "RIDAGEYR", "weights"))]
  x_sex_ethnicity_test = data_sex_ethnicity_test[,-which(names(data_sex_ethnicity_test) %in% c("SEQN", "RIDAGEYR", "weights"))]
  weight_train <- data_physiologicals_train$weights
  weight_test <- data_physiologicals_test$weights
  
  #save
  #exclude some files depending on the demographic group
  if (age_group %in% sexes){
    predictors_list2 <- predictors_list[-2]
  } else if (age_group %in% ethnicities) {
    predictors_list2 <- predictors_list[-3]
  } else {
    predictors_list2 <- predictors_list
  }
  for (half in c("train", "test"))
  {
    savRDS(get(file_name("y", half)), "Preprocessing/y", analysis, age_group, half, side)
    for (predictors in predictors_list2)
    {
      savRDS(get(file_name("data", predictors, half)), "Preprocessing/data", analysis, age_group, predictors, half, side)
      savRDS(get(file_name("x", predictors, half)), "Preprocessing/x", analysis, age_group, predictors, half, side)
    }
    savRDS(get(file_name("data", "demographics", half)), "Preprocessing/data", analysis, age_group, "demographics", half, side)
    savRDS(get(file_name("x", "demographics", half)), "Preprocessing/x", analysis, age_group, "demographics", half, side)    
  }
  #also save full data, including data_demographics
  savRDS(data_demographics, "Preprocessing/data", analysis, age_group, "demographics", side)
  for (predictors in predictors_list2)
  {
    savRDS(get(file_name("data", predictors)), "Preprocessing/data", analysis, age_group, predictors, side)
  }
}


#summarize coef1 and coef2 to give conservative coefficients
conservative_significance <- function(coef1, coef2, demographic_group)
{
  coef_12 <- cbind(coef1, coef2)
  coef_calc <- cbind(coef1[,1], coef2[,1])
  coef_12$coef_mean <- apply(coef_calc, 1, mean)
  coef_12$coef_sd <- apply(coef_calc, 1, sd)
  coef_12$p_max <- pmax(coef_12[,2], coef_12[,6])
  coef_12$corrected_max <- pmax(coef_12[,3], coef_12[,7])
  names(coef_12) <- c("coef1", "p1", "corrected1", "coef2", "p2", "corrected2", "coef", "coef_sd", "p", "corrected")
  coef_12 <- coef_12[,c(7,9,10,8,1,4,2,5,3,6)]
  return(coef_12)
}



generate_plots <- function(summary_coef, analysis, side)
{
  coef <- summary_coef[[1,1]]
  coef_bound <- data.frame(matrix(0, 0, ncol(coef)))
  names(coef_bound) <- names(coef)
  for (age_group in age_groups)
  {
    for (demographic_group in demographics_groups)
    {
      coef <- get_data_unlist(summary_coef, demographic_group, age_group)
      plot_diff(coef, file_name("coef", analysis, age_group, demographic_group, side))
      coef$age <- age_group
      coef$demo <- demographic_group
      coef_bound <- rbind(coef_bound, coef)
    }
  }
  
  #for rownames, colnames and dimension purposes:
  coef <- summary_coef[[1,1]]
  #format
  coef_bound$color <-  ifelse(coef_bound$corrected < 0.05, "significant", "non-significant")
  coef_bound$age <- factor(coef_bound$age, levels = as.factor(age_groups))
  coef_bound$demo <- factor(coef_bound$demo, levels = as.factor(demographics_groups))
  coef_bound$color <- factor(coef_bound$color, levels = as.factor(significance_levels))
  #create labels
  coef_bound$text <- rownames(coef_bound)
  for (i in seq(nrow(coef)))
  {
    if(rownames(coef)[i] %in% biomarkers_labels$name){
      coef_bound$text[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(coef)[i])]
    }
  }
  #calculating indexes to leave alone for text
  n_a <- nrow(coef_bound)/length(age_groups)
  n_b <- nrow(get_data_unlist(summary_coef, "male", "all"))
  n_c <- nrow(get_data_unlist(summary_coef, "female", "all"))
  #indexes to leave alone, in two steps
  index_a <- 1 + c(2*n_b, 3*n_b+n_c, 3*n_b+2*n_c, 3*n_b+3*n_c)
  index_b <- rowSums(expand.grid(index_a,(0:4)*n_a))
  #input text for all samples
  coef_bound$text[-index_b] <- coef_bound$text[1:n_b]
  coef_bound$text[1+2*n_b+(0:4)*n_a] <- "female"
  coef_bound$text[1+3*n_b+n_c+(0:4)*n_a] <- "hispanic"
  coef_bound$text[1+3*n_b+2*n_c+(0:4)*n_a] <- "black"
  coef_bound$text[1+3*n_b+3*n_c+(0:4)*n_a] <- "other"
  #plot
  cols = c("significant" = "red", "non-significant" = "black")
  names(cols) <- c("significant", "non-significant")
  cols <- cols[which(names(cols) %in% names(table(coef_bound$color)[which(table(coef_bound$color) > 0)]))]
  p <- ggplot(coef_bound, aes(coef, -log10(corrected), text = text, color = color))  + geom_point() + geom_hline(yintercept = -log10(0.05), color = "black") + scale_color_manual(values= cols) + xlab("Coefficients") + ylab("Negative log corrected p-values") + theme(axis.text.x= element_text(size = 5), axis.text.y= element_text(size = 5)) + scale_y_continuous(position = "right") + facet_grid(demo~age, switch = "y")
  p
  ggsav("Coefficients/Comparisons/p_summary", analysis, side)
  savRDS(coef_bound, "Coefficients/Comparisons/Summary_coef_", analysis, side)
  p <- plotly_build(p)
  if (analysis == "age")
  {
    p$x$data[[1]]$text <- coef_bound$text[which(coef_bound$color == "significant")]
    p$x$data[[2]]$text <- coef_bound$text[which(coef_bound$color == "non-significant")]    
  }
  htmlsav(p, "Coefficients/Comparisons/p_summary", analysis, side)
  
  
  #generate comparison plots
  coef_youngVSold_bound <- data.frame(matrix(0, 0, 2*ncol(coef)+2))
  coef_mixed1VSmixed2_bound <- data.frame(matrix(0, 0, 2*ncol(coef)+2))
  for (demographic_group in demographics_groups)
  {
    #YvsO
    coef_young <- get_data_unlist(summary_coef, demographic_group, "young")
    coef_old <- get_data_unlist(summary_coef, demographic_group, "old")
    comp_plot(coef_young, coef_old, "young", "old", file_name("Coefficients/Comparisons/Comp_YvsO", analysis, demographic_group, side))
    ifelse(side == "12", coef_youngVSold <- cbind(coef_young[,1:3], coef_old[,1:3]), coef_youngVSold <- cbind(coef_young, coef_old))
    coef_youngVSold$age <- "youngVSold"
    coef_youngVSold$demo <- demographic_group
    coef_youngVSold_bound <- rbind(coef_youngVSold_bound, coef_youngVSold)
    #M1vsM2
    coef_mixed1 <- get_data_unlist(summary_coef, demographic_group, "mixed1")
    coef_mixed2 <- get_data_unlist(summary_coef, demographic_group, "mixed2")
    comp_plot(coef_mixed1, coef_mixed2, "mixed1", "mixed2", file_name("Coefficients/Comparisons/Comp_M1vsM2", analysis, demographic_group, side))
    ifelse(side == "12", coef_mixed1VSmixed2 <- cbind(coef_young[,1:3], coef_old[,1:3]), coef_mixed1VSmixed2 <- cbind(coef_young, coef_old))
    coef_mixed1VSmixed2$age <- "mixed1VSmixed2"
    coef_mixed1VSmixed2$demo <- demographic_group
    coef_mixed1VSmixed2_bound <- rbind(coef_mixed1VSmixed2_bound, coef_mixed1VSmixed2)
  }
  coef_bound_comp <- rbind(coef_youngVSold_bound, coef_mixed1VSmixed2_bound)
  coef_bound_comp <- coef_bound_comp[,c(1,4,2,5,3,6,7,8)]
  names(coef_bound_comp) <- c("coef1", "coef2", "p1", "p2", "nlp1", "nlp2", "age", "demo")
  #color
  coef_bound_comp$color <- "none"
  coef_bound_comp$color[which(coef_bound_comp$nlp1 < 0.05)] <- "1"
  coef_bound_comp$color[which(coef_bound_comp$nlp2 < 0.05)] <- "2"
  coef_bound_comp$color[which(coef_bound_comp$nlp1 < 0.05 & coef_bound_comp$nlp2 < 0.05)] <- "both"
  coef_bound_comp$nlp1 <- -log10(coef_bound_comp$nlp1)
  coef_bound_comp$nlp2 <- -log10(coef_bound_comp$nlp2)
  #format
  coef_bound_comp$age <- factor(coef_bound_comp$age, levels = as.factor(comparison_groups))
  coef_bound_comp$demo <- factor(coef_bound_comp$demo, levels = as.factor(demographics_groups))
  coef_bound_comp$color <- factor(coef_bound_comp$color, levels = as.factor(comparison_categories))
  #create labels
  coef_bound_comp$text <- rownames(coef_bound_comp)
  for (i in seq(nrow(coef_bound_comp)))
  {
    if(rownames(coef_bound_comp)[i] %in% biomarkers_labels$name){
      coef_bound_comp$text[i] <- biomarkers_labels$label[which(biomarkers_labels$name == rownames(coef_bound_comp)[i])]
    }
  }
  
  #indexes to leave alone, in two steps
  index_a <- 1 + c(2*n_b, 3*n_b+n_c, 3*n_b+2*n_c, 3*n_b+3*n_c)
  index_b <- rowSums(expand.grid(index_a,(0:1)*n_a))
  #input text for all samples
  coef_bound_comp$text[-index_b] <- coef_bound_comp$text[1:n_b]
  coef_bound_comp$text[1+2*n_b+(0:1)*n_a] <- "female"
  coef_bound_comp$text[1+3*n_b+n_c+(0:1)*n_a] <- "hispanic"
  coef_bound_comp$text[1+3*n_b+2*n_c+(0:1)*n_a] <- "black"
  coef_bound_comp$text[1+3*n_b+3*n_c+(0:1)*n_a] <- "other"
  coef_bound_comp$text <- paste(coef_bound_comp$text, ", -lp1=", round(coef_bound_comp$nlp1, 1), ", -lp2=", round(coef_bound_comp$nlp2, 1))

  #plot
  cols = c("both" = "green", "1" = "blue", "2" = "red", "none" = "black")
  names(cols) <- c("both", "1", "2", "none")
  cols <- cols[which(names(cols) %in% names(table(coef_bound_comp$color)[which(table(coef_bound_comp$color) > 0)]))]
  p_comp <- ggplot(coef_bound_comp, aes(coef1, coef2, text = text, color = color))  + geom_point() + geom_abline(slope = 1, intercept = 0, color = "black") + scale_color_manual(values = cols) + xlab("Coefficients") + ylab("Coefficients") + theme(axis.text.x= element_text(size = 5), axis.text.y= element_text(size = 5)) + scale_y_continuous(position = "right") + facet_grid(demo~age, switch = "y")
  p_comp
  #save
  ggsav("Coefficients/Comparisons/p_summary_comp", analysis, side)
  savRDS(coef_bound_comp, "Coefficients/Comparisons/Summary_coef_comp_", analysis, side)
  p_comp <- plotly_build(p_comp)
  p_comp
  if (analysis == "age")
  {
    p_comp$x$data[[1]]$text <- coef_bound_comp$text[which(coef_bound_comp$color == "significant")]
    p_comp$x$data[[2]]$text <- coef_bound_comp$text[which(coef_bound_comp$color == "non-significant")]    
  }
  htmlsav(p_comp, "Coefficients/Comparisons/p_summary_comp", analysis, side)
}


generate_data_surv <- function(age_group, side, index_train, index_test, index_1vs2, subset = "surv")
{
  analysis = subset
  
  #load and format data
  bigData <- redRDS("Preprocessing/bigData")
  #reorganize predictors based on cluster to make the reading of the coefficients easier during post processing
  bigData[,biomarkers] <- bigData[,biomarkers][,biomarkers_hc]
  names(bigData)[which(names(bigData) %in% biomarkers)] <- biomarkers_hc
  
  #get rid of appropriate samples for surv, cancer, or cardiac
  if (analysis == "cardiac")
  {
    index <- redRDS("Preprocessing/SEQN", analysis)
    bigData <- bigData[which(bigData$SEQN %in% index),]  } else if (analysis == "cancer") {
    index <- redRDS("Preprocessing/SEQN", analysis)
    bigData <- bigData[which(bigData$SEQN %in% index),] } else {
    #get rid of deaths by trauma
    index <- redRDS("Preprocessing/SEQN_notrauma")
    bigData <- bigData[which(bigData$SEQN %in% index),]
  }
  #get rid of deaths by trauma
  bigData <- merge(bigData, survival_var, by = "SEQN")
  
  #get rid of samples depending on the cohort
  if(age_group == "young")
  {
    bigData <- bigData[which(bigData$RIDAGEYR < 50),]
  } else if(age_group == "old")
  {
    bigData <- bigData[-which(bigData$RIDAGEYR < 50),]
  } else if(age_group == "mixed1")
  {
    bigData <- bigData[which(bigData$SEQN %in% index_1vs2),]
  } else if(age_group == "mixed2")
  {
    bigData <- bigData[-which(bigData$SEQN %in% index_1vs2),]
  }
  
  #keep track of the training and testing indexes
  if(side == "1"){
    index_train_SEQN <- index_train
    index_test_SEQN <- index_test
  } else{
    index_train_SEQN <- index_test
    index_test_SEQN <- index_train
  }
  hist(bigData$RIDAGEYR[which(bigData$SEQN %in% index_train_SEQN)])
  hist(bigData$RIDAGEYR[which(bigData$SEQN %in% index_test_SEQN)])
  
  #create dataset without sex, ethnicity
  data_physiologicals <- bigData[, -which(names(bigData) %in% c("RIDRETH1", "RIAGENDR"))]
  
  #normalize data: physiologicals only (sex, ethnicity, interaction terms won't be normalized)
  data_physiologicals_train <- data_physiologicals[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  data_physiologicals_test <- data_physiologicals[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  wt <- wt.moments(data_physiologicals_train[, biomarkers_hc], data_physiologicals_train$weights)
  data_physiologicals_train[, biomarkers_hc] <- weighted_scaling(data_physiologicals_train[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_physiologicals_test[, biomarkers_hc] <- weighted_scaling(data_physiologicals_test[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_physiologicals <- rbind(data_physiologicals_train, data_physiologicals_test)
  
  #replace numbers by labels for sex and ethnicity and create dummy variables
  #sex (delete male, use it as baseline)
  data_demographics <- bigData
  data_demographics$RIAGENDR[which(data_demographics$RIAGENDR == 1)] <- "male"
  data_demographics$RIAGENDR[which(data_demographics$RIAGENDR == 2)] <- "female"
  dummy_sex <- data.frame(dummy(data_demographics$RIAGENDR))
  names(dummy_sex) <- c("female", "male")
  dummy_sex <- dummy_sex[,c(2,1)]
  dummy_sex <- data.frame(as.matrix(dummy_sex[,-which(names(dummy_sex) %in% c("male"))]))
  names(dummy_sex) <- c("female")
  dummy_sex_factor <- dummy_sex
  dummy_sex_factor[] <- lapply(dummy_sex_factor, factor)
  
  #ethnicity (delete white, use it as baseline)
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 %in% c(1,2))] <- "hispanic"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 3)] <- "white"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 4)] <- "black"
  data_demographics$RIDRETH1[which(data_demographics$RIDRETH1 == 5)] <- "other"
  dummy_ethnicity <- data.frame(dummy(data_demographics$RIDRETH1))
  names(dummy_ethnicity) <- c("black", "hispanic", "other", "white")
  dummy_ethnicity <- dummy_ethnicity[,c(4,2,1,3)]
  dummy_ethnicity <- dummy_ethnicity[,-which(names(dummy_ethnicity) %in% c("white"))]
  dummy_ethnicity_factor <- dummy_ethnicity
  dummy_ethnicity_factor[] <- lapply(dummy_ethnicity_factor, factor)
  
  #create dataset with demographics variables
  data_demographics <- cbind(data_demographics[,-which(names(data_demographics) %in% c("RIAGENDR", "RIDRETH1"))], dummy_sex_factor, dummy_ethnicity_factor)
  dummy_sex <- data.frame(as.matrix(dummy_sex[complete.cases(data_demographics),]))
  names(dummy_sex) <- c("female")
  dummy_ethnicity <- dummy_ethnicity[complete.cases(data_demographics),]
  data_demographics <- data_demographics[complete.cases(data_demographics),]
  
  #normalize data_demographics
  dummy_sex$female <- dummy_sex[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)),]
  dummy_ethnicity <- dummy_ethnicity[c(which(data_demographics$SEQN %in% index_train_SEQN), which(data_demographics$SEQN %in% index_test_SEQN)), ]
  data_demographics_train <- data_demographics[which(data_demographics$SEQN %in% index_train_SEQN),]
  data_demographics_test <- data_demographics[which(data_demographics$SEQN %in% index_test_SEQN),]
  wt <- wt.moments(data_demographics_train[, biomarkers_hc], data_demographics_train$weights)
  data_demographics_train[, biomarkers_hc] <- weighted_scaling(data_demographics_train[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_demographics_test[, biomarkers_hc] <- weighted_scaling(data_demographics_test[, biomarkers_hc], wt$mean, sqrt(wt$var))
  data_demographics <- rbind(data_demographics_train, data_demographics_test)   
  
  #create sex and ethnicity interaction terms
  interactions_sex_ethnicity <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 1*3))
  k = 0
  for (i in 1:dim(dummy_sex)[2])
  {
    for (j in 1:dim(dummy_ethnicity)[2])
    {
      k = k + 1
      name <- paste(names(dummy_sex)[i], names(dummy_ethnicity)[j], sep = ":")
      interactions_sex_ethnicity[,k] <- dummy_sex[,i]*dummy_ethnicity[,j]
      names(interactions_sex_ethnicity)[k] <- name
    }
  }
  interactions_sex_ethnicity$SEQN <- data_physiologicals$SEQN
  
  #create datasets with interaction terms
  data_physiologicals_int <- data_physiologicals[which(data_physiologicals$SEQN %in% data_demographics$SEQN),-which(names(data_physiologicals) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  interactions_sex <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 1*dim(data_physiologicals_int)[2]))
  interactions_ethnicity <- data.frame(matrix(nrow = dim(data_demographics)[1], ncol = 3*dim(data_physiologicals_int)[2]))
  k = 0
  l = 0
  for (i in seq(dim(data_physiologicals_int)[2]))
  {
    for (j in 1:dim(dummy_sex)[2])
    {
      k = k + 1
      name <- paste(names(data_physiologicals_int)[i], names(dummy_sex)[j], sep = ":")
      interactions_sex[,k] <- data_physiologicals_int[,i]*dummy_sex[,j]
      names(interactions_sex)[k] <- name
    }
    for (j in 1:dim(dummy_ethnicity)[2])
    {
      l = l + 1
      name <- paste(names(data_physiologicals_int)[i], names(dummy_ethnicity)[j], sep = ":")
      interactions_ethnicity[,l] <- data_physiologicals_int[,i]*dummy_ethnicity[,j]
      names(interactions_ethnicity)[l] <- name
    }
  }
  data_sex <- cbind(data_demographics[,-which(names(data_demographics) %in% c("hispanic", "black", "other"))], interactions_sex)
  data_ethnicity <- cbind(data_demographics[,-which(names(data_demographics) %in% c("female"))], interactions_ethnicity)
  data_sex_ethnicity <- cbind(data_demographics, interactions_sex, interactions_ethnicity, interactions_sex_ethnicity[,-which(names(interactions_sex_ethnicity) %in% c("SEQN"))])
  
  #turn into numeric (for cv.glmnet purpose)
  data_physiologicals <- data.frame(sapply(data_physiologicals, function (x) { x <- as.numeric(as.character(x))}))
  data_demographics <- data.frame(sapply(data_demographics, function (x) { x <- as.numeric(as.character(x))}))
  data_control = data_physiologicals[, which(names(data_physiologicals) %in%  c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT", "RIDAGEYR"))]
  data_baseline = data_physiologicals
  data_sex <- data.frame(sapply(data_sex, function (x) { x <- as.numeric(as.character(x))}))
  data_ethnicity <- data.frame(sapply(data_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  data_sex_ethnicity <- data.frame(sapply(data_sex_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  interactions_sex_ethnicity <- data.frame(sapply(interactions_sex_ethnicity, function (x) { x <- as.numeric(as.character(x))}))
  
  #split into training and testing
  data_physiologicals_train <- data_physiologicals[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  data_physiologicals_test <- data_physiologicals[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  data_demographics_train <- data_demographics[which(data_demographics$SEQN %in% index_train_SEQN),]
  data_demographics_test <- data_demographics[which(data_demographics$SEQN %in% index_test_SEQN),]
  data_control_train <- data_control[which(data_control$SEQN %in% index_train_SEQN),]
  data_control_test <- data_control[which(data_control$SEQN %in% index_test_SEQN),]
  data_baseline_train <- data_baseline[which(data_baseline$SEQN %in% index_train_SEQN),]
  data_baseline_test <- data_baseline[which(data_baseline$SEQN %in% index_test_SEQN),]
  data_sex_train <- data_sex[which(data_sex$SEQN %in% index_train_SEQN),]
  data_sex_test <- data_sex[which(data_sex$SEQN %in% index_test_SEQN),]
  data_ethnicity_train <- data_ethnicity[which(data_ethnicity$SEQN %in% index_train_SEQN),]
  data_ethnicity_test <- data_ethnicity[which(data_ethnicity$SEQN %in% index_test_SEQN),]
  data_sex_ethnicity_train <- data_sex_ethnicity[which(data_sex_ethnicity$SEQN %in% index_train_SEQN),]
  data_sex_ethnicity_test <- data_sex_ethnicity[which(data_sex_ethnicity$SEQN %in% index_test_SEQN),]
  interactions_sex_ethnicity_train <- interactions_sex_ethnicity[which(data_physiologicals$SEQN %in% index_train_SEQN),]
  interactions_sex_ethnicity_test <- interactions_sex_ethnicity[which(data_physiologicals$SEQN %in% index_test_SEQN),]
  
  # prepare data for different models
  y <- Surv(data_physiologicals$PERMTH_EXM, data_physiologicals$MORTSTAT)
  y_train <- y[which(data_physiologicals$SEQN %in% index_train_SEQN)]
  y_test <- y[which(data_physiologicals$SEQN %in% index_test_SEQN)]
  
  x_physiologicals_train = data_physiologicals_train[,-which(names(data_physiologicals_train) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_physiologicals_test = data_physiologicals_test[,-which(names(data_physiologicals_test) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_demographics_train = data_demographics_train[,-which(names(data_demographics_train) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_demographics_test = data_demographics_test[,-which(names(data_demographics_test) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_control_train <- data.frame(as.matrix(data_control_train[, -which(names(data_control_train) %in%  c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))], ncol = 1))  
  x_control_test <- data.frame(as.matrix(data_control_test[, -which(names(data_control_test) %in%  c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))], ncol = 1))  
  names(x_control_train) <- "RIDAGEYR"
  names(x_control_test) <- "RIDAGEYR"
  x_control_train <- data_control_train[, -which(names(data_control_train) %in%  c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_control_test <- data_control_test[, -which(names(data_control_test) %in%  c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_baseline_train = x_physiologicals_train
  x_baseline_test = x_physiologicals_test
  x_sex_train = data_sex_train[,-which(names(data_sex_train) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_sex_test = data_sex_test[,-which(names(data_sex_test) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_ethnicity_train = data_ethnicity_train[,-which(names(data_ethnicity_train) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_ethnicity_test = data_ethnicity_test[,-which(names(data_ethnicity_test) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_sex_ethnicity_train = data_sex_ethnicity_train[,-which(names(data_sex_ethnicity_train) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  x_sex_ethnicity_test = data_sex_ethnicity_test[,-which(names(data_sex_ethnicity_test) %in% c("SEQN", "weights", "PERMTH_EXM", "MORTSTAT"))]
  weight_train <- data_physiologicals_train$weights
  weight_test <- data_physiologicals_test$weights
  
  #save
  for (half in c("train", "test"))
  {
    savRDS(get(file_name("y", half)), "Preprocessing/y", analysis, age_group, half, side)
    for (predictors in predictors_list_surv)
    {
      savRDS(get(file_name("data", predictors, half)), "Preprocessing/data", analysis, age_group, predictors, half, side)
      savRDS(get(file_name("x", predictors, half)), "Preprocessing/x", analysis, age_group, predictors, half, side)
    }
  }
}


cor_and_comp <- function(summary_coef_1, summary_coef_2, name = "")
{
  for (age_group in age_groups)
  {
    Comp_coef <- initiate_store(comparisons, demographics_groups)
    for (demographic_group in demographics_groups)
    {
      coef1 <- get_data_unlist(summary_coef_1, demographic_group, age_group)
      coef2 <- get_data_unlist(summary_coef_2, demographic_group, age_group)
      Comp_coef[, which(names(Comp_coef) == demographic_group)] <- cor_and_comp_model(coef1, coef2)
      names(Comp_coef[, which(names(Comp_coef) == demographic_group)]) <- demographic_group
    }
    savRDS(Comp_coef, "Coefficients/Comparisons/Comp_coef", name, age_group)
  }
}

cor_and_comp_within <- function(summary_coef, name = "")
{
  #YvsO
  Comp_coef <- initiate_store(comparisons, demographics_groups)
  for (demographic_group in demographics_groups)
  {
    coef1 <- get_data_unlist(summary_coef, demographic_group, "young")
    coef2 <- get_data_unlist(summary_coef, demographic_group, "old")
    Comp_coef[, which(names(Comp_coef) == demographic_group)] <- cor_and_comp_model(coef1, coef2)
    names(Comp_coef[, which(names(Comp_coef) == demographic_group)]) <- demographic_group
  }
  savRDS(Comp_coef, "Coefficients/Comparisons/Comp_coef", name, "YvsO")
  #M1vsM2
  Comp_coef <- initiate_store(comparisons, demographics_groups)
  for (demographic_group in demographics_groups)
  {
    coef1 <- get_data_unlist(summary_coef, demographic_group, "mixed1")
    coef2 <- get_data_unlist(summary_coef, demographic_group, "mixed2")
    Comp_coef[, which(names(Comp_coef) == demographic_group)] <- cor_and_comp_model(coef1, coef2)
    names(Comp_coef[, which(names(Comp_coef) == demographic_group)]) <- demographic_group
  }
  savRDS(Comp_coef, "Coefficients/Comparisons/Comp_coef", name, "M1vsM2")
}


# function to create risk groups: bins size: 0.5. start with group <-1.5, end with group > 2.5
risk_group <- function(x){
  if (x < -2){
    return(-4)
  }
  else if (x > 2.5){
    return(5)
  }
  else if (x < 0){
    return(-ceiling(abs(x)/0.5))
  }
  else{
    return(ceiling(x/0.5))
  }
}


#Correlation with VO2m and telomere length
bio_ages_comp <- function(BA, age_group)
{
  #a) Generate Data
  data_aging <- merge(BA, aging_var, by = "SEQN")
  data_aging <- merge(data_aging, demographics_var, by = "SEQN")
  data_aging <- data_aging[, which(names(data_aging) != "AA")]
  if (length(table(data_aging$BA)) > 1)
  {
    data_aging$BA <- normalize(data_aging$BA)
  }
  data_aging$RIDAGEYR <- normalize(data_aging$RIDAGEYR)
  #telomeres length
  data_telo <- data_aging %>% select(-CVDESVO2) %>% filter(complete.cases(.))
  if (age_group != "old")
  {
    #VO2m
    data_vo2m <- data_aging %>% select(-TELOMEAN) %>% filter(complete.cases(.))
    #telo + VO2m
    data_telo_vo2m <- data_aging %>% filter(complete.cases(.))      
  }
  
  #b) Generate residuals for correlation purposes
  #telo
  lm_telo <- lm(TELOMEAN ~ RIDAGEYR, data=data_telo, weights = data_telo$weights)
  data_telo$res_telo <- data_telo$TELOMEAN - predict(lm_telo, data_telo)
  lm_BA <- lm(BA ~ RIDAGEYR, data=data_telo, weights = data_telo$weights)
  data_telo$res_BA <- data_telo$BA - predict(lm_BA, data_telo)
  if (age_group != "old")
  {
    #VO2m
    lm_VO2m <- lm(CVDESVO2 ~ RIDAGEYR, data=data_vo2m, weights = data_vo2m$weights)
    data_vo2m$res_vo2m <- data_vo2m$CVDESVO2 - predict(lm_VO2m, data_vo2m)
    lm_BA <- lm(BA ~ RIDAGEYR, data=data_vo2m, weights = data_vo2m$weights)
    data_vo2m$res_BA <- data_vo2m$BA - predict(lm_BA, data_vo2m)
    #telo + VO2m
    #telo
    lm_telo <- lm(TELOMEAN ~ RIDAGEYR, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    data_telo_vo2m$res_telo <- data_telo_vo2m$TELOMEAN - predict(lm_telo, data_telo_vo2m)
    #VO2m
    lm_VO2m <- lm(CVDESVO2 ~ RIDAGEYR, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    data_telo_vo2m$res_vo2m <- data_telo_vo2m$CVDESVO2 - predict(lm_VO2m, data_telo_vo2m)
    #BA
    lm_BA <- lm(BA ~ RIDAGEYR, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    data_telo_vo2m$res_BA <- data_telo_vo2m$BA - predict(lm_BA, data_telo_vo2m)
  }
  
  #c) Corrected Correlations
  #telomeres length
  lm_res_telo <- lm(res_telo ~. -SEQN - weights - BA - TELOMEAN, data=data_telo, weights = data_telo$weights)
  summary(lm_res_telo)
  c_bt <- summary(lm_res_telo)$coefficients[which(rownames(summary(lm_res_telo)$coefficients) == "res_BA"), 1]
  p_bt <- summary(lm_res_telo)$coefficients[which(rownames(summary(lm_res_telo)$coefficients) == "res_BA"), 4]
  if (age_group != "old")
  {
    #VO2m
    lm_res_vo2m <- lm(res_vo2m ~. -SEQN - weights - BA - CVDESVO2, data=data_vo2m, weights = data_vo2m$weights)
    summary(lm_res_vo2m)
    c_bv <- summary(lm_res_vo2m)$coefficients[which(rownames(summary(lm_res_vo2m)$coefficients) == "res_BA"), 1]
    p_bv <- summary(lm_res_vo2m)$coefficients[which(rownames(summary(lm_res_vo2m)$coefficients) == "res_BA"), 4]
    #telomeres length vs VO2m
    lm_res_telo_vo2m <- lm(res_vo2m ~. -SEQN - weights - BA - TELOMEAN - CVDESVO2 - res_BA, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    summary(lm_res_telo_vo2m)
    c_tv <- summary(lm_res_telo_vo2m)$coefficients[which(rownames(summary(lm_res_telo_vo2m)$coefficients) == "res_telo"), 1]
    p_tv <- summary(lm_res_telo_vo2m)$coefficients[which(rownames(summary(lm_res_telo_vo2m)$coefficients) == "res_telo"), 4]
    #pred corrected for both telo and VO2m
    #res (DA = - AA)
    lm_res_pred_telo_vo2m <- lm(res_BA ~. -SEQN - weights - BA - TELOMEAN - CVDESVO2, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    summary(lm_res_pred_telo_vo2m)
    #absolute values (BA)
    lm_pred_telo_vo2m <- lm(BA ~. -SEQN - weights - res_BA - res_telo - res_vo2m, data=data_telo_vo2m, weights = data_telo_vo2m$weights)
    summary(lm_pred_telo_vo2m)
  }
  #store results
  coef <- c(NaN, NaN, NaN)
  p <- c(NaN, NaN, NaN)
  if (length(c_bt) > 1)
  {
    coef[1] <- c_bt
    p[1] <- p_bt    
  }
  if (age_group != "old")
  {
    if (length(c_bv) > 1)
    {
      coef[2] <- c_bv
      p[2] <- p_bv  
    }
    if (length(c_tv) > 1)
    {
      coef[3] <- c_tv
      p[3] <- p_tv   
    }
  }
  results <- list(coef, p)
  return(results)
}


boot_CI <- function(data, indices, PI_name)
{
  data = data[indices, which(names(data) %in% c("BA", "PERMTH_EXM", "MORTSTAT", "weights"))]
  x <- data[, which(names(data) == "BA")]
  c <- concordance.index(x = x, surv.time= data$PERMTH_EXM, surv.event= data$MORTSTAT, weights = data$weights)$c.index
  return(c)
}


normalize <- function(v)
{
  return((v-mean(v))/sd(v))
}

#take two dataframes, convert their content into strings and paste them
merge_text <- function(A, B){
  RN <- rownames(A)
  CN <- colnames(A)
  C <- initiate_store(RN, CN)
  for (i in seq(length(RN)))
  {
    for (j in seq(length(CN)))
    {
      C[i,j] <- paste(as.character(format(round(A[i,j], digits = 3), nsmall = 3)), as.character(format(round(B[i,j], digits = 3), nsmall = 3)), sep = "+-")
    }
  }
  return(C)
}

#weighted scaling.
weighted_scaling <- function(data, vec_means, vec_sds)
{
  return(t((t(data)-vec_means)/vec_sds))
}

#path
create_path <- function(folder1="", folder2="", folder3="", folder4="", folder5="", folder6="", folder7="", folder8="", folder9="", folder10="", folder11="", folder12="")
{
  args = c(folder1, folder2, folder3, folder4, folder5, folder6, folder7, folder8, folder9, folder10, folder11, folder12)
  args <- args[which(args != "")]
  created_path <- args[1]
  for (arg in args[-1])
  {
    created_path <- paste(created_path, arg, sep = "/")
  }
  return(created_path)
}

