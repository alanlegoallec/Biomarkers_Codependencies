#Helpers square

#ifelse(length(args) > 0, machine <- args[1], machine <- "local")
#ifelse(length(args) > 0, n_cores <- as.numeric(args[2]), n_cores <- 1)

preprocessing_done = TRUE

n_lambdas <- 100
n_folds_CV <- 10
R_boot <- 1000
R_boot_coefficients <- 100
R_boot_R2 = 1000
R_boot_correlations <- 1000
r_boot_hc <- 1000
bins_Coefficients <- "maxdata"
#bins_Coefficients <- "equalbins"
#bins_R2s <- "maxdata"
bins_R2s <- "equalbins"
bins_Correlations <- "maxdata"
#bins_Correlations <- "equalbins"

#demographics_correctors <- c("RIDAGEYR", "female", "hispanic", "black")
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

if (preprocessing_done)
{
  biomarkers <- redRDS("Square/Data/Preprocessing/biomarkers")
  labels <- redRDS("Square/Data/Preprocessing/labels")
  n_predictors <- length(biomarkers) + length(demographics_correctors) - 1
  n_samples_sw <- redRDS("Square/Data/Preprocessing/n_samples_sw")
  n_samples_equalbins_full <- redRDS("Square/Data/Preprocessing/n_samples_equalbins")
  #load labels files
  biomarkers_hc <- redRDS("Square/Data/Preprocessing/biomarkers_hc", "all")
  labels_hc <- redRDS("Square/Data/Preprocessing/labels_hc", "all")
  Dictionnary_names_labels <- redRDS("Square/Data/Preprocessing/Dictionnary_names_labels")
  square_names_hc <- redRDS("Square/Data/Preprocessing/square_names_hc")
  square_labels_hc <- redRDS("Square/Data/Preprocessing/square_labels_hc")
  melted_names_hc <- redRDS("Square/Data/Preprocessing/melted_names_hc")
  melted_labels_hc <- redRDS("Square/Data/Preprocessing/melted_labels_hc")
  concatenated_names_target_hc <- redRDS("Square/Data/Preprocessing/concatenated_names_target_hc")
  concatenated_labels_target_hc <- redRDS("Square/Data/Preprocessing/concatenated_labels_target_hc")
  concatenated_names_predictor_hc <- redRDS("Square/Data/Preprocessing/concatenated_names_predictor_hc")
  concatenated_labels_predictor_hc <- redRDS("Square/Data/Preprocessing/concatenated_labels_predictor_hc")
  #load sliding window parameters
  Age_ranges <- redRDS("Square/Data/Preprocessing/Age_ranges")
  Age_means <- redRDS("Square/Data/Preprocessing/Age_means")
  age_ranges <- Age_ranges[["all"]]
  age_means <- Age_means[["all"]]
  Age_ranges_maxdata <- redRDS("Square/Data/Preprocessing/Age_ranges_maxdata")
  Age_means_maxdata <- redRDS("Square/Data/Preprocessing/Age_means_maxdata")
  Age_ranges_equalbins <- redRDS("Square/Data/Preprocessing/Age_ranges_equalbins")
  Age_means_equalbins <- redRDS("Square/Data/Preprocessing/Age_means_equalbins")
  Age_ranges_Coefficients <- get(paste("Age_ranges", bins_Coefficients, sep = "_"))
  Age_means_Coefficients <- get(paste("Age_means", bins_Coefficients, sep = "_"))
  Age_ranges_R2s <- get(paste("Age_ranges", bins_R2s, sep = "_"))
  Age_means_R2s <- get(paste("Age_means", bins_R2s, sep = "_"))
  Age_ranges_Correlations <- get(paste("Age_ranges", bins_Correlations, sep = "_"))
  Age_means_Correlations <- get(paste("Age_means", bins_Correlations, sep = "_")) 
  Dictionnary_groups_comps <- redRDS("Square/Data/Preprocessing/Dictionnary_groups_comps")
  Dictionnary_distances_comps <- redRDS("Square/Data/Preprocessing/Dictionnary_distances_comps")
  Dictionnary_groups_comps_sw <- redRDS("Square/Data/Preprocessing/Dictionnary_groups_comps_sw")
}



boot_R2_square <- function(data, indices, k, corrected){
  data = data[indices,]
  y <- data$y
  pred <- data$pred
  w <- data$w
  weighted_mean = sum(y*w)/sum(w)
  SSR = sum(w*(y - pred) ^ 2)
  SST = sum(w * (y - weighted_mean) ^ 2)
  r2 <- (1 - SSR/SST)
  if(corrected)
  {
    n <- length(y)
    r2 <- 1-((1-r2)*(n-1)/(n-k-1))
  }
  return(r2)
}



#sliding window version of the helper functions. To generate only baseline results ("20-80"), modify age_ranges variable accordingly.

#generate scatter plots for differences matrices
scatterplot_correlations_sw <- function(square_1, square_2, name_1, name_2, distance, demo_group, age_range)
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
  # text <- square_labels_hc[upper.tri(square_labels_hc)]
  # n = length(text)
  # data <- data.frame("cor_1" = vector(mode="numeric", length=n), "cor_2" = vector(mode="numeric", length=n), "text" = vector(mode="character", length=n))
  # rownames(data) <- square_names_hc[upper.tri(square_names_hc)]
  # data$cor_1 <- cor_1
  # data$cor_2 <- cor_2
  # data$text <- text
  # names(data) <- c(file_name("cor", name_1), file_name("cor", name_2))
  # p <- ggplot(data, aes(cor_1, cor_2, text = text)) + geom_point(alpha=.5) + geom_abline() + coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) + ggtitle(paste("Correlations comparison, distance = ", group, ", r = ", round(cor_12,3), sep = "")) + xlab(name_1) + ylab(name_2)
  # p
  # ggsav(create_path("Square/Results/Correlations/Plots/Scatterplots", distance, demo_group, "Correlations_comparisons"), distance, demo_group, age_range)
  # if (age_range == "20-80")
  # {
  #   p <- ggplotly(p)
  #   htmlsav(p, create_path("Square/Results/Correlations/Interactive/Scatterplots", distance, "Correlations_comparison"), distance, demo_group, age_range)
  # }
  return(cor_12)
}


generate_predictions_sw <- function(demo_group, age_range)
{
  age_min <- as.numeric(gsub( "-.*$", "", age_range ))
  age_max <- as.numeric(gsub("^.*\\-","", age_range))
  for (target in biomarkers_hc)
  {
    for (side in sides)
    {
      #format data, build the model and save predictions
      #train model
      data <- redRDS("Square/Data/Preprocessing/data_square", demo_group, "train", side)
      data <- data[data$age_factor >= age_min & data$age_factor < age_max,]
      #if equalizing between bins, use the same number of samples for all the age bins. Also use the same numbers for comparisons between demographics.
      if (bins_R2s == "equalbins")
      {
        n_samples_equalbins <- redRDS(file_name("Square/Data/Preprocessing/n_samples_equalbins_train", side))
        ns <- n_samples_equalbins[[demo_group]][age_range]
        data <- data[1:ns,]
      }
      w <- data$weights
      x <- data[, which(names(data) %in% c(biomarkers_hc, demographics_correctors_plus))]
      x <- as.matrix(x[, -which(names(x) == target)])
      y <- data[, which(names(data) == target)]
      model <- cv.glmnet(x = x, y = y, nlambda = n_lambdas, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = parallel_cv, standardize = FALSE, weights = w)
      #predict
      for (half in halves)
      {
        data <- redRDS("Square/Data/Preprocessing/data_square", demo_group, half, side)
        data <- data[data$age_factor >= age_min & data$age_factor < age_max,]
        y <-  data[, which(names(data) == target)]
        w <- data$weights
        x <- data[, which(names(data) %in% c(biomarkers_hc, demographics_correctors_plus))]
        x <- as.matrix(x[, -which(names(x) == target)])
        pred <- as.vector(predict(model, newx = x, s= "lambda.min"))
        data_target <- as.data.frame(cbind(y, pred, w, data$age_factor))
        names(data_target) <- c("y", "pred", "w", "age_factor")
        savRDS(data_target, "Square/Data/Processing/data", target, demo_group, age_range, half, side)
      }
    }
    #merge to generate 12
    for (half in halves)
    {
      data_target_12 <- rbind(redRDS("Square/Data/Processing/data", target, demo_group, age_range, half, "1"), redRDS("Square/Data/Processing/data", target, demo_group, age_range, half, "2"))
      savRDS(data_target_12, "Square/Data/Processing/data", target, demo_group, age_range, half, "12")
    }
  }
}


#generate R2s sliding window
generate_R2_sw <- function(demo_group, age_range)
{
  age_min <- as.numeric(gsub( "-.*$", "", age_range ))
  age_max <- as.numeric(gsub("^.*\\-","", age_range))
  for (half in halves)
  {
    for (side in sidesS)
    {
      for (type in R2s_types)
      {
        R2S <- initiate_store(biomarkers_hc, c(age_range))
        R2Ssd <- initiate_store(biomarkers_hc, c(age_range))
        R2Svar <- initiate_store(biomarkers_hc, c(age_range))
        for (target in biomarkers_hc)
        {
          if(type == "sm")
          {
            data <- redRDS("Square/Data/Processing/data", target, demo_group, "20-80", half, side)
            data <- data[data$age_factor >= age_min,]
            data <- data[data$age_factor < age_max,]
          } else{
            data <- redRDS("Square/Data/Processing/data", target, demo_group, age_range, half, side)
          }
          R2_boot <- boot(data=data, statistic = boot_R2_square, R = R_boot_R2, k = n_predictors, corrected = FALSE)
          R2S[target,] <- R2_boot$t0
          R2Ssd[target,] <- sd(R2_boot$t, na.rm = TRUE)
          R2Svar[target,] <- var(R2_boot$t, na.rm = TRUE)
        }
        savRDS(R2S, "Square/Data/R2s/R2S", type, demo_group, age_range, half, side)
        savRDS(R2Ssd, "Square/Data/R2s/R2Ssd", type, demo_group, age_range, half, side)
        savRDS(R2Svar, "Square/Data/R2s/R2Svar", type, demo_group, age_range, half, side)
        R2S_text <- merge_text(R2S, R2Ssd)
        savRDS(R2S_text, "Square/Data/R2s/R2S_text", type, demo_group, age_range, half, side)
        #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
        #do not generate normed, because bias because of larger number of samples for 20-80, and wider range of samples. use mean instead. Done in a different function. R2s_generate_normed_squares
        #baseline <- redRDS("Square/Data/R2s/R2S", type, demo_group, "20-80", half, side)
        #R2S_norm <- R2S - baseline
        #savRDS(R2S_norm, "Square/Data/R2s/R2S_norm", type, demo_group, age_range, half, side)
      }
    }
  }
}

R2s_generate_normed_squares <- function(half, side)
{
  for (demo_group in demo_groups_sw)
  {
    for (type2 in c("R2S", "R2Svar"))
    {
      for (type in R2s_types)
      {
        mean <- redRDS(paste("Square/Data/R2s", type2, sep ="/"), type, demo_group, "20-80", half, side)*0
        for (age_range in Age_ranges_R2s[[demo_group]][-1])
        {
          mean <- mean + redRDS(paste("Square/Data/R2s", type2, sep ="/"), type, demo_group, age_range, half, side)
        }
        mean <- mean/length(Age_ranges_R2s[[demo_group]][-1])
        for (age_range in Age_ranges_R2s[[demo_group]][-1])
        {
          R2S_normed <- redRDS(paste("Square/Data/R2s", type2, sep ="/"), type, demo_group, age_range, half, side) - mean
          savRDS(R2S_normed, paste("Square/Data/R2s", type2, sep ="/"), "normed", type, demo_group, age_range, half, side)
        }
        R2S_normed <- redRDS(paste("Square/Data/R2s", type2, sep ="/"), type, demo_group, "20-80", half, side)
        savRDS(R2S_normed, paste("Square/Data/R2s", type2, sep ="/"), "normed", type, demo_group, "20-80", half, side)
      }
    }
  }
}


R2s_compile_by_demo_groups <- function(half, side)
{
  for (type3 in c("", "normed"))
  {
    for (type2 in c("R2S", "R2Svar"))
    {
      for (type in R2s_types)
      {
        for (demo_group in demo_groups_sw)
        {
          age_ranges_R2_demo_group <- Age_ranges_R2s[[demo_group]]
          R2S <- initiate_store(biomarkers_hc, age_ranges_R2_demo_group)
          for (age_range in age_ranges_R2_demo_group)
          {
            R2S[, age_range] <- as.vector(redRDS(paste("Square/Data/R2s", type2, sep = "/"), type3, type, demo_group, age_range, half, side))
          }
          #save data
          savRDS(R2S, paste("Square/Data/R2s", type2, sep = "/"), type3, type, demo_group, half, side)
          
        }
        #generate comparisons between groups
        for (comp in groups_comps_sw)
        {
          age_ranges_R2_comp <- Age_ranges_R2s[[comp]]
          R2S <- initiate_store(biomarkers_hc, age_ranges_R2_comp)
          R2S_display <- initiate_store(biomarkers_hc, age_ranges_R2_comp)
          for (age_range in age_ranges_R2_comp)
          {
            for (i in seq(2))
            {
              ic <- as.character(i)
              assign(file_name("R2S", ic), redRDS(paste("Square/Data/R2s", type2, sep = "/"), type3, type, Dictionnary_groups_comps[comp, i], age_range, half, side))
            }
            R2S_display[, age_range] <- R2S_2 - R2S_1
            ifelse(type2 == "R2Svar", R2S[, age_range] <- R2S_2 + R2S_1, R2S[, age_range] <- R2S_2 - R2S_1)
          }
          #save data
          savRDS(R2S, paste("Square/Data/R2s", type2, sep = "/"), type3, type, comp, half, side)
          savRDS(R2S_display, paste("Square/Data/R2s", paste(type2, "display", sep = "_"), sep = "/"), type3, type, comp, half, side)
        }
      }
    }
  }
}

R2s_plot_by_demo_groups <- function(half, side)
{
  for (type in R2s_types)
  {
    for (demo_group in demo_groups_swS)
    {
      #generate interactive heatmaps
      for ( r2s in c("R2S", "R2S_normed"))
      {
        data <- redRDS(create_path("Square/Data/R2s", r2s), type, demo_group, half, side)
        if (r2s == "R2S_normed" ){data <- data - data[, which(names(data) == "20-80")]}
        lim <- max(abs(data), na.rm = TRUE)
        rownames(data) <- biomarkers_labels[rownames(data),"label"]
        melted_data <- melt(as.matrix(data))
        names(melted_data) <- c("Biomarker", "Age_Range", "value")
        #timelapse picture
        p <- ggplot(data = melted_data, aes(Age_Range, Biomarker, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(r2s, type, demo_group, half, side))
        p
        ggsav(create_path("Square/Results/R2s/Plots/Heatmaps/by_demo_groups", half, side, type, r2s, r2s), type, demo_group, half, side)
        #interactive plot
        melted_data$value <- round(melted_data$value, 3)
        p <- ggplot(data = melted_data, aes(Age_Range, Biomarker, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(r2s, type, demo_group, half, side))
        pi <- ggplotly(p)
        htmlsav(pi, create_path("Square/Results/R2s/Interactive/Heatmaps/by_demo_groups", half, side, type, r2s, r2s), type, demo_group, half, side) 
      }
    }
  }
}

R2s_compile_by_age_ranges <- function(half, side)
{
  for (type3 in c("", "normed"))
  {
    for (type2 in c("R2S", "R2Svar"))
    {
      for (type in R2s_types)
      {
        for (i in seq(length(demo_groupS)))
        {
          category <- demographic_categories[i]
          demo_groups <- demo_groupS[[i]]
          comps <- compS[[i]]
          for (age_range in Age_ranges_R2s[[demo_groups[1]]])
          {
            R2S <- initiate_store(biomarkers_hc, c(demo_groups, comps))
            for (demo_group in demo_groups)
            {
              R2S[, demo_group] <- as.vector(redRDS(paste("Square/Data/R2s", type2, sep = "/"), type3, type, demo_group, age_range, half, side))
            }
            R2S_display <- R2S
            for (comp in comps)
            {
              for (j in seq(2))
              {
                jc <- as.character(j)
                assign(file_name("R2S", jc), redRDS(paste("Square/Data/R2s", type2, sep = "/"), type3, type, Dictionnary_groups_comps[comp, j], age_range, half, side))
              }
              R2S_display[, comp] <- R2S_2 - R2S_1
              ifelse(type2 == "R2Svar", R2S[, comp] <- R2S_2 + R2S_1, R2S[, comp] <- R2S_2 - R2S_1)
            }
            #save data
            savRDS(R2S, paste("Square/Data/R2s", type2, sep = "/"), type3, type, category, age_range, half, side)
            savRDS(R2S_display, paste("Square/Data/R2s", paste(type2, "display", sep = "_"), sep = "/"), type3, type, category, age_range, half, side)
          }
        }
      }
    }
  }
}

R2s_plot_by_age_ranges <- function(half, side)
{
  for (type in R2s_types)
  {
    for (r2s in c("R2S", "R2S_normed"))
    {
      for (i in seq(length(demo_groupS)))
      {
        category <- demographic_categories[i]
        demo_and_comp <- demo_and_compS[[i]]
        demo_groups <- demo_groupS[[i]]
        comps <- compS[[i]]
        for (type2 in c("demo_and_comp", "demo_groups", "comps"))
        {
          selected_cols <- get(type2)
          if(is.null(selected_cols)){next}
          #find lims for plotting purposes
          lim <- 0
          for (age_range in Age_ranges_R2s[[demo_groups[1]]][-1])
          {
            data <- redRDS(create_path("Square/Data/R2s", r2s), type, category, age_range, half, side)
            data <- data[, selected_cols]
            lim <- max(lim, max(abs(data), na.rm = TRUE))
          }
          for (age_range in Age_ranges_R2s[[demo_groups[1]]])
          {
            data <- redRDS(create_path("Square/Data/R2s", r2s), type, category, age_range, half, side)
            rownames(data) <- biomarkers_labels[rownames(data),"label"]
            data <- data[, selected_cols]
            type_sub <- sub('_sw', "", type2)
            melted_data <- melt(as.matrix(data))
            names(melted_data) <- c("Biomarker", "Demographic_Group", "value")
            if (age_range == "20-80")
            {
              #timelapse picture
              p <- ggplot(data = melted_data, aes(Demographic_Group, Biomarker, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(r2s, type, type_sub, age_range, half, side))
              p
              ggsav(create_path("Square/Results/R2s/Plots/Heatmaps/by_age_ranges", half, side, type, r2s, category, type_sub, r2s), type, category, type_sub, paste("0", age_range, sep = "-"), half, side)
              #interactive plot
              l <- max(abs(melted_data$value))
              melted_data$value <- round(melted_data$value, 3)
              p <- ggplot(data = melted_data, aes(Demographic_Group, Biomarker, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-l, l), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(r2s, type, type_sub, age_range, half, side))
              pi <- ggplotly(p)
              htmlsav(pi, create_path("Square/Results/R2s/Interactive/Heatmaps/by_age_ranges", half, side, type, r2s, category, r2s), type, category, type_sub, age_range, half, side)
            }
            else
            {
              #timelapse picture
              p <- ggplot(data = melted_data, aes(Demographic_Group, Biomarker, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(r2s, type, type_sub, age_range, half, side))
              p
              ggsav(create_path("Square/Results/R2s/Plots/Heatmaps/by_age_ranges", half, side, type, r2s, category, type_sub, r2s), type, category, type_sub, age_range, half, side)
            }
          }
        }
      }
    }
  }
}

#generate R2 correlations between demographic groups
R2s_plot_correlations <- function(half, side)
{
  for (type2 in c("R2S", "R2Svar"))
  {
    for (i in seq(length(demo_groupS[-1])))
    {
      category <- demographic_categories[-1][i]
      comps <- compS[-1][[i]]
      for (type in R2s_types)
      {
        #save correlations between R2s
        R2s_correlations <- initiate_store(comps, Age_ranges_R2s[[comps[1]]])
        for (age_range in Age_ranges_R2s[[comps[1]]])
        {
          #scatter plot: group comparisons
          R2S <- redRDS(create_path("Square/Data/R2s", type2), type, category, age_range, half, side)
          for (comp in comps)
          {
            for (i in seq(2))
            {
              ic <- as.character(i)
              assign(file_name("name", ic), Dictionnary_groups_comps_sw[comp, i])
              assign(file_name("R2", ic), R2S[,get(file_name("name", ic))])
            }
            c <- cor(R2_1, R2_2)
            R2s_correlations[comp, age_range] <- c
            # p <- ggplot(R2S, aes(R2_1, R2_2, text = labels_hc))
            # p <- p + geom_point(alpha=.5) + geom_abline() + coord_cartesian(xlim = c(-0.1, 1), ylim = c(-0.1, 1)) + ggtitle(paste("R2,", half, ",", side, ":", comp, ", ", age_range, ", Correlation = ", round(c, 4))) + xlab(name_1) + ylab(name_2)
            # p
            # if (age_range == "20-80")
            # {
            #   #time lapse picture
            #   ggsav(file_name(create_path("Square/Results/R2s/Plots/Scatterplots", half, side, comp, type, "R2s_correlations"), type, comp, paste("0", age_range, sep = "-"), half, side))
            #   #interactive plot
            #   pi <- ggplotly(p)
            #   htmlsav(pi, file_name(create_path("Square/Results/R2s/Interactive/Scatterplots", half, side, type, category, "R2s_correlations"), type, comp, age_range, half, side))
            # } else {
            #   ggsav(file_name(create_path("Square/Results/R2s/Plots/Scatterplots", half, side, comp, type, "R2s_correlations"), type, comp, age_range, half, side))
            # }
          }
        }
        R2s_correlations_normed <- R2s_correlations - R2s_correlations[,1]
        savRDS(R2s_correlations, create_path("Square/Data/R2s", type2), "correlations", type, category, half, side)
        savRDS(R2s_correlations_normed, create_path("Square/Data/R2s", type2), "correlations_normed", type, category, half, side)
        
        # #plot as a heatmap
        # #timelapse picture
        # melted_R2 <- melt(as.matrix(R2s_correlations))
        # names(melted_R2) <- c("Demographics_Comparisons", "Age_Range", "value")
        # p <- ggplot(data = melted_R2, aes(Age_Range, Demographics_Comparisons, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = (min(R2s_correlations)+max(R2s_correlations))/2, limit = c(min(R2s_correlations), max(R2s_correlations)), space = "Lab", name="R2s_correlations_comparisons") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name("R2s_correlations", type, category, half, side))
        # p
        # ggsav("Square/Results/R2s/Plots/Scatterplots/Summary/R2s_correlations", type, category, half, side)
        # #interactive plot
        # pi <- ggplotly(p)
        # htmlsav(pi, "Square/Results/R2s/Interactive/Scatterplots/Summary/R2s_correlations", type, category, half, side)
      }
    }
  }
}

#generate gifs for R2s
generate_gifs_R2s <- function(half, side)
{
  for (type in R2s_types)
  {
    #heatmaps
    for (r2s in c("R2S", "R2S_normed"))
    {
      for (i in seq(length(demo_groupS)))
      {
        category <- demographic_categories[i]
        demo_and_comp <- demo_and_compS[[i]]
        demo_groups <- demo_groupS[[i]]
        comps <- compS[[i]]
        for (type2 in c("demo_and_comp", "demo_groups", "comps"))
        {
          if(is.null(get(type2))){next}
          input_path <- file_name(create_path(path, "Square/Results/R2s/Plots/Heatmaps/by_age_ranges", half, side, type, r2s, category, type2, r2s), type, category, type2, "*")
          name_file <- paste(file_name(r2s, type, type2, half, side), "gif", sep = ".")
          output_path <- file_name(create_path(path, "Square/Results/R2s/Videos/Heatmaps", half, side, type, r2s, category, name_file))
          if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
          system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
        }
      }
    }
    #scatterplots
    for (comp in groups_comps_sw)
    {
      input_path <- file_name(create_path(path, "Square/Results/R2s/Plots/Scatterplots", half, side, comp, type, "R2s_correlations"), type, comp, "*")
      name_file <- paste(file_name("R2s_correlations", type, comp, half, side), "gif", sep = ".")
      output_path <- file_name(create_path(path, "Square/Results/R2s/Videos/Scatterplots", half, side, type, name_file))
      if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
      system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
    }
    #volcanos
    for (demo_group in demo_groups_swS)
    {
      input_path <- file_name(create_path(path, "Square/Results/R2s/Plots/Significance/Volcano/Demographics_differences", demo_group, half, side, type, "R2s_significance"), type, demo_group, half, side, "*")
      name_file <- paste(file_name("R2s_significances", type, demo_group, half, side), "gif", sep = ".")
      output_path <- file_name(create_path(path, "Square/Results/R2s/Videos/Significance/Volcano/Demographics_differences", half, side, type, name_file))
      if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
      system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
    }
    #histograms
    for (demo_group in demo_groups_swS)
    {
      for (type2 in c("R2S", "R2Svar"))
      {
        input_path <- file_name(create_path(path, "Square/Results/R2s/Plots/Histograms/histograms", type2, demo_group, type, half, side, "histogram"), type2, demo_group, type, half, side, "*")
        name_file <- paste(file_name("histograms", type2, demo_group, type, half, side), "gif", sep = ".")
        output_path <- file_name(create_path(path, "Square/Results/R2s/Videos/Histograms/histograms", type2, type, half, side, name_file))
        if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
        system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
      }
    }
  }
}

generate_coefficients_sw <- function()
{
  for (demo_group in demo_groups_sw)
  {  
    for (age_range in Age_ranges_Coefficients[[demo_group]])
    {
      Coefficients <- initiate_store(biomarkers_hc, biomarkers_hc)
      Coefficients_var <- initiate_store(biomarkers_hc, biomarkers_hc)
      for (target in biomarkers_hc)
      {
        coefs <- redRDS("Square/Data/Coefficients/Coef", demo_group, target, age_range)
        coefs <- coefs[which(names(coefs) %in% biomarkers_hc)]
        Coefficients[names(coefs), target] <- coefs
        coefs_var <- redRDS("Square/Data/Coefficients/Coef_var", demo_group, target, age_range)
        coefs_var <- coefs_var[which(names(coefs_var) %in% biomarkers_hc)]
        Coefficients_var[names(coefs_var), target] <- coefs_var
      }
      savRDS(Coefficients, "Square/Data/Coefficients/Coefficients", demo_group, age_range)
      savRDS(Coefficients_var, "Square/Data/Coefficients/Coefficients_var", demo_group, age_range)
      #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
      Coefficients_normed <- Coefficients
      if(age_range != "20-80")
      {
        baseline <- redRDS("Square/Data/Coefficients/Coefficients", demo_group, "20-80")
        Coefficients_normed <- Coefficients - baseline
      }
      savRDS(Coefficients_normed, "Square/Data/Coefficients/Coefficients_normed", demo_group, age_range)
      #generate normed var: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
      Coefficients_var_normed <- Coefficients_var
      if(age_range != "20-80")
      {
        baseline <- redRDS("Square/Data/Coefficients/Coefficients_var", demo_group, "20-80")
        Coefficients_var_normed <- Coefficients_var - baseline
      }
      savRDS(Coefficients_var_normed, "Square/Data/Coefficients/Coefficients_var_normed", demo_group, age_range)
    }
  }
  #generate difference matrices
  for (comp in groups_comps_sw)
  {
    for (age_range in Age_ranges_Coefficients[[comp]])
    {
      for (i in seq(2))
      {
        ic <- as.character(i)
        assign(file_name("Coefficients", ic), redRDS("Square/Data/Coefficients/Coefficients", Dictionnary_groups_comps_sw[comp, i], age_range))
        assign(file_name("Coefficients_var", ic), redRDS("Square/Data/Coefficients/Coefficients_var", Dictionnary_groups_comps_sw[comp, i], age_range))
      }
      Coefficients_diff <- Coefficients_2 - Coefficients_1
      Coefficients_var_diff <- Coefficients_var_1 + Coefficients_var_2
      Coefficients_var_display_diff <- Coefficients_var_2 - Coefficients_var_1
      savRDS(Coefficients_diff, "Square/Data/Coefficients/Coefficients", comp, age_range)
      savRDS(Coefficients_var_diff, "Square/Data/Coefficients/Coefficients_var", comp, age_range)
      savRDS(Coefficients_var_display_diff, "Square/Data/Coefficients/Coefficients_var_display", comp, age_range)
      #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
      Coefficients_diff_normed <- Coefficients_diff
      if(age_range != "20-80")
      {
        baseline <- redRDS("Square/Data/Coefficients/Coefficients", comp, "20-80")
        Coefficients_diff_normed <- Coefficients_diff - baseline
      }
      savRDS(Coefficients_diff_normed, "Square/Data/Coefficients/Coefficients_normed", comp, age_range)
      #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
      Coefficients_var_diff_normed <- Coefficients_var_diff
      if(age_range != "20-80")
      {
        baseline <- redRDS("Square/Data/Coefficients/Coefficients_var", comp, "20-80")
        Coefficients_var_diff_normed <- Coefficients_var_diff - baseline
      }
      savRDS(Coefficients_var_diff_normed, "Square/Data/Coefficients/Coefficients_var_normed", comp, age_range)
      #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
      Coefficients_var_display_diff_normed <- Coefficients_var_display_diff
      if(age_range != "20-80")
      {
        baseline <- redRDS("Square/Data/Coefficients/Coefficients_var_display", comp, "20-80")
        Coefficients_var_display_diff_normed <- Coefficients_var_display_diff - baseline
      }
      savRDS(Coefficients_var_display_diff_normed, "Square/Data/Coefficients/Coefficients_var_display_normed", comp, age_range)
    }
  }
}




plot_coefficients_sw <- function(demo_group)
{
  for ( hmc in c("Coefficients", "Coefficients_normed"))
  {
    lim <- 0
    for (age_range in Age_ranges_Coefficients[[demo_group]][-1])
    {
      hm <- redRDS(create_path("Square", "Data", "Coefficients", hmc), demo_group, age_range)
      lim <- max(lim, max(abs(hm)))
    }
    for (age_range in Age_ranges_Coefficients[[demo_group]])
    {
      hm <- redRDS(create_path("Square", "Data", "Coefficients", hmc), demo_group, age_range)
      names(hm) <- biomarkers_labels[names(hm),"label"]
      rownames(hm) <- biomarkers_labels[rownames(hm),"label"]
      melted_hm <- melt(as.matrix(hm))
      names(melted_hm) <- c("Predictor", "Target", "value")
      if (age_range == "20-80")
      {
        #timelapse picture
        p <- ggplot(data = melted_hm, aes(Target, Predictor, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-max(abs(hm)),max(abs(hm))), space = "Lab", name="Coefficients") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, demo_group, age_range))
        p
        ggsav(create_path("Square/Results/Coefficients/Plots/Heatmaps", demo_group, hmc, hmc), demo_group, paste("0", age_range, sep = "-"))
        #interactive plot
        pi <- ggplotly(p)
        htmlsav(pi, create_path("Square/Results/Coefficients/Interactive/Heatmaps", hmc, hmc), demo_group, age_range)
      } else {
        #timelapse picture
        ggplot(data = melted_hm, aes(Target, Predictor, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim,lim), space = "Lab", name="Coefficients") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, demo_group, age_range))
        ggsav(create_path("Square/Results/Coefficients/Plots/Heatmaps", demo_group, hmc, hmc), demo_group, age_range)
      }
    }      
  }
}


#generate gifs for coefficients
generate_gifs_coefficients <- function(demo_group)
{
  #heatmaps
  for ( hmc in c("Coefficients", "Coefficients_normed"))
  {
    input_path <- file_name(create_path(path, "Square/Results/Coefficients/Plots/Heatmaps", demo_group, hmc, hmc), demo_group, "*")
    name_file <- paste(file_name(hmc, demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Coefficients/Videos/Heatmaps", hmc, name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  }
  #scatterplots
  if (demo_group %in% groups_comps_sw)
  {
    input_path <- file_name(create_path(path, "Square/Results/Coefficients/Plots/Scatterplots", demo_group, "Coefficients_correlations"), demo_group, "*")
    name_file <- paste(file_name("Coefficients_correlations", demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Coefficients/Videos/Scatterplots", name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  }
  #volcano
  input_path <- file_name(create_path(path, "Square/Results/Coefficients/Plots/Significance/Volcano/Demographics_differences", demo_group, "Coefficients_significance"), demo_group, "*")
  name_file <- paste(file_name("Coefficients_significances", demo_group), "gif", sep = ".")
  output_path <- file_name(create_path(path, "Square/Results/Coefficients/Videos/Significance/Volcano/Demographics_differences", name_file))
  if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
  system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  #histograms
  for (type2 in c("Coefficients", "Coefficients_var"))
  {
    for (type3 in c("signed", "abs"))
    {
      input_path <- file_name(create_path(path, "Square/Results/Coefficients/Plots/Histograms/histograms", type2, demo_group, type3, "histogram"), type2, demo_group, type3, "*")
      name_file <- paste(file_name("histograms", type2, demo_group, type3), "gif", sep = ".")
      output_path <- file_name(create_path(path, "Square/Results/Coefficients/Videos/Histograms/histograms", type2, type3, name_file))
      if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
      system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
    }
  }
}

#generate comparisons and scatter plots between demographic groups for all age_ranges
generate_correlations_groups_comparisons_sw <- function()
{
  for (distance in distances)
  {
    for (i in seq(2, length(demo_groupS)))
    {
      category <- demographic_categories[i]
      demo_and_comp <- demo_and_compS[[i]]
      demo_groups <- demo_groupS[[i]]
      comps <- compS[[i]]
      age_ranges <- Age_ranges_Correlations[[demo_groups[1]]]
      Comp_cor_groups <- initiate_store(comps, age_ranges)
      Comp_cor_groups_var <- initiate_store(comps, age_ranges)
      for (age_range in age_ranges)
      {
        #load matrices for comparisons
        for (comp in comps)
        {
          for (i in seq(2))
          {
            assign(file_name("name", as.character(i)), Dictionnary_groups_comps[comp, i])
            assign(file_name("square", as.character(i)), redRDS("Square/Data/Correlations/Correlations", distance, Dictionnary_groups_comps[comp, i], age_range))
            assign(file_name("square_var", as.character(i)), redRDS("Square/Data/Correlations/Correlations_var", distance, Dictionnary_groups_comps[comp, i], age_range))
          }
          square_diff <- combine_symmetrical_matrices(square_1, square_2)
          square_var_diff <- square_var_1 + square_var_2
          square_var_display_diff <- combine_symmetrical_matrices(square_var_1, square_var_2)
          savRDS(square_diff, "Square/Data/Correlations/Correlations", distance, comp, age_range)
          savRDS(square_var_diff, "Square/Data/Correlations/Correlations_var", distance, comp, age_range)
          savRDS(square_var_display_diff, "Square/Data/Correlations/Correlations_var_display", distance, comp, age_range)
          Comp_cor_groups[comp, age_range] <- scatterplot_correlations_sw(square_1, square_2, name_1, name_2, distance, comp, age_range)
          Comp_cor_groups_var[comp, age_range] <- scatterplot_correlations_sw(square_var_1, square_var_2, name_1, name_2, distance, comp, age_range)
          #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
          square_diff_normed <- square_diff
          if(age_range != "20-80")
          {
            baseline <- redRDS("Square/Data/Correlations/Correlations", distance, comp, "20-80")
            square_diff_normed <- square_diff - baseline
          }
          savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_normed", distance, comp, age_range)
          #generate normed var: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
          square_diff_normed <- square_var_diff
          if(age_range != "20-80")
          {
            baseline <- redRDS("Square/Data/Correlations/Correlations_var", distance, comp, "20-80")
            square_diff_normed <- square_var_diff - baseline
          }
          savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_var_normed", distance, comp, age_range)
          #generate normed var display: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
          square_diff_normed <- square_var_display_diff
          if(age_range != "20-80")
          {
            baseline <- redRDS("Square/Data/Correlations/Correlations_var_display", distance, comp, "20-80")
            square_diff_normed <- square_var_display_diff - baseline
          }
          savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_var_display_normed", distance, comp, age_range)
        }
      }
      #save summary
      savRDS(Comp_cor_groups, "Square/Data/Correlations/Correlations_correlationsgroups", distance, category)
      #generate and save normed matrix
      Comp_cor_groups_normed <- Comp_cor_groups - Comp_cor_groups[, which(names(Comp_cor_groups) == "20-80")]
      savRDS(Comp_cor_groups_normed, "Square/Data/Correlations/Correlations_correlationsgroups_normed", distance, category)
      #save summary var
      savRDS(Comp_cor_groups_var, "Square/Data/Correlations/Correlations_var_correlationsgroups", distance, category)
      #generate and save normed matrix
      Comp_cor_groups_var_normed <- Comp_cor_groups_var - Comp_cor_groups_var[, which(names(Comp_cor_groups_var) == "20-80")]
      savRDS(Comp_cor_groups_var_normed, "Square/Data/Correlations/Correlations_var_correlationsgroups_normed", distance, category)
      # #plot as heatmap
      # for (comp_cor in c("Comp_cor_groups", "Comp_cor_groups_normed"))
      # {
      #   ccg <- get(comp_cor)
      #   ifelse(grepl("normed", comp_cor), lims <- c(-max(abs(ccg), na.rm = TRUE), 0, max(abs(ccg), na.rm = TRUE)), lims <- c(min(ccg, na.rm = TRUE), (min(ccg, na.rm = TRUE)+max(ccg, na.rm = TRUE))/2, max(ccg, na.rm = TRUE)))
      #   #picture
      #   melted_comp <- melt(as.matrix(ccg))
      #   names(melted_comp) <- c("Group_Comparison", "Age_Range", "value")
      #   p <- ggplot(data = melted_comp, aes(Age_Range, Group_Comparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(paste("Demographic group correlations, distance =", distance))
      #   p
      #   ggsav(create_path("Square/Results/Correlations/Plots/Comparisons/demo_groups_comparisons", category, comp_cor, comp_cor), distance)
      #   #interactive plot
      #   pi <- ggplotly(p)
      #   htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Comparisons/demo_groups_comparisons", category, comp_cor, comp_cor), distance)
      # }
    }
  }
}


#generate comparisons between "distances". 
generate_correlations_distances_comparisons_sw <- function()
{
  for (demo_group in demo_groups_swS)
  {
    age_ranges <- Age_ranges_Correlations[[demo_group]]
    Comp_cor_distances <- initiate_store(distances_comps, age_ranges)
    Comp_cor_distances_var <- initiate_store(distances_comps, age_ranges)
    for (age_range in age_ranges)
    {
      for (comp in distances_comps)
      {
        for (i in seq(2))
        {
          #load matrices for comparisons
          assign(file_name("name", as.character(i)), Dictionnary_distances_comps[comp, i])
          assign(file_name("square", as.character(i)), redRDS("Square/Data/Correlations/Correlations", Dictionnary_distances_comps[comp, i], demo_group, age_range))
          assign(file_name("square_var", as.character(i)), redRDS("Square/Data/Correlations/Correlations_var", Dictionnary_distances_comps[comp, i], demo_group, age_range))
        }
        square_diff <- square_2 - square_1
        square_var_diff <- square_var_2 + square_var_1
        square_var_display_diff <- square_2 - square_1
        savRDS(square_diff, "Square/Data/Correlations/Correlations", comp, demo_group, age_range)
        savRDS(square_var_diff, "Square/Data/Correlations/Correlations_var", comp, demo_group, age_range)
        savRDS(square_var_display_diff, "Square/Data/Correlations/Correlations_var_display", comp, demo_group, age_range)
        Comp_cor_distances[comp, age_range] <- scatterplot_correlations_sw(square_1, square_2, name_1, name_2, comp, demo_group, age_range)
        Comp_cor_distances_var[comp, age_range] <- scatterplot_correlations_sw(square_var_1, square_var_2, name_1, name_2, comp, demo_group, age_range)
        #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
        square_diff_normed <- square_diff
        if(age_range != "20-80")
        {
          baseline <- redRDS("Square/Data/Correlations/Correlations", comp, demo_group, "20-80")
          square_diff_normed <- square_diff - baseline
        }
        savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_normed", comp, demo_group, age_range)
        #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
        square_diff_normed <- square_var_diff
        if(age_range != "20-80")
        {
          baseline <- redRDS("Square/Data/Correlations/Correlations_var", comp, demo_group, "20-80")
          square_diff_normed <- square_diff - baseline
        }
        savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_var_normed", comp, demo_group, age_range)
        #generate normed: if age_range == "20-80", do not normalize, but save unnormalize, for timelapse purpose
        square_diff_normed <- square_var_diff
        if(age_range != "20-80")
        {
          baseline <- redRDS("Square/Data/Correlations/Correlations_var_display", comp, demo_group, "20-80")
          square_diff_normed <- square_diff - baseline
        }
        savRDS(square_diff_normed, "Square/Data/Correlations/Correlations_var_display_normed", comp, demo_group, age_range)
      }
    }
    #save summary
    savRDS(Comp_cor_distances, "Square/Data/Correlations/Correlations_correlationsdistances", demo_group)
    #generate and save normed matrix
    Comp_cor_distances_normed <- Comp_cor_distances - Comp_cor_distances[, which(names(Comp_cor_distances) == "20-80")]
    savRDS(Comp_cor_distances_normed, "Square/Data/Correlations/Correlations_correlationsdistances_normed", demo_group)
    #save summary var
    savRDS(Comp_cor_distances_var, "Square/Data/Correlations/Correlations_var_correlationsdistances", demo_group)
    #generate and save normed matrix
    Comp_cor_distances_var_normed <- Comp_cor_distances_var - Comp_cor_distances_var[, which(names(Comp_cor_distances_var) == "20-80")]
    savRDS(Comp_cor_distances_var_normed, "Square/Data/Correlations/Correlations_var_correlationsdistances_normed", demo_group)
    # #plot as heatmap
    # for (comp_cor in c("Comp_cor_distances", "Comp_cor_distances_normed"))
    # {
    #   ccg <- get(comp_cor)
    #   ifelse(grepl("normed", comp_cor), lims <- c(-max(abs(ccg), na.rm = TRUE), 0, max(abs(ccg), na.rm = TRUE)), lims <- c(min(ccg, na.rm = TRUE), (min(ccg, na.rm = TRUE)+max(ccg, na.rm = TRUE))/2, max(ccg, na.rm = TRUE)))
    #   #picture
    #   melted_comp <- melt(as.matrix(ccg))
    #   names(melted_comp) <- c("DistanceComparison", "AgeRange", "value")
    #   p <- ggplot(data = melted_comp, aes(AgeRange, DistanceComparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(paste("Distances correlations, demographic group =", demo_group))
    #   p
    #   ggsav(create_path("Square/Results/Correlations/Plots/Comparisons/distances_comparisons", comp_cor, comp_cor), demo_group)
    #   #interactive plot
    #   pi <- ggplotly(p)
    #   htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Comparisons/distances_comparisons", comp_cor, comp_cor), demo_group)
    # }
  }
}


#generate correlations plots
generate_correlations_plots_sw <- function(distance, demo_group)
{
  for (hmc in c("Correlations", "Correlations_normed"))
  {
    #find lim for for normed pictures for time lapse
    lim <- 0
    for (age_range in Age_ranges_Correlations[[demo_group]][-1])
    {
      square <- redRDS(create_path("Square/Data/Correlations", hmc), distance, demo_group, age_range)
      lim <- max(lim, max(abs(square), na.rm = TRUE))
    }
    for (age_range in Age_ranges_Correlations[[demo_group]])
    {
      square <- redRDS(create_path("Square/Data/Correlations", hmc), distance, demo_group, age_range)
      names(square) <- biomarkers_labels[names(square),"label"]
      rownames(square) <- biomarkers_labels[rownames(square),"label"]
      melted_square <- melt(as.matrix(square))
      names(melted_square) <- c("Biomarker1", "Biomarker2", "value")
      if (age_range == "20-80")
      {
        #timelapse picture
        p <- ggplot(data = melted_square, aes(Biomarker1, Biomarker2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, demo_group, age_range))
        p
        ggsav(create_path("Square/Results/Correlations/Plots/Heatmaps", distance, demo_group, hmc, hmc), distance, demo_group, paste("0", age_range, sep = "-"))
        #interactive plot
        pi <- ggplotly(p)
        htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Heatmaps", distance, hmc, hmc), distance, demo_group, age_range)
      } else {
        #timelapse picture
        p <- ggplot(data = melted_square, aes(Biomarker1, Biomarker2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim,lim), space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, demo_group, age_range))
        p
        ggsav(create_path("Square/Results/Correlations/Plots/Heatmaps", distance, demo_group, hmc, hmc), distance, demo_group, age_range)
      }
    }
  }
}

#generate gifs for correlations
generate_gifs_correlations <- function(distance, demo_group)
{
  #heatmaps
  for (hmc in c("Correlations", "Correlations_normed"))
  {
    input_path <- file_name(create_path(path, "Square/Results/Correlations/Plots/Heatmaps", distance, demo_group, hmc, hmc), distance, demo_group, "*")
    name_file <- paste(file_name(hmc, distance, demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Correlations/Videos/Heatmaps", distance, hmc, name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  }
  #scatterplots
  #comparisons between demographic_groups
  if(demo_group %in% groups_comps_sw)
  {
    input_path <- file_name(create_path(path, "Square/Results/Correlations/Plots/Scatterplots", distance, demo_group, "Correlations_comparisons"), distance, demo_group, "*")
    name_file <- paste(file_name("Correlations_comparisons", distance, demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Correlations/Videos/Scatterplots/between_demo_groups", distance, name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  }
  #comparisons between distances
  if(distance %in% distances_comps)
  {
    input_path <- file_name(create_path(path, "Square/Results/Correlations/Plots/Scatterplots", distance, demo_group, "Correlations_comparisons"), distance, demo_group, "*")
    name_file <- paste(file_name("Correlations_comparisons", distance, demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Correlations/Videos/Scatterplots/between_distances", demo_group, name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  }
  #volcano
  input_path <- file_name(create_path(path, "Square/Results/Correlations/Plots/Significance/Volcano/Demographics_differences", distance, demo_group, "Correlations_significance"), distance, demo_group, "*")
  name_file <- paste(file_name("Correlations_significances", distance, demo_group), "gif", sep = ".")
  output_path <- file_name(create_path(path, "Square/Results/Correlations/Videos/Significance/Volcano/Demographics_differences", distance, name_file))
  if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
  system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
  #histograms
  for (type2 in c("Correlations", "Correlations_var"))
  {
    for (type3 in c("signed", "abs"))
    {
    input_path <- file_name(create_path(path, "Square/Results/Correlations/Plots/Histograms/histograms", type2, distance, demo_group, type3, "histogram"), type2, distance, demo_group, type3, "*")
    name_file <- paste(file_name("histograms", type2, distance, demo_group), "gif", sep = ".")
    output_path <- file_name(create_path(path, "Square/Results/Correlations/Videos/Histograms/histograms", type2, distance, type3, name_file))
    if (!dir.exists(dirname(output_path))){dir.create(dirname(output_path), recursive = TRUE)}
    system(command= paste("convert", input_path, "-delay 100",  "-loop 0", output_path, sep = " "))
    }
  }
}

#generate square correlation matrices: biomarkers and coefficients
generate_coefficients_and_correlations_sw <- function(demo_group, age_range)
{
  age_min <- as.numeric(gsub( "-.*$", "", age_range ))
  age_max <- as.numeric(gsub("^.*\\-","", age_range))
  data <- redRDS("Square/Data/Preprocessing/data_square", demo_group)
  data <- data[data$age_factor >= age_min,]
  data <- data[data$age_factor < age_max,]
  #if equalizing between bins, use the same number of samples for all the age bins. Also use the same numbers for comparisons between demographics.
  if (bins_Coefficients == "equalbins" | bins_Correlations == "equalbins")
  {
    n_samples_equalbins <- redRDS("Square/Data/Preprocessing/n_samples_equalbins")
    ns <- n_samples_equalbins[[demo_group]][age_range]
    data <- data[1:ns,]
  }
  data <- data[,-which(names(data) %in% c("SEQN", "age_factor", "RIDAGEYR"))]
  #for each variable, build model based on all the other variables
  w <- unlist(data$weights)
  #initiate store for residuals
  data_res <- data
  for (target in biomarkers_hc)
  {
    print(target)
    #boostrap to obtain the variance and p-values for the coefficients
    boot_stat <- boot(data = data, statistic = boot_coefficients, R = R_boot_coefficients, parallel = parallel_boot, ncpus=n_cores, target = target)
    coefs <- boot_stat$t0[-1]
    coefs_var <- apply(boot_stat$t, 2, var, na.rm = TRUE)[-1]
    names(coefs_var) <- names(coefs)
    #save coefficient
    savRDS(coefs, "Square/Data/Coefficients/Coef", demo_group, target, age_range)
    savRDS(coefs_var, "Square/Data/Coefficients/Coef_var", demo_group, target, age_range)
    #build the model without boostrapping, to generate the residuals
    x <- as.matrix(data[, -which(names(data) %in% c(target, "weights"))])
    y <- data[, which(names(data) == target)]
    model <- cv.glmnet(x = x, y = y, nlambda = n_lambdas, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = parallel_cv, standardize = FALSE, weights = w)
    #store residuals
    #pred <- as.vector(predict(model, newx = x, weights = w, s= "lambda.min"))
    #data_res[, which(names(data_res) == target)] <- data[, which(names(data) == target)] - pred
  }
  #generate correlation squares
  Correlations_biomarkers <- data.frame(cov.wt(data[,-which(names(data) %in% c("SEQN", "weights", demographics_correctors_plus))], wt = w, cor = TRUE, center = TRUE)$cor)
  Correlations_var_biomarkers <- correlations_weighted_boostrapped(data)
  #Correlations_residuals <- data.frame(cov.wt(data_res[,-which(names(data_res) %in% c("SEQN", "weights", demographics_correctors_plus))], wt = w, cor = TRUE, center = TRUE)$cor)
  #Correlations_var_residuals <- correlations_weighted_boostrapped(data_res)
  builded_square <- build_square_sw(demo_group, age_range)
  Correlations_coefficients <- builded_square[[1]]
  Correlations_var_coefficients <- builded_square[[2]]
  #reorganize squares based on clusters and save them
  for (distance in distances)
  {
    sq <- get(file_name("Correlations", distance))
    sq <- sq[biomarkers_hc, biomarkers_hc]
    savRDS(sq, "Square/Data/Correlations/Correlations", distance, demo_group, age_range)
    #generate normed
    baseline <- redRDS("Square/Data/Correlations/Correlations", distance, demo_group, "20-80")
    sq_normed <- combine_symmetrical_matrices(baseline, sq)
    savRDS(sq_normed, "Square/Data/Correlations/Correlations_normed", distance, demo_group, age_range)
    #save variance
    sq <- get(file_name("Correlations_var", distance))
    sq <- sq[biomarkers_hc, biomarkers_hc]
    savRDS(sq, "Square/Data/Correlations/Correlations_var", distance, demo_group, age_range)
    savRDS(sq, "Square/Data/Correlations/Correlations_var_display", distance, demo_group, age_range)
    #generate normed
    baseline <- redRDS("Square/Data/Correlations/Correlations_var", distance, demo_group, "20-80")
    sq_normed <- combine_symmetrical_matrices(baseline, sq)
    savRDS(sq_normed, "Square/Data/Correlations/Correlations_var_normed", distance, demo_group, age_range)
    savRDS(sq_normed, "Square/Data/Correlations/Correlations_var_display_normed", distance, demo_group, age_range)
  }
}

#define aging boostrapping functions
boot_coefficients <- function(data, indices, target)
{
  data <- data[indices,]
  x <- as.matrix(data[, -which(names(data) %in% c(target, "weights"))])
  y <- data[, target]
  w <- unlist(data$weights)
  model <- tryCatch(model <- cv.glmnet(x = x, y = y, nlambda = n_lambdas, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = parallel_cv, standardize = FALSE, weights = w), error=function(err) NA)
  if(is.na(model))
  {
    coefs <- numeric(ncol(x)+1)
    names(coefs) <- c("(Intercept)", names(data))[-which(c("(Intercept)", names(data)) %in% c(target, "weights"))]
    return(coefs)
  }
  coefs <- as.vector(coef(model, s= "lambda.min"))
  names(coefs) <- rownames(coef(model, s= "lambda.min"))
  return(coefs)
}

build_square_sw <- function(demo_group, age_range)
{
  square <- initiate_store(biomarkers_hc, biomarkers_hc)
  square_var <- initiate_store(biomarkers_hc, biomarkers_hc)
  for (i in seq(length(biomarkers_hc)-1))
  {
    name1 <- biomarkers_hc[i]
    coef1 <- redRDS("Square/Data/Coefficients/Coef", demo_group, name1, age_range)
    for (j in seq(i+1,length(biomarkers_hc)))
    {
      name2 <- biomarkers_hc[j]
      coef2 <- redRDS("Square/Data/Coefficients/Coef", demo_group, name2, age_range)
      coef12 <- coef1[-which(names(coef1) %in% c(name2, demographics_correctors))]
      coef21 <- coef2[-which(names(coef2) %in% c(name1, demographics_correctors))]
      data <- data.frame(cbind(coef12, coef21))
      boot_stat <- boot(data = data, statistic = boot_significance_correlations_coefficients, R = R_boot_correlations, parallel = parallel_boot, ncpus=n_cores)
      cor_pair <- boot_stat$t0
      cor_var <- var(boot_stat$t, na.rm = TRUE, use = "na.or.complete")
      if(is.na(cor_pair)){cor_pair <- 0}
      if(is.na(cor_var)){cor_var <- 4} #4 is the maximal value that coulb be obtained, (+1 - (-1))**2
      square[name1, name2] <- cor_pair
      square[name2, name1] <- cor_pair
      square_var[name1, name2] <- cor_var
      square_var[name2, name1] <- cor_var
    }
  }
#  square_var <- square_var/n_samples_sw[demo_group, age_range]
  return(list(square, square_var))
}


#define square correlations distance coefficient boostrapping functions
boot_significance_correlations_coefficients <- function(data, indices)
{
  data = data[indices,]
  c <- cor(data$coef12, data$coef21)
  if(is.na(c)){c <- 0}
  return(c)
}


#combine two symmetrical matrices: merge upper and lower diag
combine_symmetrical_matrices <- function(m1, m2)
{
  m <- m1
  m[!upper.tri(m)] <- m2[!upper.tri(m2)] - m1[!upper.tri(m1)]
  return(m)
}

#flatten correlations data over age
flatten_videos_correlations <- function(distance, demo_group)
{
  ifelse(grepl("vs", demo_group) | grepl("VS", distance), types <- c("Correlations", "Correlations_var", "Correlations_var_display"), types <- c("Correlations", "Correlations_var"))
  for (hmc in types)
  {
    #initiate flat data store
    age_ranges <- Age_ranges_Correlations[[demo_group]]
    flat <- initiate_store(melted_labels_hc, age_ranges)
    for (age_range in age_ranges)
    {
      square <- data.frame(redRDS(create_path("Square/Data/Correlations", hmc), distance, demo_group, age_range))
      flat[, age_range] <- square[lower.tri(square)]
    }
    flat_normed <- flat - flat[, "20-80"]
    #save data
    savRDS(flat, create_path("Square/Data/Correlations", hmc), "flat", distance, demo_group)
    savRDS(flat_normed, create_path("Square/Data/Correlations", hmc), "normed", "flat", distance, demo_group)
  }
}

#flatten coefficients data over age (in both directions: grouping by target, and grouping by predictor)
flatten_videos_coefficients <- function(demo_group)
{
  age_ranges <- Age_ranges_Coefficients[[demo_group]]
  ifelse(grepl("vs", demo_group), types <- c("Coefficients", "Coefficients_var", "Coefficients_var_display"), types <- c("Coefficients", "Coefficients_var"))
  for (type in types)
  {
    #initiate flat data store
    flat_target <- initiate_store(concatenated_labels_target_hc, age_ranges)
    flat_predictor <- initiate_store(concatenated_labels_predictor_hc, age_ranges)
    for (age_range in age_ranges)
    {
      coefficients <- data.frame(redRDS(create_path("Square/Data/Coefficients", type), demo_group, age_range))
      flat_target[, age_range] <- concatenate_coefficients(coefficients)
      flat_predictor[, age_range] <- concatenate_coefficients(t(coefficients))
    }
    flat_target_normed <- flat_target - flat_target[, "20-80"]
    flat_predictor_normed <- flat_predictor - flat_predictor[, "20-80"]
    #save data
    savRDS(flat_target, create_path("Square/Data/Coefficients", type), "flat", "target", demo_group)
    savRDS(flat_target_normed, create_path("Square/Data/Coefficients", type), "normed", "flat", "target", demo_group)
    savRDS(flat_predictor, create_path("Square/Data/Coefficients", type), "flat", "predictor", demo_group)
    savRDS(flat_predictor_normed, create_path("Square/Data/Coefficients", type), "normed", "flat", "predictor", demo_group)
  }
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

#plot flat correlations as heatmaps
plot_flat_correlations_heatmaps_sw <- function(distance, demo_group)
{
  for (hmc in c("Correlations", "Correlations_normed"))
  {
    #load data
    data <- redRDS(create_path("Square/Data/Correlations", hmc), "flat", distance, demo_group)
    lim <- max(abs(data), na.rm = TRUE)
    melted_data <- melt(as.matrix(data))
    names(melted_data) <- c("Biomarkers", "Age_Range", "value")
    #picture
    p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Cor") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 1, hjust = 1), axis.text.y = element_text(vjust = 1, size = 1, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, distance, demo_group))
    p
    ggsavpdf(100, create_path("Square/Results/Correlations/Plots/Flat", distance, hmc, hmc), "flat", distance, demo_group)
    #interactive plot
    melted_data$value <- round(melted_data$value, 3)
    p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Cor") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 1, hjust = 1), axis.text.y = element_text(vjust = 1, size = 1, hjust = 1)) + coord_fixed() + ggtitle(file_name(hmc, distance, demo_group))
    pi <- ggplotly(p)
    htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Flat", distance, hmc, hmc), "flat", distance, demo_group)
  }
}

#compute variance of weighted correlations
correlations_weighted_boostrapped <- function(data)
{
  boot_stat <- boot(data = data, statistic = boot_significance_correlations, R = R_boot_correlations, parallel = parallel_boot, ncpus=n_cores)
  melted_cor <- boot_stat$t0
  square <- data.frame(matrix(melted_cor, length(biomarkers_hc)))
  names(square) <- biomarkers_hc
  rownames(square) <- biomarkers_hc
  melted_cor_var <- apply(boot_stat$t, 2, var, na.rm = TRUE)
  square_var <- data.frame(matrix(melted_cor_var, length(biomarkers_hc)))
  names(square_var) <- biomarkers_hc
  rownames(square_var) <- biomarkers_hc
  return(square_var)
}

#define square correlations boostrapping functions
boot_significance_correlations <- function(data, indices)
{
  data = data[indices,]
  w <- data$weights
  data <- data[, -which(names(data) %in% c("weights", "SEQN"))]
  square <- data.frame(cov.wt(data[,-which(names(data) %in% demographics_correctors_plus)], wt = w, cor = TRUE, center = TRUE)$cor)
  square <- square[biomarkers_hc, biomarkers_hc]
  sq <- melt(as.matrix(square))
  sq$value[is.na(sq$value)] <- 0
  return(sq$value)
}


trajectory_coefficients_or_correlations <- function(young, old)
{
  if (old - young >= 0)
  {
    if (young < 0 & old < 0) {
      return("increase_-_-")
    } else if (young < 0 & old >= 0){
      return("increase_-_+")
    } else{
      return("increase_+_+")
    }
  } else {
    if (young < 0 & old < 0) {
      return("decrease_-_-")
    } else if (young >= 0 & old < 0){
      return("decrease_+_-")
    } else{
      return("decrease_+_+")
    }
  }
}


#plot flat Coefficients as heatmaps
plot_flat_coefficients_heatmaps_sw <- function(demo_group)
{
  for (type in c("Coefficients", "Coefficients_normed"))
  {
    for (type2 in c("target", "predictor"))
    {
      #load data
      data <- redRDS(create_path("Square/Data/Coefficients", type), "flat", type2, demo_group)
      lim <- max(abs(data), na.rm = TRUE)
      melted_data <- melt(as.matrix(data))
      names(melted_data) <- c("Biomarkers", "Age_Range", "value")
      #picture
      p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Cor") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 1, hjust = 1), axis.text.y = element_text(vjust = 1, size = 1, hjust = 1)) + coord_fixed() + ggtitle(file_name(type, type2, demo_group))
      p
      ggsavpdf(100, create_path("Square/Results/Coefficients/Plots/Flat", type, type2, type), "flat", type2, demo_group)
      #interactive plot
      melted_data$value <- round(melted_data$value, 3)
      p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Cor") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 1, hjust = 1), axis.text.y = element_text(vjust = 1, size = 1, hjust = 1)) + coord_fixed() + ggtitle(file_name(type, type2, demo_group))
      pi <- ggplotly(p)
      htmlsav(pi, create_path("Square/Results/Coefficients/Interactive/Flat", type, type2, type), "flat", type2, demo_group)
    }
  }
}

#generate scatter plots for differences matrices
scatterplot_coefficients_sw <- function()
{
  for (type in c("Coefficients", "Coefficients_var"))
  {
    for (i in seq(2, length(demo_groupS)))
    {
      category <- demographic_categories[i]
      demo_groups <- demo_groupS[[i]]
      comps <- compS[[i]]
      age_ranges <- Age_ranges_Coefficients[[demo_groups[1]]]
      Comp_coefs_groups <- initiate_store(comps, age_ranges)
      for (age_range in age_ranges)
      {
        for (demo_group in comps)
        {
          for (i in seq(2))
          {
            #load matrices for comparisons
            assign(file_name("name", as.character(i)), Dictionnary_groups_comps[demo_group, i])
            assign(file_name("square", as.character(i)), redRDS(create_path("Square/Data/Coefficients", type), Dictionnary_groups_comps[demo_group, i], age_range))
          }
          #prepare data
          for (side in sides)
          {
            sq <- get(file_name("square", side))
            x <- concatenate_coefficients(sq)
            assign(file_name("coefs", side), x)
          }
          coefs_12 <- cor(coefs_1, coefs_2)
          n = length(concatenated_labels_target_hc)
          data <- data.frame("coefs_1" = vector(mode="numeric", length=n), "coefs_2" = vector(mode="numeric", length=n), "text" = vector(mode="character", length=n))
          rownames(data) <- concatenated_names_target_hc
          data$coefs_1 <- coefs_1
          data$coefs_2 <- coefs_2
          data$text <- concatenated_labels_target_hc
          names(data) <- c(file_name("coefs", name_1), file_name("coefs", name_2), "text")
          # p <- ggplot(data, aes(coefs_1, coefs_2, text = text)) + geom_point(alpha=.5) + geom_abline() + ggtitle(paste("Coefficients comparison, ", demo_group, ", r = ", round(coefs_12,3), sep = "")) + xlab(name_1) + ylab(name_2)
          # p
          # if (age_range == "20-80")
          # {
          #   ggsav(create_path("Square/Results/Coefficients/Plots/Scatterplots", demo_group, "Coefficients_correlations"), demo_group, paste("0", age_range, sep = "-"))
          #   p <- ggplotly(p)
          #   htmlsav(p, "Square/Results/Coefficients/Interactive/Scatterplots/Coefficients_comparison", demo_group, age_range)
          # } else{
          #   ggsav(create_path("Square/Results/Coefficients/Plots/Scatterplots", demo_group, "Coefficients_correlations"), demo_group, age_range)
          # }
          Comp_coefs_groups[demo_group, age_range] <- coefs_12
        }
      }
      #save data
      savRDS(Comp_coefs_groups, file_name(create_path("Square/Data/Coefficients", type), "correlations", category))
      #generate and save normed matrix
      Comp_coefs_groups_normed <- Comp_coefs_groups - Comp_coefs_groups[, which(names(Comp_coefs_groups) == "20-80")]
      savRDS(Comp_coefs_groups_normed, file_name(create_path("Square/Data/Coefficients", type), "correlations_normed", category))
      # #plot as heatmap
      # for (comp_cor in c("Comp_coefs_groups", "Comp_coefs_groups_normed"))
      # {
      #   ccg <- get(comp_cor)
      #   ifelse(grepl("normed", comp_cor), lims <- c(-max(abs(ccg), na.rm = TRUE), 0, max(abs(ccg), na.rm = TRUE)), lims <- c(min(ccg, na.rm = TRUE), (min(ccg, na.rm = TRUE)+max(ccg, na.rm = TRUE))/2, max(ccg, na.rm = TRUE)))
      #   #picture
      #   #picture
      #   melted_comp <- melt(as.matrix(ccg))
      #   names(melted_comp) <- c("GroupsComparison", "AgeRange", "value")
      #   p <- ggplot(data = melted_comp, aes(AgeRange, GroupsComparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed() + ggtitle("Groups coefficients correlations")
      #   p
      #   ggsav(create_path("Square/Results/Coefficients/Plots/Comparisons", category, comp_cor))
      #   #interactive plot
      #   pi <- ggplotly(p)
      #   htmlsav(pi, create_path("Square/Results/Coefficients/Interactive/Comparisons/", category, comp_cor))
      # }
    }
  }
}

coefficients_significances <- function(demo_group)
{
  age_ranges <- Age_ranges_Coefficients[[demo_group]]
  #load data
  Coefficients_full <- redRDS("Square/Data/Coefficients/Coefficients_flat_target", demo_group)
  Coefficients_normed_full <- redRDS("Square/Data/Coefficients/Coefficients_normed_flat_target", demo_group)
  Coefficients_normed_full[,"20-80"] <- Coefficients_full[,"20-80"]
  Coefficients_var_full <- redRDS("Square/Data/Coefficients/Coefficients_var_flat_target", demo_group)
  Coefficients_p_full <- data.frame(2*pnorm(abs(as.matrix(Coefficients_normed_full)), 0, sqrt(as.matrix(Coefficients_var_full)), lower.tail = FALSE))
  names(Coefficients_p_full) <- age_ranges
  savRDS(Coefficients_p_full, "Square/Data/Coefficients/Coefficients_p", demo_group)
  for (age_range in age_ranges)
  {
    Coefficients <- Coefficients_normed_full[,age_range]
    Coefficients_var <- Coefficients_var_full[,age_range]
    Coefficients_p <- Coefficients_p_full[,age_range]
    data <- data.frame(cbind(Coefficients, -log10(Coefficients_p)))
    data$text <- concatenated_labels_target_hc
    names(data) <- c("Coefficient", "nlp", "text")
    rownames(data) <- concatenated_names_target_hc
    if(max(data$nlp == Inf)){data$nlp[which(data$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
    #plot
    ifelse(demo_group %in% groups_comps_sw, xlims <- c(-2.1, 2.1), xlims <- c(-1.2, 1.2))
    if (age_range == "20-80")
    {
      p <- ggplot(data, aes(x=Coefficient, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("Coefficients significance, demographic group = ", demo_group, ", age range = ", age_range, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
      p
      ggsav(create_path("Square/Results/Coefficients/Plots/Significance/Volcano/Demographics_differences", demo_group, "Coefficients_significance"), demo_group, paste("0", age_range, sep = "-"))
      #interactive plot
      pi <- ggplotly(p)
      htmlsav(pi, create_path("Square/Results/Coefficients/Interactive/Significance/Volcano/Demographics_differences/Coefficients_significance"), demo_group, age_range)
    } else{
      p <- ggplot(data, aes(x=Coefficient, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("Coefficients significance, demographic group = ", demo_group, ", age range = ", age_range, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
      p
      ggsav(create_path("Square/Results/Coefficients/Plots/Significance/Volcano/Demographics_differences", demo_group, "Coefficients_significance"), demo_group, age_range)
    }
  }
}

R2s_significances <- function(half, side)
{
  for (type in R2s_types)
  {
    for (demo_group in demo_groups_swS)
    {
      #read data
      Age_ranges_demo <- Age_ranges_R2s[[demo_group]]
      R2S_full <- redRDS("Square/Data/R2s/R2S", type, demo_group, half, side)
      R2S_norm_full <- redRDS("Square/Data/R2s/R2S_normed", type, demo_group, half, side)
      R2S_norm_full[,"20-80"] <- R2S_full[,"20-80"]
      R2Svar_full <- redRDS("Square/Data/R2s/R2Svar", type, demo_group, half, side)
      R2Sp_full <- data.frame(2*pnorm(abs(as.matrix(R2S_norm_full)), 0, sqrt(as.matrix(R2Svar_full)), lower.tail = FALSE))
      names(R2Sp_full) <- Age_ranges_demo
      savRDS(R2Sp_full, "Square/Data/R2s/R2Sp", type, demo_group, half, side)
      for (age_range in Age_ranges_demo)
      {
        R2S <- R2S_norm_full[,age_range]
        R2Svar <- R2Svar_full[,age_range]
        R2Sp <- R2Sp_full[,age_range]
        data <- data.frame(cbind(R2S, -log10(R2Sp)))
        data$text <- labels_hc
        names(data) <- c("R2", "nlp", "text")
        rownames(data) <- biomarkers_hc
        if(max(data$nlp == Inf)){data$nlp[which(data$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
        # #plot
        # ifelse(demo_group %in% groups_comps_sw, xlims <- c(-2.2, 2.2), xlims <- c(-1.2, 1.2))
        # if (age_range == "20-80")
        # {
        #   p <- ggplot(data, aes(x=R2, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("R2s significance, demographics = ", demo_group, ", age range = ", age_range, ", half = ", half, ", side = ", side, sep = "")) + xlab("R2s") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
        #   p
        #   ggsav(create_path("Square/Results/R2s/Plots/Significance/Volcano/Demographics_differences", demo_group, half, side, type, "R2s_significance"), type, demo_group, half, side, paste("0", age_range, sep = "-"))
        #   #interactive plot
        #   pi <- ggplotly(p)
        #   htmlsav(pi, create_path("Square/Results/R2s/Interactive/Significance/Volcano/Demographics_differences", half, side, type, "Correlations_significance"), type, demo_group, half, side, age_range)
        # } else{
        #   p <- ggplot(data, aes(x=R2, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("R2s significance, demographics = ", demo_group, ", age range = ", age_range, ", half = ", half, ", side = ", side, sep = "")) + xlab("R2s") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
        #   p
        #   ggsav(create_path("Square/Results/R2s/Plots/Significance/Volcano/Demographics_differences", demo_group, half, side, type, "R2s_significance"), type, demo_group, half, side, age_range)
        # }
      }
    }
  }
}

Correlations_significances <- function(distance, demo_group)
{
  #save data
  Correlations_full <- redRDS("Square/Data/Correlations/Correlations_flat", distance, demo_group)
  Correlations_normed_full <- redRDS("Square/Data/Correlations/Correlations_normed_flat", distance, demo_group)
  Correlations_normed_full[,"20-80"] <- Correlations_full[,"20-80"]
  Correlations_var_full <- redRDS("Square/Data/Correlations/Correlations_var_flat", distance, demo_group)
  Correlations_p_full <- data.frame(2*pnorm(abs(as.matrix(Correlations_normed_full)), 0, sqrt(as.matrix(Correlations_var_full)), lower.tail = FALSE))
  names(Correlations_p_full) <- Age_ranges_Correlations[[demo_group]]
  savRDS(Correlations_p_full, "Square/Data/Correlations/Correlations_p", distance, demo_group)
  for (age_range in Age_ranges_Correlations[[demo_group]])
  {
    Correlations <- Correlations_normed_full[,age_range]
    Correlations_var <- Correlations_var_full[,age_range]
    Correlations_p <- Correlations_p_full[,age_range]
    data <- data.frame(cbind(Correlations, -log10(Correlations_p)))
    data$text <- melted_labels_hc
    names(data) <- c("Correlation", "nlp", "text")
    rownames(data) <- melted_names_hc
    if(max(data$nlp == Inf)){data$nlp[which(data$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
    # #plot
    # ifelse(demo_group %in% groups_comps_sw, xlims <- c(-2.1, 2.1), xlims <- c(-1.2, 1.2))
    # if (age_range == "20-80")
    # {
    #   p <- ggplot(data, aes(x=Correlation, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("Correlations significance, distance = ", distance, ", demographic group = ", demo_group, sep = "")) + xlab("Correlations") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
    #   p
    #   ggsav(create_path("Square/Results/Correlations/Plots/Significance/Volcano/Demographics_differences", distance, demo_group, "Correlations_significance"), distance, demo_group, paste("0", age_range, sep = "-"))
    #   #interactive plot
    #   pi <- ggplotly(p)
    #   htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Significance/Volcano/Demographics_differences", distance, "Correlations_significance"), distance, demo_group, age_range)
    # } else{
    #   p <- ggplot(data, aes(x=Correlation, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + coord_cartesian(xlim = xlims, ylim = c(-1, 310)) + ggtitle(paste("Correlations significance, distance = ", distance, ", demographic group = ", demo_group, sep = "")) + xlab("Correlations") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
    #   p
    #   ggsav(create_path("Square/Results/Correlations/Plots/Significance/Volcano/Demographics_differences", distance, demo_group, "Correlations_significance"), distance, demo_group, age_range)
    # }
  }
}

#test significance of age changes, for both values and variances, for Coefficients, R2s and Correlations
test_significance_changes_coefficients_sw <- function(demo_group)
{
  #initiate store
  significances <- initiate_store(concatenated_labels_target_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
  significances$trajectory <- as.factor(significances$trajectory)
  levels(significances$trajectory) <- trajectories_coefficients
  significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_coefficients[1]), which(levels(significances$trajectory) == trajectories_coefficients[2]), which(levels(significances$trajectory) == trajectories_coefficients[3]), which(levels(significances$trajectory) == trajectories_coefficients[4]), which(levels(significances$trajectory) == trajectories_coefficients[5]), which(levels(significances$trajectory) == trajectories_coefficients[6])))
  pair_data <- initiate_store(Age_ranges_Coefficients[[demo_group]][-1], c("Coefficient", "Age", "w", "sd"))
  pair_data$Age <- Age_means_Coefficients[[demo_group]][-1]
  #load data
  data <- redRDS("Square/Data/Coefficients/Coefficients_flat_target", demo_group)[,-1]
  data_var <- redRDS("Square/Data/Coefficients/Coefficients_var_flat_target", demo_group)[,-1]
  for (pair in rownames(data))
  {
    print(pair)
    pair_data$Coefficient <- unlist(data[pair,])
    #weight the data points inversely proportionally to the variance of the measure (coefficient)
    if(min(data_var[pair,]) == 0){data_var[pair, which(data_var[pair,] == 0)] <- min(data_var[pair,][data_var[pair,]!=min(data_var[pair,])])}#fix if some of the variance is zero}
    pair_data$var <- unlist(data_var[pair,])
    pair_data$sd <- sqrt(pair_data$var)
    pair_data$w <- 1/pair_data$var
    model <- tryCatch(rma(Coefficient, var, mods =  ~ Age, data=pair_data, control=list(maxiter=10000)), error=function(err) NA)
    if(is.na(model[1]))
    {
      print("Error in the meta-regression, NAs stored.")
      coef <- NA
      pv <- NA
      intercept <- NA
      young <- NA
      old <- NA
      significances[pair, "coef"] <- NA
      significances[pair, "p"] <- NA
      significances[pair, "young"] <- NA
      significances[pair, "old"] <- NA
      significances[pair, "trajectory"] <- NA
    } else {
      coef <- model$beta[2]
      pv <- model$pval[2]
      intercept <- model$beta[1]
      young <- intercept + 20*coef
      old <- intercept + 80*coef
      significances[pair, "coef"] <- coef
      significances[pair, "p"] <- pv
      significances[pair, "young"] <- young
      significances[pair, "old"] <- old
      significances[pair, "trajectory"] <- trajectory_coefficients_or_correlations(young, old)
    }
    # #weighted linear regression plot
    # p <- ggplot(data = pair_data, aes(Age, Coefficient)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=Coefficient-sd, ymax=Coefficient+sd), width=.1) + ggtitle(paste("coefficient", demo_group, pair, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
    # p
    # ggsav(create_path("Square/Results/Coefficients/Plots/Significance/LinearRegressions/Coefficients", demo_group, "Coefficient_significance"), demo_group, pair)
  }
  #correcting p values for multiple testing: number of pair tested
  significances$p <- significances$p*length(significances$p)
  significances$significant <- significances$p < 0.05
  significances$nlp <- -log10(significances$p)
  #get rid of NAs after keeping track of them
  Coefficients_NAs <- rownames(significances)[is.na(significances$coef)]
  savRDS(Coefficients_NAs, "Square/Data/Coefficients/Coefficients_significances_NAs", demo_group)
  significances <- significances[!is.na(significances$coef),]
  significances$text <- paste(rownames(significances), ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
  cols = c("increase_-_-" = "yellow", "increase_-_+" = "red", "increase_+_+" = "orange", "decrease_+_+" = "green", "decrease_+_-" = "blue", "decrease_-_-" = "purple")
  names(cols) <- trajectories_coefficients
  cols <- cols[which(names(cols) %in% names(table(significances$trajectory)[which(table(significances$trajectory) > 0)]))]
  #save data
  savRDS(significances, "Square/Data/Coefficients/Coefficients_significances", demo_group)
  significances_ordered <- significances[order(significances$coef),]
  savRDS(significances_ordered, "Square/Data/Coefficients/Coefficients_significances_ordered", demo_group)
  significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
  savRDS(significances_significant_ordered, "Square/Data/Coefficients/Coefficients_significances_significant_ordered", demo_group)
  table_trajectories <- initiate_store(trajectories_coefficients, c(demo_group))
  table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
  savRDS(table_trajectories, "Square/Data/Coefficients/Coefficients_table_trajectories", demo_group)
  # #plot
  # p <- ggplot(significances, aes(x=coef, y=nlp, color = trajectory, text = text)) + geom_point(alpha=.5) + scale_colour_manual(values = cols) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence coefficients, demographics = ", demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
  # p
  # ggsav(create_path("Square/Results/Coefficients/Plots/Significance/Volcano/Age_differences/Coefficients/Coefficients_significance"), demo_group)
  # #interactive plot
  # pi <- ggplotly(p)
  # htmlsav(pi, file_name("Square/Results/Coefficients/Interactive/Significance/Volcano/Age_differences/Coefficients/Coefficients_significance", demo_group))
  # #save data and generate plots for different subgroups of trajectories
  # for (trajectory in trajectories_coefficients)
  # {
  #   significances_subgroup <- significances[which(significances$trajectory == trajectory),]
  #   significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef),]
  #   savRDS(significances_subgroup_ordered, "Square/Data/Coefficients/Coefficients_significances_ordered", trajectory, demo_group)
  #   significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
  #   savRDS(significances_subgroup_significant_ordered, "Square/Data/Coefficients/Coefficients_significances_significant_ordered", trajectory, demo_group)
  # }
}

test_significance_changes_coefficients_variances_sw <- function(demo_group)
{
  age_ranges <- Age_ranges_Coefficients[[demo_group]]
  age_means <- Age_means_Coefficients[[demo_group]]
  #initiate store
  significances <- initiate_store(concatenated_labels_target_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
  significances$trajectory <- as.factor(significances$trajectory)
  levels(significances$trajectory) <- trajectories_R2s #use trajectories_R2s because like R2s, variances are expected to be only positive
  significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_R2s[1]), which(levels(significances$trajectory) == trajectories_R2s[2])))
  pair_data <- initiate_store(age_ranges[-1], c("Coefficient", "Age"))
  pair_data$Age <- age_means[-1]
  #load data
  ifelse(grepl("vs", demo_group), data <- redRDS("Square/Data/Coefficients/Coefficients_var_display_flat_target", demo_group)[,-1], data <- redRDS("Square/Data/Coefficients/Coefficients_var_flat_target", demo_group)[,-1])
  for (pair in rownames(data))
  {
    pair_data$Coefficient <- unlist(data[pair,])
    model <- lm(Coefficient~Age, data = pair_data)
    coef <- summary(model)$coefficients["Age", 1]
    pv <- summary(model)$coefficients["Age", 4]
    if(is.na(pv)){pv <- 1} #if all coefficients are zero, coef will be zero and pv will be NaN. Replace pv by 1 in this case.
    young <- summary(model)$coefficients["(Intercept)","Estimate"] + 20*summary(model)$coefficients["Age","Estimate"]
    old <- summary(model)$coefficients["(Intercept)","Estimate"] + 80*summary(model)$coefficients["Age","Estimate"]
    significances[pair, "coef"] <- coef
    significances[pair, "p"] <- pv
    significances[pair, "young"] <- young
    significances[pair, "old"] <- old
    ifelse(old - young >= 0, significances[pair, "trajectory"] <- "increase", significances[pair, "trajectory"] <- "decrease")
    # #weighted linear regression plot
    # p <- ggplot(data = pair_data, aes(Age, Coefficient)) + geom_point(alpha=.5) + geom_smooth(method='lm') + ggtitle(paste("coefficient_var", demo_group, pair, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
    # p
    # ggsav(create_path("Square/Results/Coefficients/Plots/Significance/LinearRegressions/Coefficients_var", demo_group, "Coefficients_var_significance"), demo_group, pair)
  }
  #correcting p values for multiple testing: number of pair tested
  significances$p <- significances$p*length(significances$p)
  significances$significant <- significances$p < 0.05
  significances$nlp <- -log10(significances$p)
  significances$text <- paste(rownames(significances), ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
  #save data
  savRDS(significances, "Square/Data/Coefficients/Coefficients_var_significances", demo_group)
  significances_ordered <- significances[order(significances$coef),]
  savRDS(significances_ordered, "Square/Data/Coefficients/Coefficients_var_significances_ordered", demo_group)
  significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
  savRDS(significances_significant_ordered, "Square/Data/Coefficients/Coefficients_var_significances_significant_ordered", demo_group)
  table_trajectories <- initiate_store(trajectories_coefficients, c(demo_group))
  table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
  savRDS(table_trajectories, "Square/Data/Coefficients/Coefficients_var_table_trajectories", demo_group)
  # #plot
  # p <- ggplot(significances, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence coefficients var, demographics = ", demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
  # p
  # ggsav(create_path("Square/Results/Coefficients/Plots/Significance/Volcano/Age_differences/Coefficients_var/Coefficients_var_significance"), demo_group)
  # #interactive plot
  # pi <- ggplotly(p)
  # htmlsav(pi, file_name("Square/Results/Coefficients/Interactive/Significance/Volcano/Age_differences/Coefficients_var/Coefficients_var_significance", demo_group))
  #save data and generate plots for different subgroups of trajectories
  for (trajectory in trajectories_coefficients)
  {
    significances_subgroup <- significances[which(significances$trajectory == trajectory),]
    significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef),]
    savRDS(significances_subgroup_ordered, "Square/Data/Coefficients/Coefficients_var_significances_ordered", trajectory, demo_group)
    significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
    savRDS(significances_subgroup_significant_ordered, "Square/Data/Coefficients/Coefficients_var_significances_significant_ordered", trajectory, demo_group)
  }
}

test_significance_changes_R2s_sw <- function(half, side)
{
  for (demo_group in demo_groups_swS)
  {
    print(demo_group)
    for (type in R2s_types)
    {
      print(type)
      #initiate store
      significances <- initiate_store(biomarkers_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
      significances$trajectory <- as.factor(significances$trajectory)
      levels(significances$trajectory) <- trajectories_R2s
      significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_R2s[1]), which(levels(significances$trajectory) == trajectories_R2s[2])))
      biomarker_data <- initiate_store(Age_ranges_R2s[[demo_group]][-1], c("R2s", "Age", "var"))
      biomarker_data$Age <- Age_means_R2s[[demo_group]][-1]
      #load data
      data <- redRDS("Square/Data/R2s/R2S", type, demo_group, half, side)[,-1]
      data_var <- redRDS("Square/Data/R2s/R2Svar", type, demo_group, half, side)[,-1]
      for (biomarker in biomarkers_hc)
      {
        label <- Dictionnary_names_labels[biomarker, "label"]
        print(label)
        biomarker_data$R2s <- unlist(data[biomarker,])
        #weight the data points inversely proportionally to the variance of the measure (correlation)
        if(min(data_var[biomarker,]) == 0){data_var[biomarker,which(data_var[biomarker,] == 0)] <- min(data_var[biomarker,][data_var[biomarker,]!=min(data_var[biomarker,])])}#fix if some of the variance is zero}
        biomarker_data$var <- unlist(data_var[biomarker,])
        biomarker_data$sd <- sqrt(biomarker_data$var)
        biomarker_data$w <- 1/biomarker_data$var
        model <- tryCatch(rma(R2s, var, mods =  ~ Age, data=biomarker_data, control=list(maxiter=10000)), error=function(err) NA)
        if(is.na(model[1]))
        {
          print("Error in the meta-regression, NAs stored.")
          coef <- NA
          pv <- NA
          intercept <- NA
          young <- NA
          old <- NA
          significances[biomarker, "coef"] <- NA
          significances[biomarker, "p"] <- NA
          significances[biomarker, "young"] <- NA
          significances[biomarker, "old"] <- NA
          significances[biomarker, "trajectory"] <- NA
        } else {
          coef <- model$beta[2]
          pv <- model$pval[2]
          intercept <- model$beta[1]
          young <- intercept + 20*coef
          old <- intercept + 80*coef
          significances[biomarker, "coef"] <- coef
          significances[biomarker, "p"] <- pv
          significances[biomarker, "young"] <- young
          significances[biomarker, "old"] <- old
          ifelse(old - young >= 0, significances[biomarker, "trajectory"] <- "increase", significances[biomarker, "trajectory"] <- "decrease")
        }
        # #weighted linear regression plot
        # p <- ggplot(data = biomarker_data, aes(Age, R2s)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=R2s-sd, ymax=R2s+sd), width=.1) + ggtitle(paste("R2", demo_group, label, half, side, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
        # p
        # ggsav(create_path("Square/Results/R2s/Plots/Significance/LinearRegressions/R2S", demo_group, half, side, type, "R2s_significance"), type, demo_group, half, side, label)
      }
      #correcting p values for multiple testing: number of biomarkers tested
      significances$p <- significances$p*length(significances$p)
      significances$significant <- significances$p < 0.05
      significances$nlp <- -log10(significances$p)
      #get rid of NAs after keeping track of them
      R2s_NAs <- rownames(significances)[is.na(significances$coef)]
      savRDS(R2s_NAs, "Square/Data/R2s/R2S_significances_NAs", demo_group, half, side, type)
      significances$text <- paste(Dictionnary_names_labels[rownames(significances), "label"], ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
      #save data
      savRDS(significances, "Square/Data/R2s/R2S_significances", type, demo_group, half, side)
      significances_ordered <- significances[order(significances$coef),]
      savRDS(significances_ordered, "Square/Data/R2s/R2S_significances_ordered", type, demo_group, half, side)
      significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
      savRDS(significances_significant_ordered, "Square/Data/R2s/R2S_significances_significant_ordered", type, demo_group, half, side)
      table_trajectories <- initiate_store(trajectories_R2s, c(demo_group))
      table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
      savRDS(table_trajectories, "Square/Data/R2s/R2S_table_trajectories", type, demo_group, half, side)
      # #plot
      # p <- ggplot(significances, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence R2s, demographics = ", demo_group, paste(",", half, side, type, sep= " " ), sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
      # p
      # ggsav(create_path("Square/Results/R2s/Plots/Significance/Volcano/Age_differences/R2S", half, side, type, "R2s_significance"), type, demo_group, half, side)
      # #interactive plot
      # pi <- ggplotly(p)
      # htmlsav(pi, create_path("Square/Results/R2s/Interactive/Significance/Volcano/Age_differences/R2S", half, side, type, "R2s_significance"), type, demo_group, half, side)
      #save data and generate plots for different subgroups of trajectories
      for (trajectory in trajectories_R2s)
      {
        significances_subgroup <- significances[which(significances$trajectory == trajectory),]
        significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef, decreasing = ),]
        savRDS(significances_subgroup_ordered, "Square/Data/R2s/R2S_significances_ordered", type, trajectory, demo_group, half, side)
        significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
        savRDS(significances_subgroup_significant_ordered, "Square/Data/R2s/R2S_significances_significant_ordered", type, trajectory, demo_group, half, side)
      }
    }
  }
}

test_significance_changes_R2s_variances_sw <- function(half, side)
{
  for (demo_group in demo_groups_swS)
  {
    for (type in R2s_types)
    {
      #initiate store
      significances <- initiate_store(biomarkers_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
      significances$trajectory <- as.factor(significances$trajectory)
      levels(significances$trajectory) <- trajectories_R2s
      significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_R2s[1]), which(levels(significances$trajectory) == trajectories_R2s[2])))
      biomarker_data <- initiate_store(Age_ranges_R2s[[demo_group]][-1], c("R2s", "Age"))
      biomarker_data$Age <- Age_means_R2s[[demo_group]][-1]
      #load data
      ifelse(grepl("vs", demo_group), data <- redRDS("Square/Data/R2s/R2Svar_display", type, demo_group, half, side)[,-1], data <- redRDS("Square/Data/R2s/R2Svar", type, demo_group, half, side)[,-1])
      for (biomarker in biomarkers_hc)
      {
        label <- Dictionnary_names_labels[biomarker, "label"]
        biomarker_data$R2s <- unlist(data[biomarker,])
        #weight the data points inversely proportionally to the variance of the measure (correlation)
        model <- lm(R2s~Age, data = biomarker_data)
        coef <- summary(model)$coefficients["Age", 1]
        pv <- summary(model)$coefficients["Age", 4]
        young <- summary(model)$coefficients["(Intercept)","Estimate"] + 20*summary(model)$coefficients["Age","Estimate"]
        old <- summary(model)$coefficients["(Intercept)","Estimate"] + 80*summary(model)$coefficients["Age","Estimate"]
        significances[biomarker, "coef"] <- coef
        significances[biomarker, "p"] <- pv
        significances[biomarker, "young"] <- young
        significances[biomarker, "old"] <- old
        ifelse(old - young >= 0, significances[biomarker, "trajectory"] <- "increase", significances[biomarker, "trajectory"] <- "decrease")
        # #weighted linear regression plot
        # p <- ggplot(data = biomarker_data, aes(Age, R2s)) + geom_point(alpha=.5) + geom_smooth(method='lm') + ggtitle(paste("R2", demo_group, label, half, side, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
        # p
        # ggsav(create_path("Square/Results/R2s/Plots/Significance/LinearRegressions/R2Svar", demo_group, half, side, type, "R2Svar_significance"), type, demo_group, half, side, label)
      }
      #correcting p values for multiple testing: number of biomarkers tested
      significances$p <- significances$p*length(significances$p)
      significances$significant <- significances$p < 0.05
      significances$nlp <- -log10(significances$p)
      significances$text <- paste(Dictionnary_names_labels[rownames(significances), "label"], ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
      #save data
      savRDS(significances, "Square/Data/R2s/R2Svar_significances", type, demo_group, half, side)
      significances_ordered <- significances[order(significances$coef),]
      savRDS(significances_ordered, "Square/Data/R2s/R2Svar_significances_ordered", type, demo_group, half, side)
      significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
      savRDS(significances_significant_ordered, "Square/Data/R2s/R2Svar_significances_significant_ordered", type, demo_group, half, side)
      table_trajectories <- initiate_store(trajectories_R2s, c(demo_group))
      table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
      savRDS(table_trajectories, "Square/Data/R2s/R2S_table_trajectories_var", type, demo_group, half, side)
      # #plot
      # p <- ggplot(significances, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence R2Svar, demographics = ", demo_group, paste(",", half, side, type, sep= " " ), sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
      # p
      # ggsav(create_path("Square/Results/R2s/Plots/Significance/Volcano/Age_differences/R2Svar", half, side, type, "R2Svar_significance"), type, demo_group, half, side)
      # #interactive plot
      # pi <- ggplotly(p)
      # htmlsav(pi, create_path("Square/Results/R2s/Interactive/Significance/Volcano/Age_differences/R2Svar", half, side, type, "R2Svar_significance"), type, demo_group, half, side)
      #save data and generate plots for different subgroups of trajectories
      for (trajectory in trajectories_R2s)
      {
        significances_subgroup <- significances[which(significances$trajectory == trajectory),]
        significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef, decreasing = ),]
        savRDS(significances_subgroup_ordered, "Square/Data/R2s/R2Svar_significances_ordered", type, trajectory, demo_group, half, side)
        significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
        savRDS(significances_subgroup_significant_ordered, "Square/Data/R2s/R2Svar_significances_significant_ordered", type, trajectory, demo_group, half, side)
      }
    }
  }
}

test_significance_changes_correlations_sw <- function(distance, demo_group)
{
  #initiate store
  significances <- initiate_store(melted_labels_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
  significances$trajectory <- as.factor(significances$trajectory)
  levels(significances$trajectory) <- trajectories_correlations
  significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_correlations[1]), which(levels(significances$trajectory) == trajectories_correlations[2]), which(levels(significances$trajectory) == trajectories_correlations[3]), which(levels(significances$trajectory) == trajectories_correlations[4]), which(levels(significances$trajectory) == trajectories_correlations[5]), which(levels(significances$trajectory) == trajectories_correlations[6])))
  pair_data <- initiate_store(Age_ranges_Correlations[[demo_group]][-1], c("Biomarkers_correlation", "Age", "w", "sd"))
  pair_data$Age <- Age_means_Correlations[[demo_group]][-1]
  #load data
  data <- redRDS("Square/Data/Correlations/Correlations_flat", distance, demo_group)[,-1]
  data_var <- redRDS("Square/Data/Correlations/Correlations_var_flat", distance, demo_group)[,-1]
  for (pair in melted_labels_hc)
  {
    print(pair)
    pair_data$Biomarkers_correlation <- unlist(data[pair,])
    #weight the data points inversely proportionally to the variance of the measure (correlation)
    if(min(data_var[pair,]) == 0){data_var[pair,which(data_var[pair,] == 0)] <- min(data_var[pair,][data_var[pair,]!=min(data_var[pair,])])}#fix if some of the variance is zero}
    pair_data$var <- unlist(data_var[pair,])
    pair_data$sd <- sqrt(pair_data$var)
    pair_data$w <- 1/pair_data$var
    model <- tryCatch(rma(Biomarkers_correlation, var, mods =  ~ Age, data=pair_data, control=list(maxiter=10000)), error=function(err) NA)
    if(is.na(model[1]))
    {
      print("Error in the meta-regression, NAs stored.")
      coef <- NA
      pv <- NA
      intercept <- NA
      young <- NA
      old <- NA
      significances[pair, "coef"] <- NA
      significances[pair, "p"] <- NA
      significances[pair, "young"] <- NA
      significances[pair, "old"] <- NA
      significances[pair, "trajectory"] <- NA
    } else {
      coef <- model$beta[2]
      pv <- model$pval[2]
      intercept <- model$beta[1]
      young <- intercept + 20*coef
      old <- intercept + 80*coef
      significances[pair, "coef"] <- coef
      significances[pair, "p"] <- pv
      significances[pair, "young"] <- young
      significances[pair, "old"] <- old
      significances[pair, "trajectory"] <- trajectory_coefficients_or_correlations(young, old)
    }
    # #weighted linear regression plot
    # p <- ggplot(data = pair_data, aes(Age, Biomarkers_correlation)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=Biomarkers_correlation-sd, ymax=Biomarkers_correlation+sd), width=.1) + ggtitle(paste("Correlation", distance, demo_group, pair, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
    # p
    # ggsav(create_path("Square/Results/Correlations/Plots/Significance/LinearRegressions/Correlations", distance, demo_group, "Correlations_significance"), distance, demo_group, pair)
  }
  #correcting p values for multiple testing: number of pair tested
  significances$p <- significances$p*length(significances$p)
  significances$significant <- significances$p < 0.05
  significances$nlp <- -log10(significances$p)
  #get rid of NAs after keeping track of them
  Correlations_NAs <- rownames(significances)[is.na(significances$coef)]
  savRDS(Correlations_NAs, "Square/Data/Coefficients/Correlations_significances_NAs", distance, demo_group)
  significances$text <- paste(rownames(significances), ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
  cols = c("increase_-_-" = "yellow", "increase_-_+" = "red", "increase_+_+" = "orange", "decrease_+_+" = "green", "decrease_+_-" = "blue", "decrease_-_-" = "purple")
  names(cols) <- trajectories_correlations
  cols <- cols[which(names(cols) %in% names(table(significances$trajectory)[which(table(significances$trajectory) > 0)]))]
  #save data
  savRDS(significances, "Square/Data/Correlations/Correlations_significances", distance, demo_group)
  significances_ordered <- significances[order(significances$coef),]
  savRDS(significances_ordered, "Square/Data/Correlations/Correlations_significances_ordered", distance, demo_group)
  significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
  savRDS(significances_significant_ordered, "Square/Data/Correlations/Correlations_significances_significant_ordered", distance, demo_group)
  table_trajectories <- initiate_store(trajectories_correlations, c(demo_group))
  table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
  savRDS(table_trajectories, "Square/Data/Correlations/Correlations_table_trajectories", distance, demo_group)
  # #plot
  # p <- ggplot(significances, aes(x=coef, y=nlp, color = trajectory, text = text)) + geom_point(alpha=.5) + scale_colour_manual(values = cols) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence correlations, distance = ", distance, ", demographics = ", demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
  # p
  # ggsav(create_path("Square/Results/Correlations/Plots/Significance/Volcano/Age_differences/Correlations", distance, "Correlations_significance"), distance, demo_group)
  # #interactive plot
  # pi <- ggplotly(p)
  # htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Significance/Volcano/Age_differences/Correlations", distance, "Correlations_significance"), distance, demo_group)
  #save data and generate plots for different subgroups of trajectories
  for (trajectory in trajectories_correlations)
  {
    significances_subgroup <- significances[which(significances$trajectory == trajectory),]
    significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef),]
    savRDS(significances_subgroup_ordered, "Square/Data/Correlations/Correlations_significances_ordered", trajectory, distance, demo_group)
    significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
    savRDS(significances_subgroup_significant_ordered, "Square/Data/Correlations/Correlations_significances_significant_ordered", trajectory, distance, demo_group)
  }
}

test_significance_changes_correlations_variances_sw <- function(distance, demo_group)
{
  #initiate store
  significances <- initiate_store(melted_labels_hc, c("coef", "p", "nlp", "significant", "young", "old", "trajectory", "text"))
  significances$trajectory <- as.factor(significances$trajectory)
  levels(significances$trajectory) <- trajectories_R2s #use trajectories_R2s because like R2s, variances are expected to be only positive
  significances$trajectory <- reorder.factor(significances$trajectory, new.order=c(which(levels(significances$trajectory) == trajectories_R2s[1]), which(levels(significances$trajectory) == trajectories_R2s[2])))
  pair_data <- initiate_store(Age_ranges_Correlations[[demo_group]][-1], c("Biomarkers_correlation", "Age"))
  pair_data$Age <- Age_means_Correlations[[demo_group]][-1]
  #load data
  ifelse(grepl("vs", demo_group) | grepl("VS", distance), data <- redRDS("Square/Data/Correlations/Correlations_var_display_flat", distance, demo_group)[,-1], data <- redRDS("Square/Data/Correlations/Correlations_var_flat", distance, demo_group)[,-1])
  for (pair in melted_labels_hc)
  {
    pair_data$Biomarkers_correlation <- unlist(data[pair,])
    model <- lm(Biomarkers_correlation~Age, data = pair_data)
    coef <- summary(model)$coefficients["Age", 1]
    pv <- summary(model)$coefficients["Age", 4]
    young <- summary(model)$coefficients["(Intercept)","Estimate"] + 20*summary(model)$coefficients["Age","Estimate"]
    old <- summary(model)$coefficients["(Intercept)","Estimate"] + 80*summary(model)$coefficients["Age","Estimate"]
    significances[pair, "coef"] <- coef
    significances[pair, "p"] <- pv
    significances[pair, "young"] <- young
    significances[pair, "old"] <- old
    ifelse(old - young >= 0, significances[pair, "trajectory"] <- "increase", significances[pair, "trajectory"] <- "decrease")
    # #weighted linear regression plot
    # p <- ggplot(data = pair_data, aes(Age, Biomarkers_correlation)) + geom_point(alpha=.5) + geom_smooth(method='lm') + ggtitle(paste("Correlations_var", distance, demo_group, pair, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
    # p
    # ggsav(create_path("Square/Results/Correlations/Plots/Significance/LinearRegressions/Correlations_var", distance, demo_group, "Correlations_var_significance"), distance, demo_group, pair)
  }
  #correcting p values for multiple testing: number of pair tested
  significances$p <- significances$p*length(significances$p)
  significances$significant <- significances$p < 0.05
  significances$nlp <- -log10(significances$p)
  significances$text <- paste(rownames(significances), ", young = ", round(significances$young, 4), ", old = ", round(significances$old, 4), sep = "")
  #save data
  savRDS(significances, "Square/Data/Correlations/Correlations_var_significances", distance, demo_group)
  significances_ordered <- significances[order(significances$coef),]
  savRDS(significances_ordered, "Square/Data/Correlations/Correlations_var_significances_ordered", distance, demo_group)
  significances_significant_ordered <- significances_ordered[which(significances_ordered$significant == TRUE),]
  savRDS(significances_significant_ordered, "Square/Data/Correlations/Correlations_var_significances_significant_ordered", distance, demo_group)
  table_trajectories <- initiate_store(trajectories_correlations, c(demo_group))
  table_trajectories[,] <- data.frame(table(significances_significant_ordered$trajectory))[,2]
  savRDS(table_trajectories, "Square/Data/Correlations/Correlations_var_table_trajectories_var", distance, demo_group)
  # #plot
  # p <- ggplot(significances, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5) + geom_hline(yintercept = -log10(0.05)) + ggtitle(paste("Significance age dependence correlations var, distance = ", distance, ", demographics = ", demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=8))
  # p
  # ggsav(create_path("Square/Results/Correlations/Plots/Significance/Volcano/Age_differences/Correlations_var", distance, "Correlations_var_significance"), distance, demo_group)
  # #interactive plot
  # pi <- ggplotly(p)
  # htmlsav(pi, create_path("Square/Results/Correlations/Interactive/Significance/Volcano/Age_differences/Correlations_var", distance, "Correlations_var_significance"), distance, demo_group)
  #save data and generate plots for different subgroups of trajectories
  for (trajectory in trajectories_correlations)
  {
    significances_subgroup <- significances[which(significances$trajectory == trajectory),]
    significances_subgroup_ordered <- significances_subgroup[order(significances_subgroup$coef),]
    savRDS(significances_subgroup_ordered, "Square/Data/Correlations/Correlations_var_significances_ordered", trajectory, distance, demo_group)
    significances_subgroup_significant_ordered <- significances_subgroup_ordered[which(significances_subgroup_ordered$significant == TRUE),]
    savRDS(significances_subgroup_significant_ordered, "Square/Data/Correlations/Correlations_var_significances_significant_ordered", trajectory, distance, demo_group)
  }
}


sliding_histogram_coefficients <- function(demo_group)
{
  age_ranges <- Age_ranges_Coefficients[[demo_group]]
  age_means <- Age_means_Coefficients[[demo_group]]
  #histo_summary <- initiate_store(c("Coefficients", "Coefficients_var"), c("coef_signed", "nlp_signed", "coef_abs", "nlp_abs"))
  for (type2 in c("Coefficients", "Coefficients_var"))
  {
    for (type3 in c("signed", "abs"))
    {
      for(type4 in c("", "significant"))
      {
        display <- ifelse(type2 == "Coefficients_var" & grepl("vs", demo_group), "display", "")
        data <- redRDS(create_path("Square/Data/Coefficients", type2), display, "flat_target", demo_group)
        if(type3 == "abs"){data <- abs(data)}
        if(type4 == "significant")
        {
          significants <- redRDS(create_path("Square/Data/Coefficients", type2), "significances", demo_group)
          data <- data[rownames(significants[significants$significant & !is.na(significants$significant),]),]
        }
        #calculate trend
        data_histo <- initiate_store(age_ranges[-1], c("mean", "var", "age"))
        if(nrow(data) > 0)
        {
          data_histo$mean <- apply(data[,-1], 2, mean)
          boot_stat <- boot(data = data[,-1], statistic = boot_mean, R = 1000, parallel = parallel_boot, ncpus=n_cores)
          data_histo$var <- apply(boot_stat$t, 2, var)
          data_histo$sd <- sqrt(data_histo$var)
          data_histo$w <- 1/data_histo$var
          data_histo$age <- age_means[-1]
        }
        savRDS(data_histo, "Square/Data/Coefficients/data_histo", type2, type3, type4, demo_group)
        #model <- rma(mean, var, mods =  ~ age, data=data_histo, control=list(maxiter=10000))
        #coef <- model$beta[2]
        #pv <- model$pval[2]
        #histo_summary[type2, paste("coef", type3, sep = "_")] <- coef
        #histo_summary[type2, paste("nlp", type3, sep = "_")] <- -log10(pv)
        # #weighted linear regression plot
        # p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) + ggtitle(paste("Coefficients_histo", type2, demo_group, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
        # p
        # ggsav(file_name(create_path("Square/Results/Coefficients/Plots/Histograms/LinearRegressions", type2, type3, "Histogram_LR"), type2, demo_group, type3))
        # #plot histograms
        # mn <- min(data[,-1])
        # mx <- max(data[,-1])
        # binwidth = (mx - mn)/100
        # for (age_range in age_ranges[-1])
        # {
        #   if (age_range == "20-80")
        #   {
        #     data_a <- data[,age_range, drop = FALSE]
        #     names(data_a) <- "Age_range"
        #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
        #     p
        #     ggsav(file_name(create_path("Square/Results/Coefficients/Plots/Histograms/LinearRegressions", type2, demo_group, type3, "histogram"), type2, demo_group, type3, paste("0-", age_range, sep = "")))
        #   } else{
        #     data_a <- data[,age_range, drop = FALSE]
        #     names(data_a) <- "Age_range"
        #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + coord_cartesian(xlim = c(mn, mx)) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
        #     p
        #     ggsav(file_name(create_path("Square/Results/Coefficients/Plots/Histograms/histograms", type2, demo_group, type3, "histogram"), type2, demo_group, type3, age_range))
        #   }
        # }
      }
    }
  }
  #savRDS(histo_summary, "Square/Data/Coefficients/Histo_summary", demo_group)
  #print(histo_summary)
}

sliding_histogram_R2s <- function(half, side)
{
  #histo_summary <- initiate_store(demo_groups_swS, c("coef_R2S_sm", "nlp_R2S_sm", "coef_R2S_sw", "nlp_R2S_sw", "coef_R2Svar_sm", "nlp_R2Svar_sm", "coef_R2Svar_sw", "nlp_R2Svar_sw"))
  for (demo_group in demo_groups_swS)
  {
    print(demo_group)
    for (type in R2s_types)
    {
      for (type2 in c("R2S", "R2Svar"))
      {
        for(type4 in c("", "significant"))
        {
          display <- ifelse(type2 == "Coefficients_var" & grepl("vs", demo_group), "display", "")
          data <- redRDS(create_path("Square/Data/R2s", type2), display, type, demo_group, half, side)
          if(type4 == "significant")
          {
            significants <- redRDS(create_path("Square/Data/R2s", type2), "significances", type, demo_group, half, side)
            data <- data[rownames(significants[significants$significant & !is.na(significants$significant),]),]
          }
          #calculate trend
          data_histo <- initiate_store(Age_ranges_R2s[[demo_group]][-1], c("mean", "var", "age"))
          if(nrow(data) > 0)
          {
            data_histo$mean <- apply(data[,-1], 2, mean)
            boot_stat <- boot(data = data[,-1], statistic = boot_mean, R = 1000, parallel = parallel_boot, ncpus=n_cores)
            data_histo$var <- apply(boot_stat$t, 2, var)
            data_histo$sd <- sqrt(data_histo$var)
            data_histo$w <- 1/data_histo$var
            data_histo$age <- Age_means_R2s[[demo_group]][-1]
          }
          savRDS(data_histo, "Square/Data/R2s/data_histo", type2, type4, type, demo_group, half, side)
          # model <- rma(mean, var, mods =  ~ age, data=data_histo, control=list(maxiter=10000))
          # coef <- model$beta[2]
          # pv <- model$pval[2] 
          # histo_summary[demo_group, paste("coef", type2, type, sep = "_")] <- coef
          # histo_summary[demo_group, paste("nlp", type2, type, sep = "_")] <- -log10(pv)
          # #weighted linear regression plot
          # p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) + ggtitle(paste("R2s_histo", type2, demo_group, type, half, side, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
          # p
          # ggsav(file_name(create_path("Square/Results/R2s/Plots/Histograms/LinearRegressions", type2, half, side, type, "Histogram_LR"), type2, demo_group, type, half, side))
          # #plot histograms
          # mn <- min(data[,-1])
          # mx <- max(data[,-1])
          # binwidth = (mx - mn)/100
          # for (age_range in Age_ranges_R2s[[demo_group]][-1])
          # {
          #   if (age_range == "20-80")
          #   {
          #     data_a <- data[,age_range, drop = FALSE]
          #     names(data_a) <- "Age_range"
          #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
          #     p
          #     ggsav(file_name(create_path("Square/Results/R2s/Plots/Histograms/LinearRegressions", type2, demo_group, type, half, side, "histogram"), type2, demo_group, type, half, side, paste("0-", age_range, sep = "")))
          #   } else{
          #     data_a <- data[,age_range, drop = FALSE]
          #     names(data_a) <- "Age_range"
          #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + coord_cartesian(xlim = c(mn, mx)) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
          #     p
          #     ggsav(file_name(create_path("Square/Results/R2s/Plots/Histograms/histograms", type2, demo_group, type, half, side, "histogram"), type2, demo_group, type, half, side, age_range))
          #   }
          # }
        }
      }
    }
  }
  #savRDS(histo_summary, "Square/Data/R2s/Histo_summary", half, side)
  #print(histo_summary)
}

sliding_histogram_correlations <- function(distance, demo_group)
{
  #histo_summary <- initiate_store(c("Correlations", "Correlations_var"), c("coef_signed", "nlp_signed", "coef_abs", "nlp_abs"))
  for (type2 in c("Correlations", "Correlations_var"))
  {
    for (type3 in c("signed", "abs"))
    {
      for(type4 in c("", "significant"))
      {
        display <- ifelse(grepl("var", type2) & (grepl("vs", demo_group) | grepl("VS", distance)), "display", "")
        data <- redRDS(create_path("Square/Data/Correlations", type2), display, "flat", distance, demo_group)
        if (type3 == "abs")
        {
          data <- abs(data)
        }
        if (type4 =="significant")
        {
          significants <- redRDS(create_path("Square/Data/Correlations", type2), "significances", distance, demo_group)
          data <- data[rownames(significants[significants$significant & !is.na(significants$significant),]),]
        }
        #calculate trend
        data_histo <- initiate_store(Age_ranges_Correlations[[demo_group]][-1], c("mean", "var", "age"))
        if(nrow(data) > 0)
        {
          data_histo$mean <- apply(data[,-1], 2, mean)
          boot_stat <- boot(data = data[,-1], statistic = boot_mean, R = 1000, parallel = parallel_boot, ncpus=n_cores)
          data_histo$var <- apply(boot_stat$t, 2, var)
          data_histo$sd <- sqrt(data_histo$var)
          data_histo$w <- 1/data_histo$var
          data_histo$age <- Age_means_Correlations[[demo_group]][-1]
        }
        savRDS(data_histo, "Square/Data/Correlations/data_histo", type2, type3, type4, distance, demo_group)
        # model <- rma(mean, var, mods =  ~ age, data=data_histo, control=list(maxiter=10000))
        # coef <- model$beta[2]
        # pv <- model$pval[2]
        #histo_summary[type2, paste("coef", type3, sep = "_")] <- coef
        #histo_summary[type2, paste("nlp", type3, sep = "_")] <- -log10(pv)
        # #weighted linear regression plot
        # p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(alpha=.5) + geom_smooth(method='lm', mapping = aes(weight = w)) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) + ggtitle(paste("Correlations_histo", type2, distance, demo_group, paste("Coef =", round(coef, 4), sep = " "), paste("neg-log-corrected-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=4))
        # p
        # ggsav(file_name(create_path("Square/Results/Correlations/Plots/Histograms/LinearRegressions", type2, type3, "Histogram_LR"), type2, distance, demo_group, type3))
        # #plot histograms
        # mn <- min(data[,-1])
        # mx <- max(data[,-1])
        # binwidth = (mx - mn)/100
        # for (age_range in Age_ranges_Correlations[[demo_group]][-1])
        # {
        #   if (age_range == "20-80")
        #   {
        #     data_a <- data[,age_range, drop = FALSE]
        #     names(data_a) <- "Age_range"
        #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
        #     p
        #     ggsav(file_name(create_path("Square/Results/Correlations/Plots/Histograms/LinearRegressions", type2, distance, demo_group, type3, "histogram"), type2, distance, demo_group, type3, paste("0-", age_range, sep = "")))
        #   } else{
        #     data_a <- data[,age_range, drop = FALSE]
        #     names(data_a) <- "Age_range"
        #     p <- ggplot(data=data_a) + geom_histogram(aes(data_a$Age_range), binwidth = binwidth) + coord_cartesian(xlim = c(mn, mx)) + ggtitle(paste("Age range = ", age_range, ", mean = ", round(data_histo[age_range, "mean"],2), " , sd = ", round(sqrt(data_histo[age_range, "var"]),2))) + theme(plot.title = element_text(size = 8))
        #     p
        #     ggsav(file_name(create_path("Square/Results/Correlations/Plots/Histograms/histograms", type2, distance, demo_group, type3, "histogram"), type2, distance, demo_group, type3, age_range))
        #   }
        # }
      }
    }
  }
  # savRDS(histo_summary, "Square/Data/Correlations/Histo_summary", distance, demo_group)
  # print(histo_summary)
}

#boot mean
boot_mean <- function(data, indices){
  data = data[indices,]
  m <- apply(data, 2, mean)
  return(m)
}


