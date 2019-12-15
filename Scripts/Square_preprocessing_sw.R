#square preprocessing

args <- c("O2", 1)
machine <- "O2"
n_cores <- 1

#path <- "/Users/Alan/Desktop/Aging/"
path <- "/n/groups/patel/Alan/Aging/"
source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))

#get rid of correlated variables and generate list biomarkers
data <- redRDS("Square/Data/Preprocessing/data_square_all")
data <- data[, -which(names(data) %in% c("SEQN", "age_factor", "RIDAGEYR", "female", "hispanic", "black", "other", "weights"))]
biomarkers <- names(data)
cor <- cor(data)
cor[!lower.tri(cor)] <- 0
biomarkers <- biomarkers[!apply(cor,2,function(x) any(x > 0.90))]
savRDS(biomarkers, "Square/Data/Preprocessing/biomarkers")
for (demo_group in demo_groups_extra)
{
  data <- redRDS("Square/Data/Preprocessing/data_square", demo_group)
  data <- data[, which(names(data) %in% c(c("SEQN", "age_factor", "RIDAGEYR", "female", "hispanic", "black", "other", "weights"), biomarkers))]
  savRDS(data, "Square/Data/Preprocessing/data_square", demo_group)
}

#generate labels for unclustered
labels <- biomarkers
for (i in seq(length(labels)))
{
  if(labels[i] %in% tabDesc_clean$var)
  {
    labels[i] <- tabDesc_clean$var_desc[which(tabDesc_clean$var == labels[i])]
  }
}
labels <- gsub("\\s*\\([^\\)]+\\)","", labels)
labels <- gsub(" mm Hg","", labels)
labels <- gsub(" SI","", labels)
labels <- gsub(":","", labels)
savRDS(labels, "Square/Data/Preprocessing/labels")


#cluster biomarkers to make the matrix more readable
for (demo_group in demo_groups_extra)
{
  data <- redRDS("Square/Data/Preprocessing/data_square", demo_group)
  data <- data[, biomarkers]
  d <- dist(t(data))
  hc <- hclust(d)
  par(cex=0.3, mar=c(5, 8, 4, 1))
  #plot_sav(hc, file_name("Square/Results/Preprocessing/Plots/Hierchical_clustering/Hierchical_clustering_labels", demo_group))
  biomarkers_hc <- biomarkers[hc$order]
  savRDS(biomarkers_hc, "Square/Data/Preprocessing/biomarkers_hc", demo_group)
  #generate labels
  labels_hc <- biomarkers_hc
  for (i in seq(length(labels_hc)))
  {
    if(labels_hc[i] %in% tabDesc_clean$var){
      labels_hc[i] <- tabDesc_clean$var_desc[which(tabDesc_clean$var == labels_hc[i])]
    }
  }
  labels_hc <- gsub("\\s*\\([^\\)]+\\)","", labels_hc)
  labels_hc <- gsub(" mm Hg","", labels_hc)
  labels_hc <- gsub(" SI","", labels_hc)
  labels_hc <- gsub(":","", labels_hc)
  savRDS(labels_hc, "Square/Data/Preprocessing/labels_hc", demo_group)
}

#load "all" versions for next steps
biomarkers_hc <- redRDS("Square/Data/Preprocessing/biomarkers_hc", "all")
labels_hc <- redRDS("Square/Data/Preprocessing/labels_hc", "all")

#generate labels for scatter plots
n <- length(biomarkers_hc)
square_names_hc <- matrix("", n, n)
names(square_names_hc) <- biomarkers_hc
rownames(square_names_hc) <- biomarkers_hc
square_labels_hc <- matrix("", n, n)
names(square_labels_hc) <- labels_hc
rownames(square_labels_hc) <- labels_hc
for (i in seq(n))
{
  for (j in seq(n))
  {
    square_names_hc[i,j] <- paste(biomarkers_hc[i], biomarkers_hc[j], sep = " ; ")
    square_labels_hc[i,j] <- paste(labels_hc[i], labels_hc[j], sep = " ; ")
  }
}
savRDS(square_names_hc, "Square/Data/Preprocessing/square_names_hc")
savRDS(square_labels_hc, "Square/Data/Preprocessing/square_labels_hc")


#create names for melted correlations plots
melted_names_hc <- c()
melted_labels_hc <- c()
for (i in seq(length(biomarkers_hc)-1))
{
  for (j in seq(i+1, length(biomarkers_hc)))
  {
    melted_names_hc <- c(melted_names_hc, paste(biomarkers_hc[i], biomarkers_hc[j], sep = "vs"))
    melted_labels_hc <- c(melted_labels_hc, paste(labels_hc[i], labels_hc[j], sep = " VS "))
  }
}
savRDS(melted_names_hc, "Square/Data/Preprocessing/melted_names_hc")
savRDS(melted_labels_hc, "Square/Data/Preprocessing/melted_labels_hc")

#create names for concatenated coefficients plots
concatenated_names_target_hc <- c()
concatenated_labels_target_hc <- c()
concatenated_names_predictor_hc <- c()
concatenated_labels_predictor_hc <- c()
for (i in seq(length(biomarkers_hc)))
{
  for (j in seq(length(biomarkers_hc)))
  {
    if(i != j)
    {
      concatenated_names_target_hc <- c(concatenated_names_target_hc, paste(biomarkers_hc[i], biomarkers_hc[j], sep = ": "))
      concatenated_labels_target_hc <- c(concatenated_labels_target_hc, paste(labels_hc[i], labels_hc[j], sep = ": "))
      concatenated_names_predictor_hc <- c(concatenated_names_predictor_hc, paste(biomarkers_hc[j], biomarkers_hc[i], sep = ": "))
      concatenated_labels_predictor_hc <- c(concatenated_labels_predictor_hc, paste(labels_hc[j], labels_hc[i], sep = ": "))
    }
  }
}
savRDS(concatenated_names_target_hc, "Square/Data/Preprocessing/concatenated_names_target_hc")
savRDS(concatenated_labels_target_hc, "Square/Data/Preprocessing/concatenated_labels_target_hc")
savRDS(concatenated_names_predictor_hc, "Square/Data/Preprocessing/concatenated_names_predictor_hc")
savRDS(concatenated_labels_predictor_hc, "Square/Data/Preprocessing/concatenated_labels_predictor_hc")

#build dictionnaries for comparisonsS
Dictionnary_groups_comps <- data.frame(cbind(c("young", "male", "controlS1", "white", "white", "controlE1"), c("old", "female", "controlS2", "hispanic", "black", "controlE2")))
Dictionnary_groups_comps[] <- lapply( Dictionnary_groups_comps, as.character)
names(Dictionnary_groups_comps) <- c("group1", "group2")
rownames(Dictionnary_groups_comps) <- groups_comps
savRDS(Dictionnary_groups_comps, "Square/Data/Preprocessing/Dictionnary_groups_comps")

Dictionnary_distances_comps <- data.frame(cbind(c("biomarkers"), c("coefficients")))
Dictionnary_distances_comps[] <- lapply( Dictionnary_distances_comps, as.character)
names(Dictionnary_distances_comps) <- c("distance1", "distance2")
rownames(Dictionnary_distances_comps) <- distances_comps
savRDS(Dictionnary_distances_comps, "Square/Data/Preprocessing/Dictionnary_distances_comps")

Dictionnary_groups_comps_sw <- data.frame(cbind(c("male", "controlS1", "white", "white", "controlE1"), c("female", "controlS2", "hispanic", "black", "controlE2")))
Dictionnary_groups_comps_sw[] <- lapply( Dictionnary_groups_comps_sw, as.character)
names(Dictionnary_groups_comps_sw) <- c("group1", "group2")
rownames(Dictionnary_groups_comps_sw) <- groups_comps_sw
savRDS(Dictionnary_groups_comps_sw, "Square/Data/Preprocessing/Dictionnary_groups_comps_sw")


#generating age_ranges: use different age_ranges for R2s for ethnicity and sexes comparisons to have the same number of samples on all age bins, high enough.
data <- redRDS("Square/Data/Preprocessing/data_square_all")
Age_ranges_equalbins <- sapply(demo_groups_swS, function(x) NULL)
Age_means_equalbins <- sapply(demo_groups_swS, function(x) NULL)
Age_ranges_maxdata <- sapply(demo_groups_swS, function(x) NULL)
Age_means_maxdata <- sapply(demo_groups_swS, function(x) NULL)
for (demo_groups in demo_groupS)
{
  for (demo_group in demo_groups)
  {
    window_size <- window_sizes[demo_group, "Window"]
    age_mins <- c(min(data$age_factor), seq(min(data$age_factor), max(data$age_factor) + 1 - window_size, by = window_size))
    age_maxs <- c(max(data$age_factor)+1, seq(min(data$age_factor) + window_size, max(data$age_factor)+1, by = window_size))
    Age_means_equalbins[[demo_group]] <- (age_mins + age_maxs)/2
    Age_ranges_equalbins[[demo_group]] <- paste(age_mins, age_maxs, sep = "-")
    Age_means_maxdata[[demo_group]] <- Age_means_equalbins[["all"]]
    Age_ranges_maxdata[[demo_group]] <- Age_ranges_equalbins[["all"]]
  }
}
for (comp in groups_comps_sw)
{
  Age_means_equalbins[[comp]] <- Age_means_equalbins[[Dictionnary_groups_comps[comp, 1]]]
  Age_ranges_equalbins[[comp]] <- Age_ranges_equalbins[[Dictionnary_groups_comps[comp, 1]]]
  Age_means_maxdata[[comp]] <- Age_means_equalbins[["all"]]
  Age_ranges_maxdata[[comp]] <- Age_ranges_equalbins[["all"]]
}
savRDS(Age_ranges_equalbins, "Square/Data/Preprocessing/Age_ranges_equalbins")
savRDS(Age_means_equalbins, "Square/Data/Preprocessing/Age_means_equalbins")
savRDS(Age_ranges_maxdata, "Square/Data/Preprocessing/Age_ranges_maxdata")
savRDS(Age_means_maxdata, "Square/Data/Preprocessing/Age_means_maxdata")

#generate dictionary for biomarkers names and labels
biomarkers_hc <- redRDS("Square/Data/Preprocessing/biomarkers_hc", "all")
labels_hc <- redRDS("Square/Data/Preprocessing/labels_hc", "all")
Dictionnary_names_labels <- data.frame(cbind(biomarkers_hc, labels_hc))
Dictionnary_names_labels <- data.frame(lapply(Dictionnary_names_labels, as.character), stringsAsFactors=FALSE)
rownames(Dictionnary_names_labels) <- biomarkers_hc
names(Dictionnary_names_labels) <- c("name", "label")
savRDS(Dictionnary_names_labels, "Square/Data/Preprocessing/Dictionnary_names_labels")


#plot variables histograms
data <- redRDS("Square/Data/Preprocessing/data_square_all")
for (biomarker in c(biomarkers_hc, "RIDAGEYR", "age_factor"))
{
  data_b <- data
  ifelse(biomarker %in% biomarkers_hc, label_b <- Dictionnary_names_labels[biomarker, "label"], label_b <- biomarker)
  names(data_b)[which(names(data_b) == biomarker)] <- "biomarker"
  mx <- max(data_b$biomarker)
  mn <- min(data_b$biomarker)
  binwidth = (mx - mn)/100
  p <- ggplot(data=data_b) + geom_histogram(aes(data_b$biomarker), binwidth = binwidth) + ggtitle(paste(label_b, ", min = ", round(mn,2), " , max = ", round(mx,2), ", mean = ", round(mean(data_b$biomarker),2), ", median = ", round(median(data_b$biomarker),2))) + theme(plot.title = element_text(size = 8))
  p
  ggsav(file_name("Square/Results/Preprocessing/Plots/Histograms/histogram", label_b))
}

#save table for which variables change with age
data <- readRDS("~/Desktop/Aging/Square/Data/Preprocessing/data_square_all.Rda")
age_changes <- initiate_store(biomarkers_hc, c("Coefficients", "neg-log-corrected-p-values", "Significant", "Correlations"))
for (biomarker in biomarkers_hc)
{
  data_b <- data[,c("age_factor", biomarker, "weights")]
  names(data_b) <- c("age_factor", "biomarker", "weights")
  age_changes[biomarker,"Correlations"] <- cov.wt(cbind(data_b$age_factor, data_b$biomarker), wt = data_b$weights, cor = TRUE, center = TRUE)$cor[1,2]
  model <- lm(biomarker ~ age_factor, data_b, weights = data_b$weights)
  age_changes[biomarker,"Coefficients"] <- summary(model)$coefficients["age_factor", "Estimate"]
  age_changes[biomarker,"neg-log-corrected-p-values"] <- -log10(summary(model)$coefficients["age_factor", "Pr(>|t|)"]*length(biomarkers_hc))
  age_changes[biomarker,"Significant"] <-  ifelse(summary(model)$coefficients["age_factor", "Pr(>|t|)"]*length(biomarkers_hc) < 0.05, T, F)
}
rownames(age_changes) <- labels_hc
age_changes$Significant <- as.logical(age_changes$Significant)
age_changes <- age_changes[order(age_changes$Coefficients, decreasing = TRUE),]
savRDS(age_changes, "Square/Data/Preprocessing/Age_changes_table")

#calculate number of samples for each window, and take min for equal bin sizes analysis purpose
for (side in sides)
{
  for (half in halves)
  {
    #count how many samples in each age range bin for weighting purposes
    n_samples_sw <- sapply(demo_groups_swS, function(x) NULL)
    for (demo_group in demo_groups_sw)
    {
      n_samples_demo <- c()
      data <- redRDS(file_name("Square/Data/Preprocessing/data_square", demo_group, half, side))
      for (age_range in Age_ranges_equalbins[[demo_group]])
      {
        data_i <- data[data$age_factor >= as.numeric(gsub( "-.*$", "", age_range )) & data$age_factor < as.numeric(gsub("^.*\\-","", age_range)),]
        n_samples_demo <- c(n_samples_demo, nrow(data_i))
      }
      n_samples_sw[[demo_group]] <- n_samples_demo
    }
    #for group comparisons, take the inverse of the mean of the inverse
    for (comp in groups_comps_sw)
    {
      n_samples_sw[[comp]] <- 1/(1/n_samples_sw[[Dictionnary_groups_comps_sw[comp, 1]]] + 1/n_samples_sw[[Dictionnary_groups_comps_sw[comp, 2]]])
    }
    savRDS(n_samples_sw, file_name("Square/Data/Preprocessing/n_samples_sw", half, side))
    #count how many samples should be used for R2 comparisons purposes
    n_samples_equalbins <- n_samples_sw
    for (demo_groups in demo_groupS)
    {
      min_full <- n_samples_equalbins[[demo_groups[1]]][1]
      min_sw <- min(n_samples_equalbins[[demo_groups[1]]][-1])
      for (demo_group in demo_groups)
      {
        min_full <- min(min_full, n_samples_equalbins[[demo_group]][1])
        min_sw <- min(min_sw, n_samples_equalbins[[demo_group]][-1])
      }
      for (demo_group in demo_groups)
      {
        n_samples_equalbins[[demo_group]][1] <- min_full
        n_samples_equalbins[[demo_group]][-1] <- min_sw
      }
    }
    for (comp in groups_comps_sw)
    {
      n_samples_equalbins[[comp]] <- pmin(n_samples_equalbins[[Dictionnary_groups_comps_sw[comp, 1]]], n_samples_equalbins[[Dictionnary_groups_comps_sw[comp, 2]]])
    }
    for (demo_group in demo_groups_swS)
    {
      names(n_samples_equalbins[[demo_group]]) <- Age_ranges_equalbins[[demo_group]]
    }
    savRDS(n_samples_equalbins, file_name("Square/Data/Preprocessing/n_samples_equalbins", half, side))
  }
}
#do the same for full, unsplit data
#count how many samples in each age range bin for weighting purposes
n_samples_sw <- sapply(demo_groups_swS, function(x) NULL)
for (demo_group in demo_groups_sw)
{
  n_samples_demo <- c()
  data <- redRDS(file_name("Square/Data/Preprocessing/data_square", demo_group))
  for (age_range in Age_ranges_equalbins[[demo_group]])
  {
    data_i <- data[data$age_factor >= as.numeric(gsub( "-.*$", "", age_range )) & data$age_factor < as.numeric(gsub("^.*\\-","", age_range)),]
    n_samples_demo <- c(n_samples_demo, nrow(data_i))
  }
  n_samples_sw[[demo_group]] <- n_samples_demo
}
#for group comparisons, take the inverse of the mean of the inverse
for (comp in groups_comps_sw)
{
  n_samples_sw[[comp]] <- 1/(1/n_samples_sw[[Dictionnary_groups_comps_sw[comp, 1]]] + 1/n_samples_sw[[Dictionnary_groups_comps_sw[comp, 2]]])
}
savRDS(n_samples_sw, "Square/Data/Preprocessing/n_samples_sw")
#count how many samples should be used for R2 comparisons purposes
n_samples_equalbins <- n_samples_sw
for (demo_groups in demo_groupS)
{
  min_full <- n_samples_equalbins[[demo_groups[1]]][1]
  min_sw <- min(n_samples_equalbins[[demo_groups[1]]][-1])
  for (demo_group in demo_groups)
  {
    min_full <- min(min_full, n_samples_equalbins[[demo_group]][1])
    min_sw <- min(min_sw, n_samples_equalbins[[demo_group]][-1])
  }
  for (demo_group in demo_groups)
  {
    n_samples_equalbins[[demo_group]][1] <- min_full
    n_samples_equalbins[[demo_group]][-1] <- min_sw
  }
}
for (comp in groups_comps_sw)
{
  n_samples_equalbins[[comp]] <- pmin(n_samples_equalbins[[Dictionnary_groups_comps_sw[comp, 1]]], n_samples_equalbins[[Dictionnary_groups_comps_sw[comp, 2]]])
}
for (demo_group in demo_groups_swS)
{
  names(n_samples_equalbins[[demo_group]]) <- Age_ranges_equalbins[[demo_group]]
}
savRDS(n_samples_equalbins, file_name("Square/Data/Preprocessing/n_samples_equalbins"))

