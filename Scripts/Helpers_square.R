#Helpers square

#todo: modify generate function to also get all biomarkers + demo correctors.
n_lambdas <- 30
n_folds_CV <- 10
R_boot <- 100
names_correctors <- c("RIDAGEYR", "female", "hisp", "black", "other")
biomarkers <- redRDS("biomarkers")
n_predictors <- length(biomarkers) + length(names_correctors) - 1
distances <- c("biomarkers", "residuals", "coefficients")
age_groupsS <- c("all", "young", "old", "mixed1", "mixed2", "YvsO", "M1vsM2")
distancesS <- c("biomarkers", "residuals", "coefficients", "bmVSres", "bmVScoef", "resVScoef")

#generate square correlation matrices: biomarkers, residuals and coefficients
generate_square_matrices <- function(data, age_group)
{
  #initiate store for residuals
  data_res <- initiate_store(rownames(data), names(data))
  #for each variable, build model based on all the other variables
  w <- unlist(data$weights)
  data$RIDAGEYR <- normalize(data$RIDAGEYR)
  data <- data[, -which(names(data) %in% c("weights", "SEQN"))]
  for (target in biomarkers_hc)
  {
    x <- as.matrix(data[, -which(names(data) == target)])
    y <- data[, which(names(data) == target)]
    model <- cv.glmnet(x = x, y = y, nlambda = 100, alpha = 0.5, type.measure = "deviance", nfolds = n_folds_CV, parallel = TRUE, standardize = FALSE, weights = w)
    coefs <- coef(model, s= "lambda.min")[-1,]
    #save coefficient
    savRDS(coefs, "GL", age_group, target)
    #store residuals
    pred <- as.vector(predict(model, newx = x, weights = w, s= "lambda.min"))
    data_res[, which(names(data_res) == target)] <- data_res[, which(names(data_res) == target)] - pred
  }
  #generate squares
  square_biomarkers <- cor(data[,-which(names(data) %in% c("SEQN", "weights", names_correctors))])
  square_residuals <- cor(data_res[,-which(names(data_res) %in% c("SEQN", "weights", names_correctors))])
  square_coefficients <- build_square(age_group)
  #reorganize squares based on clusters and save them
  for (distance in distances)
  {
    sq <- get(file_name("square", distance))
    sq <- sq[biomarkers_hc, biomarkers_hc]
    savRDS(sq, "square", distance, age_group)
  }
  #save them as interactive plots
  #TODO
  return(square_coefficients)
}

build_squares <- function(age_group)
{
  square <- initiate_store(biomarkers, biomarkers)
  for (name1 in biomarkers_hc)
  {
    coef1 <- redRDS("GL", age_group, name1)
    for (name2 in biomarkers_hc)
    {
      coef2 <- redRDS("GL", age_group, name2)
      coef12 <- coef1[-which(names(coef1) %in% c(name2, names_correctors))]
      coef21 <- coef2[-which(names(coef2) %in% c(name1, names_correctors))]
      square[name1, name2] <- cor(coef12, coef21)
    }
  }
  savRDS(square, "correlation", age_group, "coefficients")
  return(square)
}


boot_R2 <- function(data, indices, k){
  data = data[indices,]
  y <- data$y
  pred <- data$pred
  w <- data$w
  weighted_mean = sum(y*w)/sum(w)
  SSR = sum(w*(y - pred) ^ 2)
  SST = sum(w * (y - weighted_mean) ^ 2)
  r2 <- (1 - SSR/SST)
  n <- length(y)
  k <- ncol(x)
  r2 <- 1-((1-r2)*(n-1)/(n-k-1))
  return(r2)
}

generate_predictions <- function()
{
  for (age_group in age_groups)
  {
    for (target in biomarkers_hc)
    {
      for (half in halves)
      {
        #format data, build the model and save predictions
        data <- get(file_name("data", half))
        w <- get(file_name("w", half))
        x <- as.matrix(data[, -which(names(data) == target)])
        y <-  data[, which(names(data) == target)]
        model <- cv.glmnet(x = x, y = y, nlambda = 100, alpha = 0.5, type.measure = "deviance", nfolds = 10, parallel = TRUE, standardize = FALSE, weights = w)
        for (half2 in halves)
        {
          data <- get(file_name("data", half2))
          y <-  data[, which(names(data) == target)]
          w <- get(file_name("w", half2))
          x <- as.matrix(data[, -which(names(data) == target)])
          pred <- as.vector(predict(model, newx = x, s= "lambda.min"))
          ifelse(half == "train", side <- "1", side <- "2")
          ifelse(half == half2, half3 <- "train", half3 <- "test")
          data_target <- as.data.frame(cbind(y, pred, w))
          savRDS(data_target, "data", target, age_group, half3, side)
        }
      }
      #merge to generate 12
      for (half in halves)
      {
        data_target_12 <- rbind(redRDS("data", target, age_group, half, "1"), redRDS("data", target, age_group, half, "2"))
        savRDS(data_target_12, "data", target, age_group, half, "12")
      }
    }
  }
}


generate_R2 <- function()
{
  for (half in halves)
  {
    for (side in sidesS)
    {
      R2S <- initiate_store(biomarkers_hc, age_groups)
      R2sdS <- initiate_store(biomarkers_hc, age_groups)
      for (age_group in age_groups)
      {
        for (target in biomarkers_hc)
        {
          data <- redRDS("data", target, age_group, half, side)
          R2_boot <- boot(data=data, statistic = boot_R2, R = R_boot, k = n_predictors)
          R2S[target, age_group] <- R2_boot$t0
          R2sdS[target, age_group] <- sd(R2_boot$t)
        }
      }
      savRDS(R2S, "R2S_square", half, side)
      savRDS(R2sdS, "R2sdS_square", half, side)
      R2S_text <- merge_text(R2S, R2sdS)
      savRDS(R2S_text, "R2_text", half, side)
    }
  }
}




#generate interactive plots
interactive_plot <- function(age_group, distance)
{
  square <- redRDS("square", distance, age_group)
  lims = c(-1,1)
  if (grepl("VS", distance))
  {
    lims <- lims*2
  }
  if (age_group %in% c("YvsO", "M1vsM2"))
  {
    lims <- lims*2
  }
  p <- plot_ly(x = labels_hc, y = labels_hc, z = as.matrix(square), type = "heatmap", colors = colorRamp(c("red", "white", "blue")), zmin = lims[1], zmax = lims[2])
  p
  export(p = p, file = paste(path, file_name("square", age_group, distance), ".png", sep = ""))
  htmlsav(p, "square", age_group, distance)
}

#generate scatter plots for differences matrices
scatter_plot <- function(square_1, square_2, name1, name2, group)
{
  #prepare data
  for (side in sides)
  {
    sq <- get(file_name("square", side))
    x <- sq[upper.tri(sq)]
    assign(file_name("cor", side), x)
  }
  cor_12 <- cor(cor_1, cor_2)
  text <- square_labels_hc[upper.tri(square_labels_hc)]
  data <- cbind(cor_1, cor_2, text)
  rownames(data) <- square_names_hc[upper.tri(square_names_hc)]
  data <- data.frame(data)
  p <- ggplot(data, aes(cor_1, cor_2, text = text))
  p <- p + geom_point(alpha=.5) + geom_abline() + ggtitle(paste("Correlations, ", group, ": ", name1, " vs ", name2)) + xlab(name1) + ylab(name2)
  ggsav(file_name("Correlation_comparisons", group, name1, "VS",name2))
  p <- ggplotly(p)
  p
  htmlsav(p, file_name("Correlation_comparisons", group, name1, "VS",name2))
  return(p)
}


