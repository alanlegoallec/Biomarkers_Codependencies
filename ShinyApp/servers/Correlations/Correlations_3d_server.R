output$Biomarker1 <- renderUI({
  selectInput(
    inputId = "Biomarker1", 
    label = "Select the 1st biomarker",
    choices = list_labels,
    selected = list_labels[[1]])
})

output$Biomarker2 <- renderUI({
  selectInput(
    inputId = "Biomarker2", 
    label = "Select the 2nd biomarker",
    choices = list_labels[-which(list_labels == input$Biomarker1)],
    selected = list_labels[[2]])
})

data <- reactive({
  demo_group <- switch(input$demo_group, 
                       "All" = "all",
                       "Males" = "male",
                       "Females" = "female",
                       "Sex Demographics - Control group 1" = "controlS1",
                       "Sex Demographics - Control group 2" = "controlS2",
                       "Whites" = "white",
                       "Hispanics" = "hispanic",
                       "Blacks" = "black",
                       "Others" = "other",
                       "Ethnicity Demographics - Control group 1" = "controlE1",
                       "Ethnicity Demographics - Control group 2" = "controlE2")
  if(input$normed)
  {
    data <- redRDS("Preprocessing", "data_square", demo_group)
    data <- data[which(data$age_factor >= input$age_range[1] & data$age_factor < input$age_range[2]),]
    data$weights <- 1
  } else {
    data <- data_raw
    data <- data[which(data$age_factor >= input$age_range[1] & data$age_factor < input$age_range[2]),]
    if(demo_group == "male"){data <- data[which(data$RIAGENDR == 1),]
    } else if(demo_group == "female"){data <- data[which(data$RIAGENDR == 2),]
    } else if(demo_group == "white"){data <- data[which(data$RIDRETH1 == 3),]
    } else if(demo_group == "hispanic"){data <- data[which(data$RIDRETH1 %in% c(1,2)),]
    } else if(demo_group == "black"){data <- data[which(data$RIDRETH1 == 4),]
    } else if(demo_group == "other"){data <- data[which(data$RIDRETH1 == 5),]
    } else if(demo_group == "controlS1"){data <- data[which(data$SEQN %in% index_1vs2_sexes),]
    } else if(demo_group == "controlS2"){data <- data[-(which(data$SEQN %in% index_1vs2_sexes)),]
    } else if(demo_group == "controlE1"){data <- data[which(data$SEQN %in% index_1vs2_ethnicities),]
    } else if(demo_group == "controlE2"){data <- data[-(which(data$SEQN %in% index_1vs2_ethnicities)),]}
  }
  data
})

biomarker1 <- reactive({Dictionnary_names_labels[which(Dictionnary_names_labels$label == input$Biomarker1), "name"]})
biomarker2 <- reactive({Dictionnary_names_labels[which(Dictionnary_names_labels$label == input$Biomarker2), "name"]})
demo_group <- reactive({
  switch(input$demo_group, 
         "All" = "all",
         "Males" = "male",
         "Females" = "female",
         "Sex Demographics - Control group 1" = "controlS1",
         "Sex Demographics - Control group 2" = "controlS2",
         "Whites" = "white",
         "Hispanics" = "hispanic",
         "Blacks" = "black",
         "Ethnicity Demographics - Control group 1" = "controlE1",
         "Ethnicity Demographics - Control group 2" = "controlE2")
})

output$rglPlot_3d <- renderRglwidget({
  try(rgl.close(), silent = TRUE)
  #load data
  data <- data()
  biomarker1 <- biomarker1()
  biomarker2 <- biomarker2()
  data <- data[, c(biomarker1, biomarker2, "age_factor", "weights")]
  names(data) <- c("biomarker1", "biomarker2", "age_factor", "weights")
  #precalculations
  lim_p = max(abs(data$biomarker1), abs(data$biomarker2))
  x = c(-lim_p,lim_p)
  x = c(min(data$biomarker1), max(data$biomarker1))
  intercepts <- c()
  coefs <- c()
  cors <- c()
  for (i in seq(min(data$age_factor),max(data$age_factor)))
  {
    data_i <- data[which(data$age_factor == i),]
    model <- lm(biomarker2 ~ biomarker1, data_i, weights = data_i$weights)
    intercept <- coef(model)["(Intercept)"]
    coef <- coef(model)["biomarker1"]
    cor_i <- cov.wt(cbind(data_i$biomarker1, data_i$biomarker2), wt = data_i$weights, cor = TRUE, center = TRUE)$cor[1,2]
    intercepts <- c(intercepts, intercept)
    coefs <- c(coefs, coef)
    cors <- c(cors, cor_i)
  }
  data_coefs <- data.frame(cbind(as.vector(coefs), as.vector(seq(min(data$age_factor),max(data$age_factor)))))
  names(data_coefs) <- c("coefs", "age")
  model_coefs <- lm(coefs ~ age, data = data_coefs)
  Coefs_coef <- summary(model_coefs)$coefficients["age", "Estimate"]
  Coefs_pv <- summary(model_coefs)$coefficients["age", "Pr(>|t|)"]
  data_cors <- data.frame(cbind(cors, seq(min(data$age_factor),max(data$age_factor))))
  names(data_cors) <- c("cors", "age")
  model_cors <- lm(cors ~ age, data = data_cors)
  Cors_coef <- summary(model_cors)$coefficients["age", "Estimate"]
  Cors_pv <- summary(model_cors)$coefficients["age", "Pr(>|t|)"]
  #generate 3D plot
  #add points
  title_main <- paste(input$Biomarker1, input$Biomarker2, sep = " VS ")
  title_sub <- paste("Coefficient: coef=", format(Coefs_coef, digits=2, scientific=TRUE), ", p_value=", format(Coefs_pv, digits=2, scientific=TRUE), ", Correlations: coef=", round(Cors_coef,3), ", p_value=", format(Cors_pv, digits=2, scientific=TRUE), sep = "")
  plot3d(data[,"biomarker1"], data[,"biomarker2"], data$age_factor, col="black", size=1, main = title_main, sub = title_sub, xlab = input$Biomarker1, ylab = input$Biomarker2, zlab = "Age")
  #add plane for average correlation
  model <- lm(biomarker2 ~ biomarker1, data, weights = data$weights)
  intercept <- coef(model)["(Intercept)"]
  coef <- coef(model)["biomarker1"]
  p1 <- c(0, intercept, 20)
  p2 <- c(4, intercept+4*coef, 20)
  p3 <- c(0, intercept, 80)
  v1 = c(0, 0, 1)
  v2 = c(1, coef, 0)
  w <- pracma::cross(v1,v2)
  d = -sum(w*p1)
  planes3d(w[1], w[2], w[3], d, alpha = 0.5, color = "green")
  #add lines for each age range correlation
  for (i in seq(length(coefs)))
  {
    y = c(intercepts[i]+x[1]*coefs[i], intercepts[i]+x[2]*coefs[i])
    z = c(input$age_range[1] - 1 + i, input$age_range[1] - 1 + i)
    lines3d(x,y,z,add=TRUE, col = "red")
  }
  rglwidget()
})

output$plot_3d <- renderPlot({
  data <- data()
  demo_group <- demo_group()
  biomarker1 <- biomarker1()
  biomarker2 <- biomarker2()
  cor <- cov.wt(cbind(data[,biomarker1], data[,biomarker2]), wt = data$weights, cor = TRUE, center = TRUE)$cor[1,2]
  data <- data.frame(cbind(data[,biomarker1], data[,biomarker2], data$age_factor, data$weights))
  names(data) <- c("biomarker1", "biomarker2", "age_factor", "weights")
  model <- lm(biomarker2 ~ biomarker1, data, weights = data$weights)
  coef <- summary(model)$coefficients["biomarker1", "Estimate"]
  pv <- summary(model)$coefficients["biomarker1", "Pr(>|t|)"]
  intercept <- summary(model)$coefficients["(Intercept)", "Estimate"]
  pair <- paste(input$Biomarker1, input$Biomarker2, sep = " VS ")
  title_p <- paste(pair, paste("Coefficient =", round(coef, 4), sep = " "), paste("p-value =", format(pv, digits=2, scientific=TRUE), sep = " "), paste("Correlation =", round(cor,3), sep = " "), sep = ", ")
  p <- ggplot(data = data, aes(biomarker1, biomarker2)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(title_p) + xlab(input$Biomarker1) + ylab(input$Biomarker2) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  p
})
