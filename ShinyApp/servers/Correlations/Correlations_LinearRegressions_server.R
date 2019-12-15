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

distance <- reactive({
  switch(input$distance,
         "Pearson Correlation" = "biomarkers",
         "Pearson Correlation between the linear regression coefficients" = "coefficients",
         "Difference between the two distances" = "bmVScoef")
})

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
         "Ethnicity Demographics - Control group 2" = "controlE2",
         "Males VS Females" = "MvsF",
         "Sex Demographics - Control Comparison" = "CS1vsCS2",
         "Whites VS Hispanics" = "WvsH",
         "Whites VS Blacks" = "WvsB",
         "Ethnicity Demographics - Control Comparison" = "CE1vsCE2")
})

type <- reactive({
  switch(input$type,
         "Correlations" = "Correlations",
         "Variance of the correlations" = "Correlations_var")
})

dataInput <- reactive({redRDS("Correlations", "Correlations_flat", distance(), demo_group())[,-1]})
dataInput_var <- reactive({redRDS("Correlations", "Correlations_var_flat", distance(), demo_group())[,-1]})
dataInput_var_display <- reactive({
  ifelse(grepl("vs", demo_group()) | grepl("VS", distance()), divp <- redRDS_equalbins("Correlations", "Correlations_var_display_flat", distance(), demo_group())[,-1], divp <- redRDS_equalbins("Correlations", "Correlations_var_flat", distance(), demo_group())[,-1])
  divp
})

output$plot_cor_lr <- renderPlot({
  demo_group <- demo_group()
  pair <- paste(input$Biomarker1, input$Biomarker2, sep = " VS ")
  if(!(pair %in% melted_labels_hc))
  {
    pair <- paste(input$Biomarker2, input$Biomarker1, sep = " VS ")
  }
  if(type() == "Correlations")
  {
    #load data
    data <- dataInput()
    data_var <- dataInput_var()
    #initiate store
    data_measure <- initiate_store(Age_ranges_Correlations[[demo_group]][-1], c("Biomarkers_correlation", "Age", "w", "sd"))
    data_measure$Age <- Age_means_Correlations[[demo_group]][-1]
    data_measure$Biomarkers_correlation <- unlist(data[pair,])
    #weight the data points inversely proportionally to the variance of the measure (correlation)
    if(min(data_var[pair,]) == 0){data_var[pair,which(data_var[pair,] == 0)] <- min(data_var[pair,][data_var[pair,]!=min(data_var[pair,])])}#fix if some of the variance is zero}
    data_measure$var <- unlist(data_var[pair,])
    data_measure$sd <- sqrt(data_measure$var)
    data_measure$w <- 1/data_measure$var
    model <- tryCatch(rma(Biomarkers_correlation, var, mods =  ~ Age, data=data_measure, control=list(maxiter=10000)), error=function(err) NA)
    if(is.na(model[1]))
    {
      print("Error in the meta-regression, NAs stored.")
      coef <- NA
      pv <- NA
      intercept <- NA
      young <- NA
      old <- NA
    } else {
      coef <- model$beta[2]
      pv <- model$pval[2]
      intercept <- model$beta[1]
      young <- intercept + 20*coef
      old <- intercept + 80*coef
    }
    #weighted linear regression plot
    plot_title <- paste(pair, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), paste("young =", round(young, 3), sep = " "), paste("old =", round(old, 3), sep = " "), sep = ", ")
    p <- ggplot(data = data_measure, aes(Age, Biomarkers_correlation)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + geom_errorbar(aes(ymin=Biomarkers_correlation-sd, ymax=Biomarkers_correlation+sd), width=size_bars) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figures
    if(generate_figures){ggsave(paste("./figures/figure3_", pair, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  } else{
    #Perform analysis of variances of correlations
    #load data
    data <- dataInput_var_display()
    data_measure <- initiate_store(Age_ranges_Correlations_equalbins[[demo_group]][-1], c("Biomarkers_correlation", "Age"))
    data_measure$Age <- Age_means_Correlations_equalbins[[demo_group]][-1]
    data_measure$Biomarkers_correlation <- unlist(data[pair,])
    model <- lm(Biomarkers_correlation~Age, data = data_measure)
    coef <- summary(model)$coefficients["Age", 1]
    pv <- summary(model)$coefficients["Age", 4]
    intercept <- summary(model)$coefficients["(Intercept)","Estimate"]
    young <- intercept + 20*summary(model)$coefficients["Age","Estimate"]
    old <- intercept + 80*summary(model)$coefficients["Age","Estimate"]
    #weighted linear regression plot
    plot_title <- paste(pair, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), paste("young =", round(young, 3), sep = " "), paste("old =", round(old, 3), sep = " "), sep = ", ")
    p <- ggplot(data = data_measure, aes(Age, Biomarkers_correlation)) +  geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  }
  p
})

output$table_cor_lr <- renderDataTable({
  data <- redRDS("Correlations", type(), "significances_ordered", distance(), demo_group())
  data <- data[,-which(names(data) %in% c("p", "text"))]
  data$coef <- format(data$coef, scientific = TRUE, digits = 2)
  data$nlp <- round(data$nlp, 1)
  data$young <- round(data$young, 3)
  data$old <- round(data$old, 3)
  data
}, rownames = TRUE)

