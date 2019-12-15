output$Target <- renderUI({
  selectInput(
    inputId = "Target",
    label = "Select the target biomarker",
    choices = list_labels,
    selected = list_labels[[1]])
  
})

output$Predictor <- renderUI({
  selectInput(
    inputId = "Predictor",
    label = "Select the predictor biomarker",
    choices = list_labels[-which(list_labels == input$Target)],
    selected = list_labels[[2]])
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
         "Coefficients" = "Coefficients",
         "Variance of the coefficients" = "Coefficients_var")
})

dataInput <- reactive({redRDS("Coefficients", "Coefficients_flat", "target", demo_group())[,-1]})
dataInput_var <- reactive({redRDS("Coefficients", "Coefficients_var_flat", "target", demo_group())[,-1]})
dataInput_var_display <- reactive({
  ifelse(grepl("vs", demo_group()), divp <-redRDS_equalbins("Coefficients", "Coefficients_var_display_flat", "target", demo_group())[,-1], divp <- redRDS_equalbins("Coefficients", "Coefficients_var_flat", "target", demo_group())[,-1])
  divp
})

output$plot_coef_lr <- renderPlot({
  demo_group <- demo_group()
  pair <- paste(input$Target, input$Predictor, sep = ": ")
  if(type() == "Coefficients")
  {
    #load data
    data <- dataInput()
    data_var <- dataInput_var()
    #initiate store
    data_measure <- initiate_store(Age_ranges_Coefficients[[demo_group]][-1], c("Biomarkers_coefficient", "Age", "w", "sd"))
    data_measure$Age <- Age_means_Coefficients[[demo_group]][-1]
    data_measure$Biomarkers_coefficient <- unlist(data[pair,])
    #weight the data points inversely proportionally to the variance of the measure (coefficient)
    if(min(data_var[pair,]) == 0){data_var[pair,which(data_var[pair,] == 0)] <- min(data_var[pair,][data_var[pair,]!=min(data_var[pair,])])}#fix if some of the variance is zero}
    data_measure$var <- unlist(data_var[pair,])
    data_measure$sd <- sqrt(data_measure$var)
    data_measure$w <- 1/data_measure$var
    model <- tryCatch(rma(Biomarkers_coefficient, var, mods =  ~ Age, data=data_measure, control=list(maxiter=10000)), error=function(err) NA)
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
    p <- ggplot(data = data_measure, aes(Age, Biomarkers_coefficient)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + geom_errorbar(aes(ymin=Biomarkers_coefficient-sd, ymax=Biomarkers_coefficient+sd), width=size_bars) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figures
    if(generate_figures){ggsave(paste("./figures/figure_coef_3_", pair, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
    p
  } else{
    #Perform analysis of variances of coefficients
    #load data
    data <- dataInput_var_display()
    data_measure <- initiate_store(Age_ranges_Coefficients_equalbins[[demo_group]][-1], c("Biomarkers_coefficient", "Age"))
    data_measure$Age <- Age_means_Coefficients_equalbins[[demo_group]][-1]
    data_measure$Biomarkers_coefficient <- unlist(data[pair,])
    model <- lm(Biomarkers_coefficient~Age, data = data_measure)
    coef <- summary(model)$coefficients["Age", 1]
    pv <- summary(model)$coefficients["Age", 4]
    intercept <- summary(model)$coefficients["(Intercept)","Estimate"]
    young <- intercept + 20*summary(model)$coefficients["Age","Estimate"]
    old <- intercept + 80*summary(model)$coefficients["Age","Estimate"]
    #weighted linear regression plot
    plot_title <- paste(pair, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), paste("young =", round(young, 3), sep = " "), paste("old =", round(old, 3), sep = " "), sep = ", ")
    p <- ggplot(data = data_measure, aes(Age, Biomarkers_coefficient)) +  geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    p
  }
})

output$table_coef_lr <- renderDataTable({
  data <- redRDS("Coefficients", type(), "significances_ordered", demo_group())
  data <- data[,-which(names(data) %in% c("p", "text"))]
  data$coef <- format(data$coef, scientific = TRUE, digits = 2)
  data$nlp <- round(data$nlp, 1)
  data$young <- round(data$young, 3)
  data$old <- round(data$old, 3)
  data
}, rownames = TRUE)

