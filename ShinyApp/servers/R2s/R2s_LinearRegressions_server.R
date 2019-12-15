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

Biomarker <- reactive({
  bm <- Dictionnary_names_labels[which(Dictionnary_names_labels$label == input$Biomarker),"name"]
})

type <- reactive({
  switch(input$type,
         "R2s" = "R2S",
         "Variance of the R2s" = "R2Svar")
})

method <- reactive({
  switch(input$method,
         "One model built for each age range" = "sw",
         "Single model built on all the samples" = "sm")
})

half <- reactive({
  switch(input$half,
         "Training" = "train",
         "Testing" = "test")
})

side <- reactive({
  switch(input$side,
         "1st half" = "1",
         "2nd half" = "2",
         "Both halves merged" = "12")
})

dataInput <- reactive({redRDS("R2s", "R2S", method(), demo_group(), half(), side())[,-1]})
dataInput_var <- reactive({redRDS("R2s", "R2Svar", method(), demo_group(), half(), side())[,-1]})
dataInput_var_display <- reactive({
  ifelse(grepl("vs", demo_group()), divp <- redRDS_equalbins("R2s", "R2Svar_display", method(), demo_group(), half(), side())[,-1], divp <- redRDS_equalbins("R2s", "R2Svar", method(), demo_group(), half(), side())[,-1])
  divp
})


output$plot_r2s_lr <- renderPlot({
  demo_group <- demo_group()
  Biomarker <- Biomarker()
  if(type() == "R2S")
  {
    #load data
    data <- dataInput()
    data_var <- dataInput_var()
    #initiate store
    data_measure <- initiate_store(Age_ranges_R2s[[demo_group]][-1], c("Biomarker_R2", "Age", "w", "sd"))
    data_measure$Age <- Age_means_R2s[[demo_group]][-1]
    data_measure$Biomarker_R2 <- unlist(data[Biomarker,])
    #weight the data points inversely proportionally to the variance of the measure (R2)
    if(min(data_var[Biomarker,]) == 0){data_var[Biomarker,which(data_var[Biomarker,] == 0)] <- min(data_var[Biomarker,][data_var[Biomarker,]!=min(data_var[Biomarker,])])}#fix if some of the variance is zero}
    data_measure$var <- unlist(data_var[Biomarker,])
    data_measure$sd <- sqrt(data_measure$var)
    data_measure$w <- 1/data_measure$var
    model <- tryCatch(rma(Biomarker_R2, var, mods =  ~ Age, data=data_measure, control=list(maxiter=10000)), error=function(err) NA)
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
    plot_title <- paste(input$Biomarker, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), paste("young =", round(young, 2), sep = " "), paste("old =", round(old, 2), sep = " "), sep = ", ")
    p <- ggplot(data = data_measure, aes(Age, Biomarker_R2)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + geom_errorbar(aes(ymin=Biomarker_R2-sd, ymax=Biomarker_R2+sd), width=size_bars) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figures
    if(generate_figures){ggsave(paste("./figures/figure_r2_3_", input$Biomarker, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  } else{
    #Perform analysis of variances of R2s
    #load data
    data <- dataInput_var_display()
    data_measure <- initiate_store(Age_ranges_R2s[[demo_group]][-1], c("Biomarker_R2", "Age"))
    data_measure$Age <- Age_means_R2s[[demo_group]][-1]
    data_measure$Biomarker_R2 <- unlist(data[Biomarker,])
    model <- lm(Biomarker_R2~Age, data = data_measure)
    coef <- summary(model)$coefficients["Age", "Estimate"]
    pv <- summary(model)$coefficients["Age", "Pr(>|t|)"]
    intercept <- summary(model)$coefficients["(Intercept)", "Estimate"]
    young <- intercept + 20*summary(model)$coefficients["Age","Estimate"]
    old <- intercept + 80*summary(model)$coefficients["Age","Estimate"]
    #weighted linear regression plot
    plot_title <- paste(input$Biomarker, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), paste("young =", round(young, 2), sep = " "), paste("old =", round(old, 2), sep = " "), sep = ", ")
    p <- ggplot(data = data_measure, aes(Age, Biomarker_R2)) +  geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(plot_title) + xlab("Age") + ylab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  }
  p
})

output$table_r2s_lr <- renderDataTable({
  data <- redRDS("R2s", type(), "significances_ordered", method(), demo_group(), half(), side())
  data <- data[,-which(names(data) %in% c("p", "text"))]
  rownames(data) <- Dictionnary_names_labels[rownames(data),"label"]
  data$coef <- format(data$coef, scientific = TRUE, digits = 2)
  data$nlp <- round(data$nlp, 1)
  data$young <- round(data$young, 3)
  data$old <- round(data$old, 3)
  data
}, rownames = TRUE)

