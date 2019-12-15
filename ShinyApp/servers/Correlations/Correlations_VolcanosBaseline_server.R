output$Age_range <- renderUI({
  selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = Age_ranges_Correlations[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
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

method <- reactive({
  switch(input$method,
                 "One model built for each age range" = "sw",
                 "Single model built on all the samples" = "sm")
})

normed <- reactive({ifelse(input$normed, "normed", "")})

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

data <- reactive({
  data <- redRDS("Correlations", "Correlations", normed(), "flat", distance(), demo_group())
  data
})

data_var <- reactive({
  data_var <- redRDS("Correlations", "Correlations_var", "flat", distance(), demo_group())
  data_var
})

data_p <- reactive({
  data_p <- data.frame(2*pnorm(abs(as.matrix(data())), 0, sqrt(as.matrix(data_var())), lower.tail = FALSE)*nrow(data()))
  names(data_p) <- Age_ranges_Correlations[[demo_group()]]
  data_p
})

data_a <- reactive({
  age_range <- input$age_range
  data_a <- data()[,input$age_range]
  data_var_a <- data_var()[,input$age_range]
  data_p_a <- data_p()[,input$age_range]
  data_a <- data.frame(cbind(data_a, -log10(data_p_a)))
  data_a$text <- melted_labels_hc
  names(data_a) <- c("Cors", "nlp", "text")
  rownames(data_a) <- melted_labels_hc
  if(grepl("vs", demo_group()) | grepl("VS", distance())){data_a <- cbind(data_a, data_12_a())}
  #print for paper writing purposes
  print(table(data_p_a < 0.05))
  print(table(data_p_a < 0.05 & data_a$Cors > 0))
  print(table(data_p_a < 0.05 & data_a$Cors < 0))
  data_a
})

data_12_a <- reactive({
  if(grepl("VS", distance()))
  {
    distance_1 <- Dictionnary_distances_comps[distance(), "distance1"]
    distance_2 <- Dictionnary_distances_comps[distance(), "distance2"]
    data_1 <- redRDS("Correlations", "Correlations", normed(), "flat", distance_1, demo_group())
    data_2 <- redRDS("Correlations", "Correlations", normed(), "flat", distance_2, demo_group())
    data_12_a <- data.frame(cbind(data_1[,input$age_range], data_2[,input$age_range]))
    names(data_12_a) <- c(Dictionnary_buttons_distancesS[distance_1, "button"], Dictionnary_buttons_distancesS[distance_2, "button"])
  } else {
    demo_1 <- Dictionnary_groups_comps[demo_group(), "group1"]
    demo_2 <- Dictionnary_groups_comps[demo_group(), "group2"]
    data_1 <- redRDS("Correlations", "Correlations", normed(), "flat", distance(), demo_1)
    data_2 <- redRDS("Correlations", "Correlations", normed(), "flat", distance(), demo_2)
    data_12_a <- data.frame(cbind(data_1[,input$age_range], data_2[,input$age_range]))
    names(data_12_a) <- c(Dictionnary_buttons_demographicsS[demo_1, "button"], Dictionnary_buttons_demographicsS[demo_2,"button"])
  }
  data_12_a
})

output$plot_cor_vb <- renderPlotly({
  data_a <- data_a()
  if(max(data_a$nlp == Inf)){data_a$nlp[which(data_a$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
  #plot
  p <- ggplot(data_a, aes(x=Cors, y=nlp, text = text)) + geom_point(alpha=.5, size = size_dots) + geom_hline(yintercept = -log10(0.05), size = size_lines) + ggtitle(paste("Baseline correlations, ", input$demo_group, sep = "")) + xlab("Correlations") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figures
  if(generate_figures){ggsave(paste("./figures/figure7_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  pi <- ggplotly(p)
  pi
})

output$table_cor_vb <- renderDataTable({
  data <- data_a()
  data <- data[,-which(names(data) %in% c("p", "text"))]
  data <- data[order(data$Cors),]
  data$Cors <- round(data$Cors, 3)
  data$nlp <- round(data$nlp, 1)
  if(grepl("vs", demo_group()) | grepl("VS", distance()))
  {
    data[,3] <- round(data[,3], 3)
    data[,4] <- round(data[,4], 3)
  }
  data
}, rownames = TRUE)

