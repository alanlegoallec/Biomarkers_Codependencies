output$demo_group <- renderUI({
  ifelse(grepl("Difference", input$distance), choices_demographics <- buttons_demographicsS, choices_demographics <- buttons_groups_comps)
  selectInput(
    inputId = "demo_group",
    label = "Select the demographic group",
    choices = choices_demographics)
})

output$plot_cor_sar <- renderPlotly({
  distance <- switch(input$distance,
                     "Pearson Correlation" = "biomarkers",
                     "Pearson Correlation between the linear regression coefficients" = "coefficients",
                     "Difference between the two distances" = "bmVScoef")
  demo_group <- switch(input$demo_group,
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
  type <- switch(input$type,
                 "Correlations" = "Correlations",
                 "Variance of the correlations" = "Correlations_var")
  category <- Dictionnary_categories[demo_group, "category"]
  ifelse(type == "Correlations_var", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  if(grepl("VS", distance))
  {
    name1 <- Dictionnary_buttons_distancesS[Dictionnary_distances_comps[demo_group, "group1"], "button"]
    name2 <- Dictionnary_buttons_distancesS[Dictionnary_distances_comps[demo_group, "group2"], "button"]
    for (i in seq(2))
    {
      assign(file_name("name", as.character(i)), Dictionnary_distances_comps[distance, i])
      assign(file_name("data", as.character(i)), rdRDS("Correlations", type, "significances", Dictionnary_distances_comps[distance, i], demo_group))
    }
  } else {
    name1 <- Dictionnary_buttons_demographicsS[Dictionnary_groups_comps[demo_group, "group1"], "button"]
    name2 <- Dictionnary_buttons_demographicsS[Dictionnary_groups_comps[demo_group, "group2"], "button"]
    for (i in seq(2))
    {
      assign(file_name("name", as.character(i)), Dictionnary_groups_comps[demo_group, i])
      assign(file_name("data", as.character(i)), rdRDS("Correlations", type, "significances", distance, Dictionnary_groups_comps[demo_group, i]))
    }
  }
  intersection <- intersect(rownames(data_1[data_1$significant,]), rownames(data_2[data_2$significant,]))
  data_1 <- data_1[intersection,]
  data_2 <- data_2[intersection,]
  Coefficient_1 <- data_1$coef
  Coefficient_2 <- data_2$coef
  c <- cor(Coefficient_1, Coefficient_2)
  data <- data.frame(cbind(Coefficient_1, Coefficient_2))
  rownames(data) <- intersection
  data$Coefficient_diff <- data$Coefficient_2 - data$Coefficient_1
  model <- lm(Coefficient_diff~Coefficient_1, data = data)
  coef <- summary(model)$coefficients["Coefficient_1", 1]
  pv <- summary(model)$coefficients["Coefficient_1", 4]
  data$text <- intersection
  data$Correlation <- as.factor("") #avoids that geom_smooth creates a separate smooth for each label
  p <- ggplot(data=data, aes(Coefficient_1, Coefficient_2, group = Correlation, text = text)) + geom_point(alpha=.5) + geom_smooth(method='lm') + geom_abline() + ggtitle(paste(input$demo_group, ", Correlation = ", round(c, 3), ", Regression Coefficient - 1 = ", format(coef, scientific=TRUE, digits = 2), ", p-value = ", format(pv, scientific=TRUE, digits =2), sep = "")) + xlab(name1) + ylab(name2) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures){ggsave(paste("./figures/figure8_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  pi <- ggplotly(p)
  pi
})

