output$plot_coef_sar <- renderPlotly({
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
  demo_1 <- Dictionnary_groups_comps[demo_group, "group1"]
  demo_2 <- Dictionnary_groups_comps[demo_group, "group2"]
  type <- switch(input$type,
                 "Coefficients" = "Coefficients",
                 "Variance of the coefficients" = "Coefficients_var")
  category <- Dictionnary_categories[demo_group, "category"]
  ifelse(type == "Coefficients_var", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  for (i in seq(2))
  {
    assign(file_name("name", as.character(i)), Dictionnary_groups_comps[demo_group, i])
    assign(file_name("data", as.character(i)), rdRDS("Coefficients", type, "significances", Dictionnary_groups_comps[demo_group, i]))
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
  data$Coefficient <- as.factor("") #avoids that geom_smooth creates a separate smooth for each label
  p <- ggplot(data, aes(Coefficient_1, Coefficient_2, group = Coefficient, text = text)) + geom_point(alpha=.5) + geom_abline() + geom_smooth(method = "lm") + ggtitle(paste(input$demo_group, ", Correlation = ", round(c, 3), ", Regression Coefficient - 1 = ", format(coef, scientific=TRUE, digits = 2), ", p-value = ", format(pv, scientific=TRUE, digits =2), sep = "")) + xlab(Dictionnary_buttons_demographicsS[demo_1, "button"]) + ylab(Dictionnary_buttons_demographicsS[demo_2, "button"]) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures){ggsave(paste("./figures/figure_coef_8_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  pi <- ggplotly(p)
  pi
})
