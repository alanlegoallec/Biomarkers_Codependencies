output$plot_r2s_sar <- renderPlotly({
  demo_group <- switch(input$demo_group,
                       "Males VS Females" = "MvsF",
                       "Sex Demographics - Control Comparison" = "CS1vsCS2",
                       "Whites VS Hispanics" = "WvsH",
                       "Whites VS Blacks" = "WvsB",
                       "Ethnicity Demographics - Control Comparison" = "CE1vsCE2")
  age_range <- input$age_range
  category <- Dictionnary_categories[demo_group, "category"]
  
  method <- switch(input$method,
                   "One model built for each age range" = "sw",
                   "Single model built on all the samples" = "sm")
  
  type <- switch(input$type,
                 "R2s" = "R2S",
                 "Variance of the R2s" = "R2Svar")
  
  half <- switch(input$half,
                 "Training" = "train",
                 "Testing" = "test")
  
  side <- switch(input$side,
                 "1st half" = "1",
                 "2nd half" = "2",
                 "Both halves merged" = "12")
  demo_1 <- Dictionnary_groups_comps[demo_group, "group1"]
  demo_2 <- Dictionnary_groups_comps[demo_group, "group2"]
  
  if(type == "R2S")
  {
    data_1 <- redRDS("R2s", type, "significances", method, demo_1, half, side)
    data_2 <- redRDS("R2s", type, "significances", method, demo_2, half, side)
  } else {
    if (grepl("vs", demo_group)){
      data_1 <- redRDS("R2s", "R2Svar_display", "significance", method, demo_1, half, side)
      data_2 <- redRDS("R2s", "R2Svar_display", "significance", method, demo_2, half, side)
    } else {
      data_1 <- redRDS("R2s", type, "significance", method, demo_1, half, side)
      data_2 <- redRDS("R2s", type, "significance", method, demo_2, half, side)
    }
  }
  intersection <- intersect(rownames(data_1[data_1$significant,]), rownames(data_2[data_2$significant,]))
  data_1 <- data_1[intersection,]
  data_2 <- data_2[intersection,]
  Coefficient_1 <- data_1$coef
  Coefficient_2 <- data_2$coef
  c <- cor(Coefficient_1, Coefficient_2)
  data <- data.frame(cbind(Coefficient_1, Coefficient_2))
  data$text <- Dictionnary_names_labels[rownames(data_1),"label"]
  data$R2 <- as.factor(1)
  data$Coefficient_diff <- data$Coefficient_2 - data$Coefficient_1
  model <- lm(Coefficient_diff~Coefficient_1, data = data)
  coef <- summary(model)$coefficients["Coefficient_1", 1]
  pv <- summary(model)$coefficients["Coefficient_1", 4]
  p <- ggplot(data, aes(Coefficient_1, Coefficient_2, group = R2, text = text)) + geom_point(alpha=.5) + geom_abline() + geom_smooth(method='lm') + ggtitle(paste(input$demo_group, ", Correlation = ", round(c, 3), ", Regression Coefficient - 1 = ", format(coef, scientific=TRUE, digits = 2), ", p-value = ", format(pv, scientific=TRUE, digits =2), sep = "")) + xlab(Dictionnary_buttons_demographicsS[demo_1, "button"]) + ylab(Dictionnary_buttons_demographicsS[demo_2, "button"]) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures){ggsave(paste("./figures/figure8_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  p
})
