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

data <- reactive({
  distance <- distance()
  demo_group <- demo_group()
  type <- type()
  #here no need to use display because during tests for significance, everything was saved under the same name (no need to save without display mode, so display mode saved under no display mode)
  ifelse(type == "Correlations_var", data <- redRDS_equalbins("Correlations", type, "significances_ordered", distance, demo_group), data <- redRDS("Correlations", type, "significances_ordered", distance, demo_group))
  data
})


output$plot_cor_vol <- renderPlotly({
  data <- data()
  if(input$type == "Correlations")
  {
    cols = c("increase_-_-" = "yellow", "increase_-_+" = "red", "increase_+_+" = "orange", "decrease_+_+" = "green", "decrease_+_-" = "blue", "decrease_-_-" = "purple")
    names(cols) <- trajectories_correlations
    cols <- cols[which(names(cols) %in% names(table(data$trajectory)[which(table(data$trajectory) > 0)]))]
    p <- ggplot(data, aes(x=coef, y=nlp, color = trajectory, text = text)) + geom_point(alpha=.5, size = size_dots) + scale_colour_manual(values = cols) + geom_hline(yintercept = -log10(0.05), size = size_lines) + ggtitle(paste("Age dependence of the correlations, ", input$demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value") + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks), legend.position="bottom", legend.box = "horizontal")
    } else {
    p <- ggplot(data, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5, size = size_dots) + geom_hline(yintercept = -log10(0.05), size = size_lines) + ggtitle(paste("Age dependence of the variances of the correlations, ", input$demo_group, sep = "")) + xlab("Coefficients") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks), legend.position="bottom", legend.box = "horizontal")}
  #save figure
  if(generate_figures & input$type == "Correlations")
  {
    if(demo_group() == "all")
    {
      ggsave(paste("./figures/figure2_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    } else {
      ggsave(paste("./figures/figure9_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    }
  }
  pi <- ggplotly(p)
  pi
})

output$table_cor_vol <- renderDataTable({
  data <- data()
  data <- data[,-which(names(data) %in% c("p", "text"))]
  data$coef <- format(data$coef, scientific = TRUE, digits = 2)
  data$nlp <- round(data$nlp, 1)
  data$young <- round(data$young, 3)
  data$old <- round(data$old, 3)
  #print for paper writing purposes
  print(table(data$significant))
  print(length(which(data$significant & (data$old - data$young) > 0)))
  print(length(which(data$significant & (data$old - data$young) < 0)))
  data_sig <- data[data$significant,]
  print(table(data_sig$trajectory))
  print(table(data_sig$trajectory)/1225*100)
  data
}, rownames = TRUE)
