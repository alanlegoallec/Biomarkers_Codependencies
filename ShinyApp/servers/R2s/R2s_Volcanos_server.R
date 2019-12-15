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

data <- reactive({
  ifelse(type() == "R2Svar", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  #no need to use display because the variables were already saved under the no display name.
  #display <- ifelse(type() == "R2Svar" & grepl("vs", demo_group()), "display", "")
  data <- rdRDS("R2s", type(), "significances_ordered", method(), demo_group(), half(), side())
  data
})


output$plot_r2s_vol <- renderPlotly({
  data <- data()
  p <- ggplot(data, aes(x=coef, y=nlp, text = text)) + geom_point(alpha=.5, size = size_dots) + geom_hline(yintercept = -log10(0.05), size = size_lines) + ggtitle(paste("Age dependence of the ", input$type, ", ", input$demo_group, sep = "")) + xlab("R2s") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures & input$type == "R2s")
  {
    if(demo_group() == "all")
    {
      ggsave(paste("./figures/figure_r2_2_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    } else {
      ggsave(paste("./figures/figure_r2_9_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    }
  }
  pi <- ggplotly(p)
  pi
})

output$table_r2s_vol <- renderDataTable({
  data <- data()
  data <- data[,-which(names(data) %in% c("p", "text"))]
  rownames(data) <- Dictionnary_names_labels[rownames(data),"label"]
  data$coef <- format(data$coef, scientific = TRUE, digits = 2)
  data$nlp <- round(data$nlp, 1)
  data$young <- round(data$young, 3)
  data$old <- round(data$old, 3)
  data_sig <- data[data$significant,]
  print(table(data_sig$trajectory))
  print(table(data_sig$trajectory)/50*100)
  data
}, rownames = TRUE)



