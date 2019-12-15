output$Age_range <- renderUI({
  ifelse(input$type == "Variance of the R2s", choices <- Age_ranges_R2s_equalbins, choices <- Age_ranges_R2s)
  selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$plot_r2s_sp <- renderPlotly({
  demo_group <- switch(input$demo_group,
                       "Males VS Females" = "MvsF",
                       "Sex Demographics - Control Comparison" = "CS1vsCS2",
                       "Whites VS Hispanics" = "WvsH",
                       "Whites VS Blacks" = "WvsB",
                       "Ethnicity Demographics - Control Comparison" = "CE1vsCE2")
  demo_1 <- Dictionnary_groups_comps[demo_group, "group1"]
  demo_2 <- Dictionnary_groups_comps[demo_group, "group2"]
  
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
  ifelse(grepl("var", type), rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  data <- rdRDS("R2s", type, method, category, age_range, half, side)
  
  for (i in seq(2))
  {
    ic <- as.character(i)
    assign(file_name("name", ic), Dictionnary_groups_comps_sw[demo_group, i])
    assign(file_name("R2", ic), data[,get(file_name("name", ic))])
  }
  c <- cor(R2_1, R2_2)
  data <- data.frame(cbind(R2_1, R2_2))
  ifelse(type == "R2S", lim_p <- c(-0.1, 1), lim_p <- c(min(data), max(data)))
  p <- ggplot(data, aes(R2_1, R2_2, text = labels_hc))
  p <- p + geom_point(alpha=.5) + geom_abline() + coord_cartesian(xlim = lim_p, ylim = lim_p) + ggtitle(paste(input$demo_group, ", Correlation = ", round(c, 3), sep = "")) + xlab(Dictionnary_buttons_demographicsS[demo_1, "button"]) + ylab(Dictionnary_buttons_demographicsS[demo_2, "button"]) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures){ggsave(paste("./figures/figure_r2_6_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  
  p
})
