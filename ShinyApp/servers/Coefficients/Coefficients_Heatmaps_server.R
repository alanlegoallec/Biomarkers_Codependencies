output$Age_range <- renderUI({
  ifelse(input$type == "Variance of the coefficients", choices <- Age_ranges_Coefficients_equalbins, choices <- Age_ranges_Coefficients)
  selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$plot_coef_sh <- renderPlotly({
  
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
                 "Coefficients" = "Coefficients",
                 "Variance of the coefficients" = "Coefficients_var")
  ifelse(input$normed, normed <- "normed", normed <- "")
  ifelse(grepl("var", type) & grepl("vs", demo_group), display <- "display", display <- "")
  ifelse(grepl("var", type), square <- redRDS_equalbins("Coefficients", type, display, normed, demo_group, input$age_range), square <- redRDS("Coefficients", type, normed, demo_group, input$age_range))
  names(square) <- biomarkers_labels[names(square),"label"]
  rownames(square) <- biomarkers_labels[rownames(square),"label"]
  melted_square <- melt(as.matrix(square))
  names(melted_square) <- c("Biomarker1", "Biomarker2", "value")
  if(input$type == "Coefficients"){melted_square$value <- round(melted_square$value,3)}
  ifelse(input$type == "Coefficients", lim_p <- max(1, max(abs(melted_square$value))), lim_p <- max(abs(melted_square$value)))
  p <- ggplot(data = melted_square, aes(Biomarker1, Biomarker2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim_p,lim_p), space = "Lab", name="Coefficient") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = size_labels_heatmaps, hjust = 1), axis.text.y = element_text(vjust = 1, size = size_labels_heatmaps, hjust = 1)) + coord_fixed() + ggtitle(paste(input$type, input$demo_group, input$age_range, sep =", "))
  #save figure
  if(generate_figures)
  {
    if(demo_group == "all"){ggsave("./figures/figure_coef_1B.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    } else {ggsave(paste("./figures/figure_coef_5_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  }
  pi <- ggplotly(p)
  pi
})


