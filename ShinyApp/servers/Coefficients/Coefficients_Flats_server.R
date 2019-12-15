output$plot_coef_fh <- renderPlotly({
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
  ifelse(grepl("var", type) & grepl("vs", demo_group), display_var <- "display", display_var <- "")
  display <- switch(input$display,
                 "the target" = "target",
                 "a predictor" = "predictor")
  ifelse(grepl("var", type), data <- redRDS_equalbins("Coefficients", type, display_var, normed, "flat", display, demo_group), data <- redRDS("Coefficients", type, normed, "flat", display, demo_group))
  if(display == "target")
  {
    data <- data[which(gsub( ":.*$", "", rownames(data)) == input$Biomarker),]
    rownames(data) <- gsub(".*: ","",rownames(data))
  } else {
    data <- data[which(gsub(".*: ","", rownames(data)) == input$Biomarker),]
    rownames(data) <- gsub( ":.*$", "", rownames(data))
  }
  lim <- max(abs(data), na.rm = TRUE)
  melted_data <- melt(as.matrix(data))
  names(melted_data) <- c("Biomarkers", "Age_Range", "value")
  #if(input$type == "Coefficients"){melted_data$value <- round(melted_data$value,4)} #better to display the numbers, but the colors are usually off at 0.
  #picture
  p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Coef") + theme_minimal() + ggtitle(input$Biomarker) + theme(plot.title = element_text(size=size_titles_flatheatmaps), axis.title.x = element_text(size = size_axis_flatheatmaps), axis.title.y = element_text(size = size_axis_flatheatmaps), axis.text.x = element_text(size=size_ticks_flatheatmaps_x, angle = 90), axis.text.y = element_text(size=size_ticks_flatheatmaps_y))
  pi <- ggplotly(p, autosize = TRUE)
  pi
})


