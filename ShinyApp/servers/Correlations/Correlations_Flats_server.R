output$plot_cor_fh <- renderPlotly({
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
  
  ifelse(input$normed, normed <- "normed", normed <- "")
  ifelse(grepl("var", type) & (grepl("vs", demo_group) | grepl("VS", distance)), display <- "display", display <- "")
  ifelse(grepl("var", type), data <- redRDS_equalbins("Correlations", type, display, normed, "flat", distance, demo_group), data <- redRDS("Correlations", type, normed, "flat", distance, demo_group))
  
  name1 <- gsub(" VS .*$", "", rownames(data))
  name2 <- gsub(".* VS ", "", rownames(data))
  print(name1)
  print(name2)
  index <- which(name1 == input$Biomarker | name2 == input$Biomarker)
  data <- data[index,]
  name1 <- name1[index]
  name2 <- name2[index]
  row_names <- c()
  for (i in seq(nrow(data)))
  {
    if(name1[i] == input$Biomarker)
    {
      row_names <- c(row_names, name2[i])
    } else {
      row_names <- c(row_names, name1[i])
    }
  }
  rownames(data) <- row_names
  lim <- max(abs(data), na.rm = TRUE)
  melted_data <- melt(as.matrix(data))
  names(melted_data) <- c("Biomarkers", "Age_Range", "value")
  #if(input$type == "Correlations"){melted_data$value <- round(melted_data$value,4)} #better to display the numbers, but the colors are usually off at 0.
  #picture
  p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="Cor") + theme_minimal() + ggtitle(input$Biomarker) + theme(plot.title = element_text(size=size_titles_flatheatmaps), axis.title.x = element_text(size = size_axis_flatheatmaps), axis.title.y = element_text(size = size_axis_flatheatmaps), axis.text.x = element_text(size=size_ticks_flatheatmaps_x, angle = 90), axis.text.y = element_text(size=size_ticks_flatheatmaps_y))
  pi <- ggplotly(p)
  pi
})


