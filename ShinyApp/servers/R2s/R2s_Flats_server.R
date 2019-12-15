output$plot_r2s_fh <- renderPlotly({
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
           "R2s" = "R2S",
           "Variance of the R2s" = "R2Svar")
  
  method <- switch(input$method,
           "One model built for each age range" = "sw",
           "Single model built on all the samples" = "sm")
  
  half <- switch(input$half,
           "Training" = "train",
           "Testing" = "test")
  
  side <- switch(input$side,
           "1st half" = "1",
           "2nd half" = "2",
           "Both halves merged" = "12")
    
  type <- ifelse(input$normed, paste(type, "normed", sep = "_"), type)
  data <- redRDS_equalbins("R2s", type, method, demo_group, half, side)[,-1]
  rownames(data) <- Dictionnary_names_labels[rownames(data),"label"]
  lim <- max(abs(data), na.rm = TRUE)
  melted_data <- melt(as.matrix(data))
  names(melted_data) <- c("Biomarkers", "Age_Range", "value")
  if(input$type == "R2s"){melted_data$value <- round(melted_data$value,20)}
  #picture
  p <- ggplot(data = melted_data, aes(Age_Range, Biomarkers, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim, lim), space = "Lab", name="R2") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = size_labels_heatmaps, hjust = 1), axis.text.y = element_text(vjust = 1, size = size_labels_heatmaps, hjust = 1)) + ggtitle(paste(input$type, input$demo_group, sep = ", "))
  pi <- ggplotly(p)
  pi
})



