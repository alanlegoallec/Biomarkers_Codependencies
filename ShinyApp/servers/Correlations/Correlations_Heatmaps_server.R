output$text <- renderText({
    if(grepl(paste(c("VS", "Comparison"), collapse="|"), input$demo_group)){
        if(input$normed)
        {
            "Interpretation: The matrix is asymmetrical. The upper left triangle is the difference between the pairwise correlations for the 1st group on the specified age range, minus the pairwise correlations calculated on the entire cohort. The lower triangle are the differences between the correlations in the 2nd group and in the 1st group on the specified age range, minus this same difference calculated on the whole cohort."
        } else {
            "Interpretation: The matrix is asymmetrical. The upper left triangle are the pairwise correlations for the 1st group. The lower triangle are the differences between the 2nd and the 1st group. All those values are calculated for the specified age range."
        }
    } else {
        if(input$normed)
        {
            "Interpretation: The matrix is asymmetrical. The upper left triangle is the control: the pairwise correlations calculated on the full cohort. The lower right triangle is the difference between the pairwise correlation for the specified age range and the control."
        } else {
            "Interpretation: The matrix is symmetrical. Each value is a pairwise correlation for the specified age range."
        }
    }
})

output$Age_range <- renderUI({
    ifelse(input$type == "Variance of the correlations", choices <- Age_ranges_Correlations_equalbins, choices <- Age_ranges_Correlations)
    selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$plot_cor_sh <- renderPlotly({
    
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
    ifelse(grepl("var", type), square <- redRDS_equalbins("Correlations", type, display, normed, distance, demo_group, input$age_range), square <- redRDS("Correlations", type, normed, distance, demo_group, input$age_range))
    names(square) <- biomarkers_labels[names(square),"label"]
    rownames(square) <- biomarkers_labels[rownames(square),"label"]
    melted_square <- melt(as.matrix(square))
    names(melted_square) <- c("Biomarker1", "Biomarker2", "value")
    print(mean(melted_square$value))
    print(mean(abs(melted_square$value)))
    print(var(melted_square$value))
    print(sd(melted_square$value))
    if(input$type == "Correlations"){melted_square$value <- round(melted_square$value,3)}
    ifelse(input$type == "Correlations", lim_p <- max(1, max(abs(melted_square$value))), lim_p <- max(abs(melted_square$value)))
    p <- ggplot(data = melted_square, aes(Biomarker1, Biomarker2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-lim_p,lim_p), space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = size_labels_heatmaps, hjust = 1), axis.text.y = element_text(vjust = 1, size = size_labels_heatmaps, hjust = 1)) + coord_fixed() + ggtitle(paste(input$type, input$demo_group, input$age_range, sep =", "))
    #save figure
    if(generate_figures)
    {
      if(demo_group == "all"){ggsave("./figures/figure1B.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
      } else {ggsave(paste("./figures/figure5_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
    }
    pi <- ggplotly(p)
    pi
})


