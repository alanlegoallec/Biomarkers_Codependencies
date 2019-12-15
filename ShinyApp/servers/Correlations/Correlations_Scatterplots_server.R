output$demo_group <- renderUI({
    ifelse(grepl("Difference", input$distance), choices_demographics <- buttons_demographicsS, choices_demographics <- buttons_groups_comps)
    selectInput(
    inputId = "demo_group",
    label = "Select the demographic group",
    choices = choices_demographics)
})

output$Age_range <- renderUI({
    ifelse(input$type == "Variance of the correlations", choices <- Age_ranges_Correlations_equalbins, choices <- Age_ranges_Correlations)
    selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$plot_cor_sp <- renderPlotly({
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
    demo_1 <- Dictionnary_groups_comps[demo_group, "group1"]
    demo_2 <- Dictionnary_groups_comps[demo_group, "group2"]
    type <- switch(input$type,
    "Correlations" = "Correlations",
    "Variance of the correlations" = "Correlations_var")
    category <- Dictionnary_categories[demo_group, "category"]
    ifelse(type == "Correlations_var", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
    if(grepl("VS", distance))
    {
        for (i in seq(2))
        {
            assign(file_name("name", as.character(i)), Dictionnary_distances_comps[distance, i])
            assign(file_name("square", as.character(i)), rdRDS("Correlations", type, Dictionnary_distances_comps[distance, i], demo_group, input$age_range))
        }
    } else {
        for (i in seq(2))
        {
            assign(file_name("name", as.character(i)), Dictionnary_groups_comps[demo_group, i])
            assign(file_name("square", as.character(i)), rdRDS("Correlations", type, distance, Dictionnary_groups_comps[demo_group, i], input$age_range))
        }
    }
    age_range <- input$age_range
    group <- paste(distance, demo_group, age_range, sep = ", ")
    #prepare data
    for (side in sides)
    {
      sq <- get(file_name("square", side))
      x <- sq[upper.tri(sq)]
      assign(file_name("cor", side), x)
    }
    cor_12 <- cor(cor_1, cor_2)
    text <- square_labels_hc[upper.tri(square_labels_hc)]
    n = length(text)
    data <- data.frame("cor_1" = vector(mode="numeric", length=n), "cor_2" = vector(mode="numeric", length=n), "text" = vector(mode="character", length=n))
    rownames(data) <- square_names_hc[upper.tri(square_names_hc)]
    data$cor_1 <- cor_1
    data$cor_2 <- cor_2
    if(!grepl("var", type))
    {
      data$cor_1 <- round(data$cor_1,3)
      data$cor_2 <- round(data$cor_2,3)
    }
    data$text <- text
    names(data) <- c("cor_1", "cor_2", "text")
    ifelse(grepl("var", type), lims_xy <- list(c(0, max(cor_1)), c(0, max(cor_2))), lims_xy <- list(c(-1,1), c(-1,1)))
    p <- ggplot(data, aes(cor_1, cor_2, text = text)) + geom_point(alpha=.5) + geom_abline() + coord_cartesian(xlim = lims_xy[[1]], ylim = lims_xy[[2]]) + ggtitle(paste(input$demo_group, ", Correlation = ", round(cor_12,3), sep = "")) + xlab(Dictionnary_buttons_demographicsS[demo_1, "button"]) + ylab(Dictionnary_buttons_demographicsS[demo_2, "button"]) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figure
    if(generate_figures){ggsave(paste("./figures/figure6_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
    p <- ggplotly(p)
    p
})

