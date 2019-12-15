output$Age_range <- renderUI({
  ifelse(input$type == "Variance of the coefficients", choices <- Age_ranges_Coefficients_equalbins, choices <- Age_ranges_Coefficients)
  selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$plot_coef_sp <- renderPlotly({
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
                 "Coefficients" = "Coefficients",
                 "Variance of the coefficients" = "Coefficients_var")
  category <- Dictionnary_categories[demo_group, "category"]
  ifelse(type == "Coefficients_var", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  for (i in seq(2))
  {
    assign(file_name("name", as.character(i)), Dictionnary_groups_comps[demo_group, i])
    assign(file_name("square", as.character(i)), rdRDS("Coefficients", type, Dictionnary_groups_comps[demo_group, i], input$age_range))
  }
  for (side in sides)
  {
    sq <- get(file_name("square", side))
    x <- concatenate_coefficients(sq)
    assign(file_name("coefs", side), x)
  }
  coefs_12 <- cor(coefs_1, coefs_2)
  n = length(concatenated_labels_target_hc)
  data <- data.frame("coefs_1" = vector(mode="numeric", length=n), "coefs_2" = vector(mode="numeric", length=n), "text" = vector(mode="character", length=n))
  rownames(data) <- concatenated_names_target_hc
  data$coefs_1 <- coefs_1
  data$coefs_2 <- coefs_2
  data$text <- concatenated_labels_target_hc
  names(data) <- c(file_name("coefs", name_1), file_name("coefs", name_2), "text")
  p <- ggplot(data, aes(coefs_1, coefs_2, text = text)) + geom_point(alpha=.5) + geom_abline() + ggtitle(paste(input$demo_group, ", Correlation = ", round(coefs_12,3), sep = "")) + xlab(Dictionnary_buttons_demographicsS[demo_1, "button"]) + ylab(Dictionnary_buttons_demographicsS[demo_2, "button"]) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  if(generate_figures){ggsave(paste("./figures/figure_coef_6_", demo_group, ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  p
})
