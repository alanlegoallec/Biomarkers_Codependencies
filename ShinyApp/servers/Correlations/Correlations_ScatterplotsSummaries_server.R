output$demo_group <- renderUI({
  ifelse(grepl("Difference", input$distance), choices_demographics <- buttons_demographicsS, choices_demographics <- list("Sexes", "Ethnicities"))
  selectInput(
    inputId = "demo_group",
    label = "Select the demographic group",
    choices = choices_demographics)
})

distance <- reactive({
  switch(input$distance,
         "Pearson Correlation" = "biomarkers",
         "Pearson Correlation between the linear regression coefficients" = "coefficients",
         "Differences between the two distances" = "bmVScoef")
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
         "Ethnicity Demographics - Control Comparison" = "CE1vsCE2",
         "All samples" = "General",
         "Sexes" = "sexes",
         "Ethnicities" = "ethnicities")
})

type <- reactive({
  switch(input$type,
         "Correlations" = "Correlations",
         "Variance of the correlations" = "Correlations_var")
})

data <- reactive({
  distance <- ifelse(grepl("Difference", input$distance), "", distance())
  demo_group <- demo_group()
  type <- type()
  normed <- ifelse(input$normed, "normed", "")
  string <- ifelse(grepl("Difference", input$distance), "correlationsdistances", "correlationsgroups")
  data <- redRDS_equalbins("Correlations", type, string, normed, distance, demo_group)
  data
})

output$plothm_cor_sps <- renderPlot({
  data <- data()
  ifelse(grepl("Difference", input$distance), rownames(data) <- Dictionnary_buttons_distancesS[rownames(data), "button"], rownames(data) <- Dictionnary_buttons_demographicsS[rownames(data), "button"])
  ifelse(input$normed, lims <- c(-max(abs(data), na.rm = TRUE), 0, max(abs(data), na.rm = TRUE)), lims <- c(min(data, na.rm = TRUE), (min(data, na.rm = TRUE)+max(data, na.rm = TRUE))/2, max(data, na.rm = TRUE)))
  #picture
  melted_comp <- melt(as.matrix(data))
  names(melted_comp) <- c("Group_Comparison", "Age_Range", "value")
  p <- ggplot(data = melted_comp, aes(Age_Range, Group_Comparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = size_labels_heatmaps, hjust = 1), axis.text.y = element_text(vjust = 1, size = size_labels_heatmaps, hjust = 1)) + coord_fixed() + ggtitle("Change in the correlations of the correlations between groups with age")
  p
})

output$plotsp_cor_sps <- renderPlot({
  data <- data()
  ifelse(grepl("VS", rownames(data)), demo <- demo_group(), demo <- rownames(data)[1])
  data <- melt(data.frame(t(data))[-1,])
  if(ncol(data) ==1){data$variable <- distance()}
  data$Age <- Age_means_Correlations_equalbins[[demo]][-1]
  data <- data[,c("Age", "value", "variable")]
  names(data) <- c("Age", "value", "Group_Comparison")
  ifelse(grepl("Difference", input$distance), data$Group_Comparison <- Dictionnary_buttons_distancesS[data$Group_Comparison,"button"], data$Group_Comparison <- Dictionnary_buttons_demographicsS[levels(data$Group_Comparison)[data$Group_Comparison],"button"])
  p <- ggplot(data = data, aes(Age, value, color = Group_Comparison)) + geom_point(alpha=.5, size=size_dots) + geom_smooth(method='lm') + ggtitle("Change in the correlations of the correlations between groups with age") + xlab("Age") + ylab("Correlation of correlations") + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks), legend.position="bottom", legend.box = "horizontal")
  if(generate_figures){ggsave("./figures/figure_MvsF_corcor.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  p
})

output$table_cor_sps <- renderTable({
  #load data
  data <- data()
  #perform calculations
  demos <- rownames(data)
  data <- data.frame(t(data))
  ifelse(grepl("Difference", input$distance), data$age <- Age_means_Correlations_equalbins[[demo_group()]], data$age <- Age_means_Correlations_equalbins[[names(data)[1]]])
  data <- data[-which(rownames(data) == "20-80"),]
  significances <- initiate_store(demos, c("Coefficient", "p-value", "young", "old"))
  for (demo in demos)
  {
    data_i <- data
    names(data_i)[which(names(data_i) == demo)] <- "demo"
    model <- lm(demo~age, data = data_i)
    significances[demo,c("Coefficient", "p-value")] <- summary(model)$coefficients["age", c("Estimate","Pr(>|t|)")]
    intercept_i <- summary(model)$coefficients["(Intercept)", "Estimate"]
    coef_i <- summary(model)$coefficients["age", "Estimate"]
    significances[demo,"young"] <- round(intercept_i + 20*coef_i,3)
    significances[demo,"old"] <- round(intercept_i + 80*coef_i,3)
  }
  #display
  significances[,"Coefficient"] <- format(significances[,"Coefficient"], scientific = TRUE, digits = 2)
  significances[,"p-value"] <- format(significances[,"p-value"], scientific = TRUE, digits = 2)
  ifelse(grepl("Difference", input$distance), rownames(significances) <- Dictionnary_buttons_distancesS[rownames(significances), "button"], rownames(significances) <- Dictionnary_buttons_demographicsS[rownames(significances), "button"])
  significances
},
rownames = TRUE, digits = 3, caption = "Changes in the correlation of the correlations between the groups/measures with age.")


