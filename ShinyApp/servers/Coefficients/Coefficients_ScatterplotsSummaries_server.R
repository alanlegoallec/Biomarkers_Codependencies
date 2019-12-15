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
         "Coefficients" = "Coefficients",
         "Variance of the coefficients" = "Coefficients_var")
})

data <- reactive({
  demo_group <- demo_group()
  ifelse(input$normed, normed <- "normed", normed <- "")
  data <- redRDS_equalbins("Coefficients", type(), "correlations", normed, demo_group)
  data
})

output$plothm_coef_sps <- renderPlot({
  data <- data()
  ifelse(input$normed, lims <- c(-max(abs(data), na.rm = TRUE), 0, max(abs(data), na.rm = TRUE)), lims <- c(min(data, na.rm = TRUE), (min(data, na.rm = TRUE)+max(data, na.rm = TRUE))/2, max(data, na.rm = TRUE)))
  #picture
  melted_comp <- melt(as.matrix(data))
  names(melted_comp) <- c("Group_Comparison", "Age_Range", "value")
  p <- ggplot(data = melted_comp, aes(Age_Range, Group_Comparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="Coefficient") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 4, hjust = 1), axis.text.y = element_text(vjust = 1, size = 4, hjust = 1)) + coord_fixed()
  p
})

output$plotsp_coef_sps <- renderPlot({
  data <- data()
  ifelse(grepl("VS", rownames(data)), demo <- demo_group(), demo <- rownames(data)[1])
  data <- melt(data.frame(t(data))[-1,])
  if(ncol(data) ==1){data$variable <- distance()}
  data$Age <- Age_means_Coefficients_equalbins[[demo]][-1]
  data <- data[,c("Age", "value", "variable")]
  names(data) <- c("Age", "value", "Group_Comparison")
  ifelse(grepl("Difference", input$distance), data$Group_Comparison <- Dictionnary_buttons_distancesS[data$Group_Comparison,"button"], data$Group_Comparison <- Dictionnary_buttons_demographicsS[levels(data$Group_Comparison)[data$Group_Comparison],"button"])
  p <- ggplot(data = data, aes(Age, value, color = Group_Comparison)) + geom_point(alpha=.5, size=size_dots) + geom_smooth(method='lm') + ggtitle("Change in the correlations of the regression coefficients between groups with age") + xlab("Age") + ylab("Correlation of coefficients") + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks), legend.position="bottom", legend.box = "horizontal")
  if(generate_figures){ggsave("./figures/figure_MvsF_corcoef.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  p
})

output$table_coef_sps <- renderTable({
  #load data
  data <- data()
  #perform calculations
  demos <- rownames(data)
  data <- data.frame(t(data))
  data$age <- Age_means_Coefficients_equalbins[[names(data)[1]]]
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
  rownames(significances) <- Dictionnary_buttons_demographicsS[rownames(significances), "button"]
  significances
},
rownames = TRUE, digits = 3, caption = "Changes in the correlation of the coefficients between the groups with age.")

