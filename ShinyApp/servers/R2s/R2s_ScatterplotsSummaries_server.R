category <- reactive({switch(input$demo_group,
                               "All samples" = "General",
                               "Sexes" = "sexes",
                               "Ethnicities" = "ethnicities")
})

method <- reactive({switch(input$method,
                           "One model built for each age range" = "sw",
                           "Single model built on all the samples" = "sm")
})
type <- reactive({switch(input$type,
                         "R2s" = "R2S",
                         "Variance of the R2s" = "R2Svar")
})
half <- reactive({switch(input$half,
                         "Training" = "train",
                         "Testing" = "test")
})
side <- reactive({switch(input$side,
                         "1st half" = "1",
                         "2nd half" = "2",
                         "Both halves merged" = "12")
})

data <- reactive({
  ifelse(input$normed, string <- "correlations_normed", string <- "correlations")
  ifelse(type() == "R2S", data <- redRDS("R2s", type(), string, method(), category(), half(), side()), data <- redRDS_equalbins("R2s", type(), string, method(), category(), half(), side()))
  data
})

output$plothm_r2s_sps <- renderPlot({
  data <- data()
  ifelse(input$normed, lims <- c(-max(abs(data), na.rm = TRUE), 0, max(abs(data), na.rm = TRUE)), lims <- c(min(data, na.rm = TRUE), (min(data, na.rm = TRUE)+max(data, na.rm = TRUE))/2, max(data, na.rm = TRUE)))
  #picture
  melted_comp <- melt(as.matrix(data))
  names(melted_comp) <- c("Group_Comparison", "Age_Range", "value")
  p <- ggplot(data = melted_comp, aes(Age_Range, Group_Comparison, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = lims[2], limit = lims[-2], space = "Lab", name="R2") + theme_minimal() + coord_fixed() + theme(axis.text.x = element_text(angle = 90, vjust = 1, size = size_labels_heatmaps, hjust = 1), axis.text.y = element_text(vjust = 1, size = size_labels_heatmaps, hjust = 1)) + coord_fixed() + ggtitle("Change in the correlations of the biomarkers predictabilities between groups with age")
  p
})

output$plotsp_r2s_sps <- renderPlot({
  data <- data()
  ifelse(grepl("VS", rownames(data)), demo <- demo_group(), demo <- rownames(data)[1])
  data <- melt(data.frame(t(data))[-1,])
  if(ncol(data) ==1){data$variable <- distance()}
  data$Age <- Age_means_R2s_equalbins[[demo]][-1]
  data <- data[,c("Age", "value", "variable")]
  names(data) <- c("Age", "value", "Group_Comparison")
  ifelse(grepl("Difference", input$distance), data$Group_Comparison <- Dictionnary_buttons_distancesS[data$Group_Comparison,"button"], data$Group_Comparison <- Dictionnary_buttons_demographicsS[levels(data$Group_Comparison)[data$Group_Comparison],"button"])
  p <- ggplot(data = data, aes(Age, value, color = Group_Comparison)) + geom_point(alpha=.5, size=size_dots) + geom_smooth(method='lm') + ggtitle("Change in the correlations of the biomarkers predictabilities between groups with age") + xlab("Age") + ylab("Correlation of predictabilities") + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks), legend.position="bottom", legend.box = "horizontal")
  if(generate_figures){ggsave("./figures/figure_MvsF_corr2s.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  p
})

output$table_r2s_sps <- renderTable({
  #load data
  data <- data()
  #perform calculations
  demos <- rownames(data)
  data <- data.frame(t(data))
  data$age <- Age_means_R2s_equalbins[[names(data)[1]]]
  data <- data[-which(rownames(data) == "20-80"),]
  significances <- initiate_store(demos, c("R2", "p-value", "young", "old"))
  for (demo in demos)
  {
    data_i <- data
    names(data_i)[which(names(data_i) == demo)] <- "demo"
    model <- lm(demo~age, data = data_i)
    significances[demo,c("R2", "p-value")] <- summary(model)$coefficients["age", c("Estimate","Pr(>|t|)")]
    intercept_i <- summary(model)$coefficients["(Intercept)", "Estimate"]
    coef_i <- summary(model)$coefficients["age", "Estimate"]
    significances[demo,"young"] <- round(intercept_i + 20*coef_i,3)
    significances[demo,"old"] <- round(intercept_i + 80*coef_i,3)
  }
  #display
  significances[,"R2"] <- format(significances[,"R2"], scientific = TRUE, digits = 2)
  significances[,"p-value"] <- format(significances[,"p-value"], scientific = TRUE, digits = 2)
  rownames(significances) <- Dictionnary_buttons_demographicsS[rownames(significances), "button"]
  significances
},
rownames = TRUE, digits = 3, caption = "Changes in the correlation of the R2s between the groups with age.")


output$plots_r2s_sps <- renderUI({
  data <- data()
  n_plots <- as.integer(nrow(data))
  plot_output_list <- lapply(1:n_plots, function(i) {
    plotname <- paste("plot", i, sep="")
    plotOutput(plotname)
  })
  
  do.call(tagList, plot_output_list)
})
for (i in 1:max_plots_traj) {
  local({
    local_i <- i
    plotname <- paste("plot", local_i, sep="")
    output[[plotname]] <- renderPlot({
      data <- data()
      ifelse(grepl("VS", rownames(data)), index <- demo_group(), index <- rownames(data)[local_i])
      data_i <- data.frame(cbind(Age_means_R2s_equalbins[[index]][-1], as.vector(t(data[local_i,])[-1])))
      names(data_i) <- c("Age", "Cor")
      model <- lm(Cor~Age, data = data_i)
      coef <- summary(model)$coefficients["Age", "Estimate"]
      pv <- summary(model)$coefficients["Age", "Pr(>|t|)"]
      string <- ifelse(grepl("VS", rownames(data)[local_i]), input$demo_group, rownames(data)[local_i])
      p <- ggplot(data = data_i, aes(Age, Cor)) + geom_point(alpha=.5) + geom_smooth(method='lm') + ggtitle(paste("R2s", string, paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", round(-log10(pv), 1), sep = " "), sep = ", ")) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
      p
    })
  })
}

