output$lists_sexes <- renderUI({
  n_plots <- as.integer(input$n_plots)
  lapply(1:n_plots, function(i) {
    div(style="display:inline-block", radioButtons(paste("sexes", i, sep = "_"), paste("Sexes", i, sep = " "), choiceNames = c("All", "Males", "Females"), choiceValues = c("all", "male", "female"), selected = "all", width = 90))
  })
})

output$lists_ethnicities <- renderUI({
  n_plots <- as.integer(input$n_plots)
  lapply(1:n_plots, function(i) {
    div(style="display:inline-block", radioButtons(paste("ethnicities", i, sep = "_"), paste("Ethnicities", i, sep = " "), choiceNames = c("All", "Whites", "Hispanics", "Blacks", "Others"), choiceValues = c("all", "white", "hispanic", "black", "other"), selected = "all", width = 90))
  })
})

output$lists_age_ranges <- renderUI({
  n_plots <- as.integer(input$n_plots)
  lapply(1:n_plots, function(i) {
    div(style="display:inline-block", sliderInput(paste("age_range", i, sep = "_"), paste("Age Range", i, sep = " "), min = 20, max = 80, value = c(20,80), width = 90))
  })
})

data <- reactive({
  ifelse(input$normed, data <- data_normed, data <- data_raw)
  data
})

biomarker <- reactive({
  Dictionnary_names_labels[which(Dictionnary_names_labels$label == input$Biomarker), "name"]
})

output$plots_age <- renderUI({
  n_plots <- as.integer(input$n_plots)
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
      sex_i <- input[[paste("sexes", local_i, sep = "_")]]
      ethnicity_i <- input[[paste("ethnicities", local_i, sep = "_")]]
      age_min_i <- input[[paste("age_range", local_i, sep = "_")]][1]
      age_max_i <- input[[paste("age_range", local_i, sep = "_")]][2]
      if(input$normed)
      {
        if(sex_i == "male"){data <- data[which(data[, "female"] <= 0),]
        } else if(sex_i == "female"){data <- data[which(data[, "female"] > 0),]}
        if(ethnicity_i == "white"){data <- data[which(data[, "hispanic"] <= 0 & data[, "black"] <= 0 & data[, "other"] <= 0),]
        } else if(ethnicity_i == "hispanic"){data <- data[which(data[, "hispanic"] > 0),]
        } else if(ethnicity_i == "black"){data <- data[which(data[, "black"] > 0),]
        } else if(ethnicity_i == "other"){data <- data[which(data[, "other"] > 0),]}
      } else {
        if(sex_i == "male"){data <- data[which(data$RIAGENDR == 1),]
        } else if(sex_i == "female"){data <- data[which(data$RIAGENDR == 2),]}
        if(ethnicity_i == "white"){data <- data[which(data$RIDRETH1 == 3),]
        } else if(ethnicity_i == "hispanic"){data <- data[which(data$RIDRETH1 %in% c(1,2)),]
        } else if(ethnicity_i == "black"){data <- data[which(data$RIDRETH1 == 4),]
        } else if(ethnicity_i == "other"){data <- data[which(data$RIDRETH1 == 5),]}
      }
      data <- data[which(data$age_factor >= age_min_i & data$age_factor < age_max_i),]
      data <- data[,c("age_factor", biomarker(), "weights")]
      names(data) <- c("age_factor", "biomarker", "weights")
      cor <- cov.wt(cbind(data$age_factor, data$biomarker), wt = data$weights, cor = TRUE, center = TRUE)$cor[1,2]
      model <- lm(age_factor ~ biomarker, data, weights = data$weights)
      coef <- summary(model)$coefficients["biomarker", "Estimate"]
      pv <- summary(model)$coefficients["biomarker", "Pr(>|t|)"]
      title_p <- paste(paste("Plot", local_i, sep = " "), paste("Sex = ", Dictionnary_buttons_demographicsS[sex_i,"button"], sep = ""), paste("Ethnicity = ", Dictionnary_buttons_demographics_preprocessing[ethnicity_i,"button"], sep = ""), paste("Coefficient =", format(coef, digits=2, scientific=TRUE), sep = " "), paste("p-value =", format(pv, digits=2, scientific=TRUE), sep = " "), paste("Correlation =", round(cor,3), sep = " "), sep = ", ")
      p <- ggplot(data = data, aes(age_factor, biomarker)) + geom_point(size = size_dots, alpha=.5) + geom_smooth(method='lm') + ggtitle(title_p) + xlab("Age") + ylab(input$Biomarker) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
      p
    })
  })
}

output$plot_vol <- renderPlotly({
  data <- redRDS("Preprocessing", "Age_changes_table")
  data <- data[,-which(names(data) == "Significant")]
  names(data) <- c("Coefficients", "nlp", "Correlations")
  if(max(data$nlp == Inf)){data$nlp[which(data$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
  data$text <- paste(rownames(data), ", Correlation = ", round(data$Correlations,3), paste = "")
  p <- ggplot(data, aes(x=Coefficients, y=nlp, text = text)) + geom_point(size = size_dots, alpha=.5) + geom_hline(yintercept = -log10(0.05)) + ggtitle("Significance of the age dependence of the biomarkers") + xlab("Coefficient") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figure
  if(generate_figures){ggsave(paste("./figures/figure_0XA.pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  pi <- ggplotly(p)
  pi
})

output$table_age <- renderDataTable({
  data <- redRDS("Preprocessing", "Age_changes_table")
  data$Coefficients <- format(data$Coefficients, scientific = TRUE, digits = 2)
  if(max(data$`neg-log-corrected-p-values` == Inf)){data$`neg-log-corrected-p-values`[which(data$`neg-log-corrected-p-values` == Inf)] <- 308} #use the max nlp value that can be encoded:308
  data$`neg-log-corrected-p-values` <- round(data$`neg-log-corrected-p-values`, 1)
  data$Correlations <- round(data$Correlations, 3)
  #save table
  if(generate_figures){write.table(data, "./tables/table0.txt", sep=";", quote = FALSE)}
  data
}, rownames = TRUE)


