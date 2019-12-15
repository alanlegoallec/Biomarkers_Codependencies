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

output$table_dists <- renderTable({
    table_demographics <- initiate_store(c("All", "Males", "Females"), c("All", "Whites", "Hispanics", "Blacks", "Others"))
    data <- data()
    print(mean(data$age_factor))
    print(names(data_raw))
    print(names(data_normed))
    if(input$normed)
    {
        for (sex in c("All", "Males", "Females"))
        {
            data_s <- data
            if(sex == "Males"){data_s <- data_s[which(data_s[, "female"] <= 0),]}
            if(sex == "Females"){data_s <- data_s[which(data_s[, "female"] > 0),]}
            for (ethnicity in c("All", "Whites", "Hispanics", "Blacks", "Others"))
            {
                data_se <- data_s
                if(ethnicity == "Whites"){data_se <- data[which(data_se[, "hispanic"] <= 0 & data_se[, "black"] <= 0 & data_se[, "other"] <= 0),]}
                if(ethnicity == "Hispanics"){data_se <- data_s[which(data_se[, "hispanic"] > 0),]}
                if(ethnicity == "Blacks"){data_se <- data_s[which(data_se[, "black"] > 0),]}
                if(ethnicity == "Others"){data_se <- data_s[which(data_se[, "other"] > 0),]}
                table_demographics[sex, ethnicity] <- nrow(data_se)
            }
        }
      write.table(table_demographics, "./tables/table1.txt", sep=";", quote = FALSE)
      if(generate_figures){write.table(table_demographics, "./tables/table1.txt", sep=";", quote = FALSE)}
    } else {
        for (sex in c("All", "Males", "Females"))
        {
            data_s <- data
            if(sex == "Males"){data_s <- data_s[which(data_s$RIAGENDR == 1),]}
            if(sex == "Females"){data_s <- data_s[which(data_s$RIAGENDR == 2),]}
            for (ethnicity in c("All", "Whites", "Hispanics", "Blacks", "Others"))
            {
                data_se <- data_s
                if(ethnicity == "Whites"){data_se <- data_se[which(data_se$RIDRETH1 == 3),]}
                if(ethnicity == "Hispanics"){data_se <- data_se[which(data_se$RIDRETH1 %in% c(1,2)),]}
                if(ethnicity == "Blacks"){data_se <- data_se[which(data_se$RIDRETH1 == 4),]}
                if(ethnicity == "Others"){data_se <- data_se[which(data_se$RIDRETH1 == 5),]}
                table_demographics[sex, ethnicity] <- nrow(data_se)
            }
        }
    }
    table_demographics
},
rownames = TRUE, digits = 0, caption = "Sample size for the different demographic groups in the dataset.")

output$plots_dists <- renderUI({
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
      biomarker <- Dictionnary_names_labels[which(Dictionnary_names_labels$label == input$Biomarker), "name"]
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
        data <- data[which(data$age_factor >= age_min_i & data$age_factor < age_max_i),]
      } else {
        if(sex_i == "male"){data <- data[which(data$RIAGENDR == 1),]
        } else if(sex_i == "female"){data <- data[which(data$RIAGENDR == 2),]}
        if(ethnicity_i == "white"){data <- data[which(data$RIDRETH1 == 3),]
        } else if(ethnicity_i == "hispanic"){data <- data[which(data$RIDRETH1 %in% c(1,2)),]
        } else if(ethnicity_i == "black"){data <- data[which(data$RIDRETH1 == 4),]
        } else if(ethnicity_i == "other"){data <- data[which(data$RIDRETH1 == 5),]}
        data <- data[which(data$age_factor >= age_min_i & data$age_factor < age_max_i),]
      }
      names(data)[which(names(data) == biomarker)] <- "biomarker"
      mx <- max(data$biomarker)
      mn <- min(data$biomarker)
      binwidth = (mx - mn)/100
      plot_title <- paste("Plot ", local_i, ", Sex = ", sex_i, ", Ethnicity = ", ethnicity_i, ", ", input$Biomarker, ", min = ", round(mn,2), " , max = ", round(mx,2), ", mean = ", round(mean(data$biomarker),2), ", median = ", round(median(data$biomarker),2), ", Number of samples = ", nrow(data), sep = "")
      p <- ggplot(data=data) + geom_histogram(aes(x=data$biomarker, y=..density.., weight = data$weights), binwidth = binwidth) + xlab(input$Biomarker) + ggtitle(plot_title) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
      p
    })
  })
}

