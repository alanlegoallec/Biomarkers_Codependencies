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

output$plots_traj <- renderUI({
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
            title_p <- paste("Plot ", local_i, ", Sex = ", Dictionnary_buttons_demographicsS[sex_i,"button"], ", Ethnicity = ", Dictionnary_buttons_demographics_preprocessing[ethnicity_i,"button"], sep = "")
            p <- ggplot(data, aes(x = age_factor, y = biomarker, group = age_factor, weight = weights)) + geom_boxplot(width = 0.3, outlier.size = 1) + xlab("Age") + ylab(input$Biomarker) + ggtitle(title_p) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
            if(generate_figures){ggsave(paste("./figures/figure0X_top.pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
            p
        })
    })
}


