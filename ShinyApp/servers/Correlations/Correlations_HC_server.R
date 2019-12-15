output$lists_inputs <- renderUI({
    n_plots <- as.integer(input$n_plots)
    lapply(1:n_plots, function(i) {
        selectInput(paste("demo_group", i, sep = "_"),
        label = paste("Select the demographic group ", i, sep = ""),
        choices = buttons_demographics_and_ages)
    })
})

output$plots_hc <- renderUI({
    n_plots <- as.integer(input$n_plots)
    plot_output_list <- lapply(1:n_plots, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname)
    })
    
    do.call(tagList, plot_output_list)
})

for (i in 1:max_plots) {
    local({
        local_i <- i
        plotname <- paste("plot", local_i, sep="")
        output[[plotname]] <- renderPlot({
            demo_group_i  <- switch(input[[paste("demo_group", local_i , sep = "_")]],
            "All" = "all",
            "Young" = "young",
            "Old" = "old",
            "Males" = "male",
            "Females" = "female",
            "Sex Demographics - Control group 1" = "controlS1",
            "Sex Demographics - Control group 2" = "controlS2",
            "Whites" = "white",
            "Hispanics" = "hispanic",
            "Blacks" = "black",
            "Ethnicity Demographics - Control group 1" = "controlE1",
            "Ethnicity Demographics - Control group 2" = "controlE2")
            hc <- redRDS("Preprocessing", "Hierchical_clustering_labels", demo_group_i)
            #print
            if(generate_figures)
            {
              pdf("figures/figure1A.pdf", height = 15, width = 10)
              plot(hc)
              pvrect(hc, alpha=0.95)
              dev.off()
              # bitmap("figures/figure1A.tiff", type = "tiff24nc", height = 30, width = 20, units = 'cm', res=600)
              # plot(hc)
              # pvrect(hc, alpha=0.95)
              # dev.off()
            }
            plot(hc, main = input[[paste("demo_group", local_i , sep = "_")]])
            pvrect(hc, alpha=0.95)
        })
    })
}

