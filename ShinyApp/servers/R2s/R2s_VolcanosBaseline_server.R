output$Age_range <- renderUI({
  selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = Age_ranges_R2s[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
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
         "Ethnicity Demographics - Control Comparison" = "CE1vsCE2")
})

method <- reactive({
  switch(input$method,
                 "One model built for each age range" = "sw",
                 "Single model built on all the samples" = "sm")
})

normed <- reactive({ifelse(input$normed, "normed", "")})

half <- reactive({
  switch(input$half,
               "Training" = "train",
               "Testing" = "test")
})

side <- reactive({
  switch(input$side,
               "1st half" = "1",
               "2nd half" = "2",
               "Both halves merged" = "12")
})

data <- reactive({
  data <- redRDS("R2s", "R2S", normed(), method(), demo_group(), half(), side())
  data
})

data_var <- reactive({
  data_var <- redRDS("R2s", "R2Svar", method(), demo_group(), half(), side())
  data_var
})

data_p <- reactive({
  data_p <- data.frame(2*pnorm(abs(as.matrix(data())), 0, sqrt(as.matrix(data_var())), lower.tail = FALSE)*nrow(data()))
  names(data_p) <- Age_ranges_R2s[[demo_group()]]
  data_p
})

data_a <- reactive({
  age_range <- input$age_range
  data_a <- data()[,input$age_range]
  data_var_a <- data_var()[,input$age_range]
  data_p_a <- data_p()[,input$age_range]
  data_a <- data.frame(cbind(data_a, -log10(data_p_a)))
  data_a$text <- labels_hc
  names(data_a) <- c("R2s", "nlp", "text")
  rownames(data_a) <- labels_hc
  if(grepl("vs", demo_group())){data_a <- cbind(data_a, data_12_a())}
  data_a
})

data_12_a <- reactive({
  demo_1 <- Dictionnary_groups_comps[demo_group(), "group1"]
  demo_2 <- Dictionnary_groups_comps[demo_group(), "group2"]
  data_1 <- redRDS("R2s", "R2S", normed(), method(), demo_1, half(), side())
  data_2 <- redRDS("R2s", "R2S", normed(), method(), demo_2, half(), side())
  data_12_a <- data.frame(cbind(data_1[,input$age_range], data_2[,input$age_range]))
  names(data_12_a) <- c(Dictionnary_buttons_demographicsS[demo_1, "button"], Dictionnary_buttons_demographicsS[demo_2,"button"])
  data_12_a
})

output$plot_r2s_vb <- renderPlotly({
  data_a <- data_a()
  if(max(data_a$nlp == Inf)){data_a$nlp[which(data_a$nlp == Inf)] <- 308} #use the max nlp value that can be encoded:308
  #plot
  p <- ggplot(data_a, aes(x=R2s, y=nlp, text = text)) + geom_point(alpha=.5, size = size_dots) + geom_hline(yintercept = -log10(0.05), size=size_lines) + ggtitle(paste("Baseline R2s, ", input$demo_group, sep = "")) + xlab("R2s") + ylab("neg-log-p-value")  + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  #save figures
  if(generate_figures){ggsave(paste("./figures/figure_r2_7_", demo_group(), ".pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")}
  pi <- ggplotly(p)
  pi
})

output$table_r2s_vb <- renderDataTable({
  data <- data_a()
  data <- data[,-which(names(data) %in% c("p", "text"))]
  data <- data[order(data$R2s),]
  data$R2s <- round(data$R2s, 3)
  data$nlp <- round(data$nlp, 1)
  if(grepl("vs", demo_group()))
  {
    data[,3] <- round(data[,3], 3)
    data[,4] <- round(data[,4], 3)
  }
  data
}, rownames = TRUE)

