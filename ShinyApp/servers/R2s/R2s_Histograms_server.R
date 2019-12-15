output$Age_range <- renderUI({
    ifelse(input$type == "Variances of the R2s", choices <- Age_ranges_R2s_equalbins, choices <- Age_ranges_R2s)
    selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
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

type <- reactive({
  switch(input$type,
         "R2s" = "R2S",
         "Variance of the R2s" = "R2Svar")
})

method <- reactive({
  switch(input$method,
         "One model built for each age range" = "sw",
         "Single model built on all the samples" = "sm")
})

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
  ifelse(type() == "R2Svar", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  display <- ifelse(type() == "R2Svar" & grepl("vs", demo_group()), "display", "")
  data <- redRDS_equalbins("R2s", type(), display, method(), demo_group(), half(), side())
  if(input$significant)
  {
    significants <- rdRDS("R2s", type(), "significances", method(), demo_group(), half(), side())
    data <- data[rownames(significants[significants$significant,]),]
  }
  data
})

data_histo <- reactive({
  ifelse((grepl("var", type()) | grepl("vs", demo_group())), rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  type4 <- ifelse(input$significant, "significant", "")
  data_histo <- rdRDS("R2s","data_histo", type(), type4, method(), demo_group(), half(), side())
  data_histo
})

output$plot_r2s_hg <- renderPlot({
    data <- data()
    mn <- min(data[,-1])
    mx <- max(data[,-1])
    binwidth = (mx - mn)/100
    data_a <- data[,input$age_range, drop = FALSE]
    names(data_a) <- "values"
    if(input$age_range == "20-80")
    {
      mn <- min(data_a)
      mx <- max(data_a)
      binwidth = (mx - mn)/100
    }
    p <- ggplot(data=data_a) + geom_histogram(aes(data_a$values), binwidth = binwidth) + xlab(input$type) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figure
    if(generate_figures)
    {
      ggsave(paste("./figures/figure_r2_1.pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
    }
    p
})


output$summaryplot_r2s_hg <- renderPlot({
  data_histo <- data_histo()
  if(min(data_histo$var > 0))
  {
    model <- rma(mean, var, mods =  ~ age, data=data_histo, control=list(maxiter=10000))
    coef <- model$beta[2]
    pv <- model$pval[2]
    intercept <- model$beta[1]
    young <- intercept + 20*coef
    old <- intercept + 80*coef
    #weighted linear regression plot
    plot_title <- paste("Meta regression", paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", format(-log10(pv), scientific = TRUE, digits = 2), sep = " "), paste("young =", round(young, 3), sep = " "), paste("old =", round(old, 3), sep = " "), sep = ", ")
    p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=size_bars) + ggtitle(plot_title) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  } else {
    model <- lm(mean~age, data = data_histo)
    coef <- summary(model)$coefficients["age","Estimate"]
    pv <- summary(model)$coefficients["age", 4]
    intercept <- summary(model)$coefficients["(Intercept)","Estimate"]
    young <- intercept + 20*coef
    old <- intercept + 80*coef
    #weighted linear regression plot
    plot_title <- paste("Meta regression", paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", format(-log10(pv), scientific = TRUE, digits = 2), sep = " "), paste("young =", round(young, 3), sep = " "), paste("old =", round(old, 3), sep = " "), sep = ", ")
    p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(plot_title) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  }
  #save figure
  if(generate_figures)
  {
    ggsave(paste("./figures/figure_r2_4.pdf", sep = ""), plot = p, device = "pdf", dpi = 600, height = 20, width = 20, units = "cm")
  }
  print(pv)
  p
})
