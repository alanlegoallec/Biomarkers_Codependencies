output$Age_range <- renderUI({
    ifelse(input$type == "Variances of the coefficients", choices <- Age_ranges_Coefficients_equalbins, choices <- Age_ranges_Coefficients)
    selectInput(
    inputId = "age_range",
    label = "Select the Age Range",
    choices = choices[[Dictionnary_buttons_demographicsS[which(Dictionnary_buttons_demographicsS$button == input$demo_group), "group"]]],
    selected = "20-80")
})

output$abs <- renderUI({
    if(input$type == "Coefficients"){checkboxInput("abs", "Take the absolute values of the coefficients", value = TRUE)}
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
    "Coefficients" = "Coefficients",
    "Variances of the coefficients" = "Coefficients_var")
})

data <- reactive({
  ifelse(type() == "Coefficients_var", rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  display <- ifelse(type() == "Coefficients_var" & grepl("vs", demo_group()), "display", "")
  data <- rdRDS("Coefficients", type(), display, "flat_target", demo_group())
  if(input$abs){data <- abs(data)}
  if(input$significant)
  {
    significants <- rdRDS("Coefficients", type(), "significances", demo_group())
    data <- data[rownames(significants[significants$significant,]),]
  }
  data
})

data_histo <- reactive({
  ifelse((grepl("var", type()) | grepl("vs", demo_group())), rdRDS <- redRDS_equalbins, rdRDS <- redRDS)
  type3 <- ifelse(input$abs, "abs", "signed")
  type4 <- ifelse(input$significant, "significant", "")
  data_histo <- rdRDS("Coefficients","data_histo", type(), type3, type4, demo_group())
  data_histo
})

output$plot_coef_hg <- renderPlot({
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
    p
})


output$summaryplot_coef_hg <- renderPlot({
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
    plot_title <- paste("Meta regression", paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", format(-log10(pv), scientific = TRUE, digits = 2), sep = " "), paste("young =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("old =", format(coef, scientific = TRUE, digits=2), sep = " "), sep = ", ")
    p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=size_bars) + ggtitle(plot_title) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
    #save figure
    if(generate_figures){ggsave("./figures/figure_coef_4.pdf", plot = p, device = "pdf", dpi = 600, height = 20, width = 40, units = "cm")}
  } else {
    model <- lm(mean~age, data = data_histo)
    coef <- summary(model)$coefficients["age","Estimate"]
    pv <- summary(model)$coefficients["age", 4]
    intercept <- summary(model)$coefficients["(Intercept)","Estimate"]
    young <- intercept + 20*coef
    old <- intercept + 80*coef
    #weighted linear regression plot
    plot_title <- paste("Meta regression", paste("Coef =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("neg-log-p-value =", format(-log10(pv), scientific = TRUE, digits = 2), sep = " "), paste("young =", format(coef, scientific = TRUE, digits=2), sep = " "), paste("old =", format(coef, scientific = TRUE, digits=2), sep = " "), sep = ", ")
    p <- ggplot(data = data_histo, aes(age, mean)) + geom_point(size = size_dots, alpha=.5) + geom_abline(intercept = intercept, slope = coef, color="red", size=size_lines) + ggtitle(plot_title) + theme(plot.title = element_text(size=size_titles), axis.title.x = element_text(size = size_axis), axis.title.y = element_text(size = size_axis), axis.text.x = element_text(size=size_ticks), axis.text.y = element_text(size=size_ticks))
  }
  p
})

