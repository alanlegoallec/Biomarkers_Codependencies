fluidPage(
  titlePanel("R2s: Scatterplots Summaries"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate plots to summarize how the correlations of the R2s between two groups are changing with age."),
      selectInput(inputId = "demo_group", label = "Select the demographic group", choices = list("Sexes", "Ethnicities")),
      selectInput("method", label = "Select the method", choices = c("One model built for each age range", "Single model built on all the samples")),
      checkboxInput("normed", "Normed: substracts the 20-80 column.", value = FALSE),
      selectInput("type", label = "Select the analysis", choices = c("R2s", "Variance of the R2s")),
      selectInput("half", label = "Training or Testing", choices = list("Training", "Testing"), selected = "Testing"),
      selectInput("side", label = "Samples used", choices = list("1st half", "2nd half", "Both halves merged"), selected = "Both halves merged")),
    
    mainPanel(plotOutput("plothm_r2s_sps"),
              plotOutput("plotsp_r2s_sps"),
              tableOutput("table_r2s_sps"))
  )
)
