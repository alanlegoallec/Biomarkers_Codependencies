fluidPage(
  titlePanel("R2s: Histograms"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate histograms to vizualize how the distribution of the R2s is changing with age."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      selectInput("method", label = "Select the method", choices = c("One model built for each age range", "Single model built on all the samples")),
      selectInput("type", label = "Select the analysis", choices = c("R2s", "Variance of the R2s")),
      checkboxInput("significant", "Consider only the R2s significantly changing with age.", value = FALSE),
      selectInput("half", label = "Training or Testing", choices = list("Training", "Testing"), selected = "Testing"),
      selectInput("side", label = "Samples used", choices = list("1st half", "2nd half", "Both halves merged"), selected = "Both halves merged")
    ),
    
    mainPanel(plotOutput("plot_r2s_hg"),
              plotOutput("summaryplot_r2s_hg"))
  )
)
