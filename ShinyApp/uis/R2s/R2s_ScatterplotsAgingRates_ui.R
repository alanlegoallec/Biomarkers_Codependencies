fluidPage(
  titlePanel("R2s: Scatterplots - Aging Rates"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate scatterplots to compare the age-dependent R2s changing rates between two demographic groups."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_groups_comps),
      selectInput("method", label = "Select the method", choices = c("One model built for each age range", "Single model built on all the samples")),
      selectInput("type", label = "Select the analysis", choices = c("R2s", "Variance of the R2s")),
      selectInput("half", label = "Training or Testing", choices = list("Training", "Testing"), selected = "Testing"),
      selectInput("side", label = "Samples used", choices = list("1st half", "2nd half", "Both halves merged"), selected = "Both halves merged")
    ),
    mainPanel(plotlyOutput("plot_r2s_sar"))
  )
)
