fluidPage(
  titlePanel("R2s: Heatmaps"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate heatmaps to visualize how the R2s are changing with age in different demographic groups."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographicsS),
      selectInput("method", label = "Select the method", choices = c("One model built for each age range", "Single model built on all the samples")),
      checkboxInput("normed", "Normed: substracts the average R2s column.", value = TRUE),
      selectInput("type", label = "Select the analysis", choices = c("R2s", "Variance of the R2s")),
      selectInput("half", label = "Training or Testing", choices = list("Training", "Testing"), selected = "Testing"),
      selectInput("side", label = "Samples used", choices = list("1st half", "2nd half", "Both halves merged"), selected = "Both halves merged")
    ),
    
    mainPanel(plotlyOutput("plot_r2s_fh"))
  )
)

