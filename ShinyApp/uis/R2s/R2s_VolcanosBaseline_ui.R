fluidPage(
  titlePanel("R2s: Volcano plots - Baseline"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate volcano plots to visualize how the baseline R2s are different between demographic groups."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      selectInput("method", label = "Select the method", choices = c("One model built for each age range", "Single model built on all the samples")),
      checkboxInput("normed", "Normalized: substracts the 20-80 R2s results.", value = TRUE),
      selectInput("half", label = "Training or Testing", choices = list("Training", "Testing"), selected = "Testing"),
      selectInput("side", label = "Samples used", choices = list("1st half", "2nd half", "Both halves merged"), selected = "Both halves merged")
    ),
    
    mainPanel(plotlyOutput("plot_r2s_vb"),
              dataTableOutput("table_r2s_vb"))
  )
)
