fluidPage(
  titlePanel("Correlations: Volcano plots - Baseline"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate volcano plots to visualize how the baseline correlations are different between demographic groups."),
      selectInput("distance", label = "Select the metric", choices = buttons_distancesS),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      checkboxInput("normed", "Normalized: substracts the 20-80 R2s results.", value = FALSE)
    ),
    
    mainPanel(plotlyOutput("plot_cor_vb"),
              dataTableOutput("table_cor_vb"))
  )
)
