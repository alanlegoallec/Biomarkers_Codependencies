fluidPage(
  titlePanel("Coefficients: Volcano plots - Baseline"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate volcano plots to visualize how the baseline R2s are different between demographic groups."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      checkboxInput("normed", "Normalized: substracts the 20-80 Coefficients.", value = FALSE)
    ),
    
    mainPanel(plotlyOutput("plot_coef_vb"),
              dataTableOutput("table_coef_vb"))
  )
)
