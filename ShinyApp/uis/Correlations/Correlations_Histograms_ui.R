fluidPage(
  titlePanel("Correlations: Histograms"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate histograms to vizualize how the distribution of the correlations is changing with age."),
      
      selectInput("distance",
                  label = "Select the metric",
                  choices = buttons_distancesS),
      selectInput("demo_group",
                  label = "Select the demographic group",
                  choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Correlations", "Variances of the correlations")),
      uiOutput("abs"),
      checkboxInput("significant", "Consider only the correlations significantly changing with age.", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot_cor_hg"),
              plotOutput("summaryplot_cor_hg"))
  )
)
