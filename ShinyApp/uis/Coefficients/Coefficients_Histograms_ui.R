fluidPage(
  titlePanel("Coefficients: Histograms"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate histograms to vizualize how the distribution of the coefficients is changing with age."),
      
      selectInput("demo_group",
                  label = "Select the demographic group",
                  choices = buttons_demographicsS),
      htmlOutput("Age_range"),
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Coefficients", "Variances of the coefficients")),
      uiOutput("abs"),
      checkboxInput("significant", "Consider only the coefficients significantly changing with age.", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot_coef_hg"),
              plotOutput("summaryplot_coef_hg"))
  )
)
