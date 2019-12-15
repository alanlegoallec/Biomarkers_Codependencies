fluidPage(
  titlePanel("Coefficients: Scatterplots Summaries"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate plots to summarize how the correlations of the regressions coefficients between two groups are changing with age."),
      selectInput(inputId = "demo_group", label = "Select the demographic group", choices = list("Sexes", "Ethnicities")),
      selectInput("type", label = "Select the analysis", choices = c("Coefficients", "Variance of the coefficients")),
      checkboxInput("normed", "Normed: substracts the 20-80 column.", value = FALSE)),
    
    mainPanel(plotOutput("plothm_coef_sps"),
              plotOutput("plotsp_coef_sps"),
              tableOutput("table_coef_sps"))
  )
)