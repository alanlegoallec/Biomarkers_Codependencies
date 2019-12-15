fluidPage(
  titlePanel("Correlations: Scatterplots Summaries"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate plots to summarize how the correlations of correlations between two groups/measures are changing with age."),
      selectInput("distance", label = "Select the metric", choices = buttons_distancesS),
      htmlOutput("demo_group"),
      selectInput("type", label = "Select the analysis", choices = c("Correlations", "Variance of the correlations")),
      checkboxInput("normed", "Normed: substracts the 20-80 column.", value = FALSE)),
    
    mainPanel(plotOutput("plothm_cor_sps"),
              plotOutput("plotsp_cor_sps"),
              tableOutput("table_cor_sps"))
  )
)
