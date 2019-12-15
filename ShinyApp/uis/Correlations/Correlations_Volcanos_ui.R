fluidPage(
titlePanel("Correlations: Volcano plots"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate interactive volcano plots to visualize how correlations are changing with age in different demographic groups."),
      
      selectInput("distance",
                  label = "Select the metric",
                  choices = buttons_distancesS),
      selectInput("demo_group",
                  label = "Select the demographic group",
                  choices = buttons_demographicsS),
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Correlations", "Variance of the correlations"))
    ),
    
    mainPanel(plotlyOutput("plot_cor_vol"),
              dataTableOutput("table_cor_vol")
    )
  )
)

