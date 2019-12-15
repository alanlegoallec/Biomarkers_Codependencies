fluidPage(
  titlePanel("Coefficients: Scatterplots - Aging Rates"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate interactive scatterplots to compare the rates at which regression coefficients are changing with age in different demographic groups."),
      selectInput(inputId = "demo_group", label = "Select the demographic group", choices = buttons_groups_comps),
      selectInput("type", label = "Select the analysis", choices = c("Coefficients", "Variance of the coefficients"))),
    
    mainPanel(plotlyOutput("plot_coef_sar"))
  )
)

