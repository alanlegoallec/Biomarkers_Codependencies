fluidPage(
  titlePanel("Coefficients: Linear Regressions"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate linear regression plots to visualize how the regression coefficients are changing with age in different demographic groups."),
      
      selectInput("demo_group",
                  label = "Select the demographic group",
                  choices = buttons_demographicsS),
      
      htmlOutput("Target"),
      
      htmlOutput("Predictor"),
      
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Coefficients", "Variance of the coefficients"))
    ),
    
    mainPanel(plotOutput("plot_coef_lr"),
              dataTableOutput("table_coef_lr"))
  )
)
