fluidPage(
titlePanel("Coefficients: Volcano plots"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate interactive volcano plots to visualize how the regression coefficients are changing with age in different demographic groups."),

      selectInput("demo_group",
                  label = "Select the demographic group",
                  choices = buttons_demographicsS),
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Coefficients", "Variance of the coefficients"))
    ),
    
    mainPanel(plotlyOutput("plot_coef_vol"),
              dataTableOutput("table_coef_vol")
    )
  )
)

