fluidPage(
  titlePanel("Coefficients: Scatterplots"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate interactive scatter plots to visualize the differences in the ways regression coefficients are changing with age in different demographic groups."),
      selectInput(inputId = "demo_group", label = "Select the demographic group", choices = buttons_groups_comps),
      htmlOutput("Age_range"),
      selectInput("type", label = "Select the analysis", choices = c("Coefficients", "Variance of the coefficients"))),
    
    mainPanel(plotlyOutput("plot_coef_sp"))
  )
)

