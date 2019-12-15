fluidPage(
  titlePanel("Coefficients: Flat Heatmaps"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate an interactive heatmap to visualize the differences in the ways regression coefficients are changing with age in different demographic groups."),
      
      selectInput("demo_group",
                  label = "Select the demographics",
                  choices = buttons_demographicsS),
      selectInput("Biomarker", label = "Select the biomarker", choices = c(Choose='', list_labels)),
      selectInput("type",
                  label = "Select the analysis",
                  choices = c("Coefficients", "Variance of the coefficients")),
      checkboxInput("normed", "Normed: substracts the 20-80 column.", value = TRUE),
      selectInput("display",
                  label = "Display the coefficients for which the selected biomarker was used as ",
                  choices = c("the target", "a predictor"))
    ),
    
    mainPanel(plotlyOutput("plot_coef_fh"))
  )
)


