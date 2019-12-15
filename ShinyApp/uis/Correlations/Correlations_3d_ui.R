fluidPage(
  titlePanel("3D plots for correlations changes with age"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Generate 3D plots of the changing correlation between two biomarkers with age."),
      selectInput("demo_group", label = "Select the demographic group", choices = buttons_demographics_other),
      htmlOutput("Biomarker1"),
      htmlOutput("Biomarker2"),
      sliderInput("age_range", "Age range:",  min = 20, max = 80, value = c(20,80)),
      checkboxInput("normed", "Normalized: the biomarkers were transformed to have a more normal distribution.", value = FALSE)
      ),
    
    mainPanel(plotOutput("plot_3d"),
              rglwidgetOutput("rglPlot_3d"))
  )
)
