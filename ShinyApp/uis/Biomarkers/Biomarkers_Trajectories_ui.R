fluidPage(
  titlePanel("Biomarkers: Trajectories"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize and compare the trajectories of the biomarkers in the different demographic groups."),
      selectInput("Biomarker", label = "Select the biomarker", choices = c(Choose = '', list_labels), selectize = TRUE),
      checkboxInput("normed", "Normalized: after centering, scaling, and non linear transformations.", value = FALSE),
      sliderInput("n_plots", "Number of plots", value=1, min=1, max=15, ticks = FALSE),
      uiOutput("lists_sexes"),
      uiOutput("lists_ethnicities"),
      uiOutput("lists_age_ranges")
    ),
    
    mainPanel(uiOutput("plots_traj"))
  )
)
