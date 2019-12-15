fluidPage(
titlePanel("Biomarkers: Distribution"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Vizualize the distribution of the different biomarkers in different demographic groups."),
      selectInput("Biomarker",
                  label = "Select the biomarker",
                  choices = c(Choose='', list_labels),
                  selectize = TRUE),
      checkboxInput("normed", "Normalized: the biomarkers were transformed to have a more normal distribution.", value = FALSE),
      sliderInput("n_plots", "Number of plots", value=1, min=1, max=15, ticks = FALSE),
      uiOutput("lists_sexes"),
      uiOutput("lists_ethnicities"),
      uiOutput("lists_age_ranges")
    ),
    
    mainPanel(uiOutput("plots_dists"),
              tableOutput("table_dists"))
  )
)
