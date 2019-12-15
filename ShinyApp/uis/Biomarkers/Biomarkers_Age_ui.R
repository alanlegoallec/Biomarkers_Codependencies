fluidPage(
  titlePanel("Biomarkers = f(Age)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Generate regression plots to see how the biomarkers change with age."),
      selectInput("Biomarker", label = "Select the biomarker", choices = c(Choose = '', list_labels), selectize = TRUE),
      checkboxInput("normed", "Normalized: after centering, scaling, and non linear transformations.", value = FALSE),
      sliderInput("n_plots", "Number of plots", value=1, min=1, max=15, ticks = FALSE),
      uiOutput("lists_sexes"),
      uiOutput("lists_ethnicities"),
      uiOutput("lists_age_ranges")
    ),
    
    mainPanel(uiOutput("plots_age"),
              plotlyOutput("plot_vol"),
              dataTableOutput("table_age"))
  )
)