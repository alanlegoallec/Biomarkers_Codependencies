fluidPage(
titlePanel("Correlations: Hierarchical clustering"),

sidebarLayout(
sidebarPanel(
helpText("Hierarchical clustering of the biomarkers in the different demographic groups."),

sliderInput("n_plots", "Number of plots", value=1, min=1, max=10, ticks = FALSE),

uiOutput("lists_inputs")
),

mainPanel(uiOutput("plots_hc"))
)
)
