fluidPage(
titlePanel("Correlations: Flat Heatmaps"),

sidebarLayout(
sidebarPanel(
helpText("Generate an interactive heatmap to visualize the differences in the ways correlations are changing with age in different demographic groups, or using different metrics."),

selectInput("distance",
label = "Select the metric",
choices = buttons_distancesS),
selectInput("demo_group",
label = "Select the demographics",
choices = buttons_demographicsS),
selectInput("Biomarker", label = "Select the biomarker", choices = c(Choose='', list_labels)),
selectInput("type",
label = "Select the analysis",
choices = c("Correlations", "Variance of the correlations")),
checkboxInput("normed", "Normed: substracts the 20-80 column.", value = TRUE)
),

mainPanel(plotlyOutput("plot_cor_fh"))
)
)


