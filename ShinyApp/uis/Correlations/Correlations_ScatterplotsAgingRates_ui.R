fluidPage(
titlePanel("Correlations: Scatterplots - Aging Rates"),

sidebarLayout(
sidebarPanel(
helpText("Generate interactive scatter plots to compare the rates at which the correlations are changing with age in different demographic groups, or using different metrics."),

selectInput("distance",
label = "Select the metric",
choices = buttons_distancesS),
htmlOutput("demo_group"),
selectInput("type",
label = "Select the analysis",
choices = c("Correlations", "Variance of the correlations"))),

mainPanel(plotlyOutput("plot_cor_sar"))
)
)

