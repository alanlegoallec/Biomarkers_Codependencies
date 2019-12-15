fluidPage(
titlePanel("Correlations: Scatterplots"),

sidebarLayout(
sidebarPanel(
helpText("Generate interactive scatter plots to visualize the differences in the ways correlations are changing with age in different demographic groups, or using different metrics."),

selectInput("distance",
label = "Select the metric",
choices = buttons_distancesS),
htmlOutput("demo_group"),
htmlOutput("Age_range"),
selectInput("type",
label = "Select the analysis",
choices = c("Correlations", "Variance of the correlations"))),

mainPanel(plotlyOutput("plot_cor_sp"))
)
)

