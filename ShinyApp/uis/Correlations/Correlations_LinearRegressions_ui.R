fluidPage(
titlePanel("Correlations: Linear Regressions"),

sidebarLayout(
sidebarPanel(
helpText("Generate linear regression plots to visualize how the correlations between pairs of biomarkers are changing with age in different demographic groups."),

selectInput("distance",
label = "Select the metric",
choices = buttons_distancesS),
selectInput("demo_group",
label = "Select the demographic group",
choices = buttons_demographicsS),

htmlOutput("Biomarker1"),

htmlOutput("Biomarker2"),

selectInput("type",
label = "Select the analysis",
choices = c("Correlations", "Variance of the correlations"))
),

mainPanel(plotOutput("plot_cor_lr"),
dataTableOutput("table_cor_lr"))
)
)
