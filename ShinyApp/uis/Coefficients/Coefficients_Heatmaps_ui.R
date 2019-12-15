fluidPage(
titlePanel("Coefficients: Squared Heatmaps"),

sidebarLayout(
sidebarPanel(
helpText("Generate an interactive squared heatmap to visualize the differences in the ways regression coefficients are changing with age in different demographic groups."),

selectInput("demo_group",
label = "Select the demographics",
choices = buttons_demographicsS),
htmlOutput("Age_range"),
selectInput("type",
label = "Select the analysis",
choices = c("Coefficients", "Variance of the coefficients")),
checkboxInput("normed", "Normed: substracts the 20-80 matrix.", value = TRUE),
verbatimTextOutput("text")
),

mainPanel(plotlyOutput("plot_coef_sh"))
)
)
