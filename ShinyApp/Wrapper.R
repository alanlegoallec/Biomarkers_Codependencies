source("helpers_apps.R")

ui <- fluidPage(
  navbarPage("Aging Biomarkers Co-Dependencies", id = "Home",
             tabPanel("Home", value = "Home_LandingPage"
             ),
             navbarMenu("Biomarkers",
                        tabPanel("Distributions - Baseline", value = "Biomarkers_Distributions"),
                        tabPanel("Distributions - Age changes", value = "Biomarkers_Trajectories"),
                        tabPanel("Distributions - Age changes", value = "Biomarkers_Age")
             ),
             navbarMenu("Correlations",
                        tabPanel("3D visualization of changes with age", value = "Correlations_3d"),
                        tabPanel("Linear Regressions", value = "Correlations_LinearRegressions"),
                        tabPanel("Histograms", value = "Correlations_Histograms"),
                        tabPanel("Hierarchical Clustering", value = "Correlations_HC"),
                        tabPanel("Heatmaps - Baseline", value = "Correlations_Heatmaps"),
                        tabPanel("Heatmaps - Age changes", value = "Correlations_Flats"),
                        tabPanel("Volcano Plots - Baseline", value = "Correlations_VolcanosBaseline"),
                        tabPanel("Volcano Plots - Age changes", value = "Correlations_Volcanos"),
                        tabPanel("Scatterplots - Baseline", value = "Correlations_Scatterplots"),
                        tabPanel("Scatterplots - Age changes", value = "Correlations_ScatterplotsAgingRates"),
                        tabPanel("Scatterplots - Age changes summaries", value = "Correlations_ScatterplotsSummaries")
             ),
             navbarMenu("Predictabilities",
                        tabPanel("Linear Regressions", value = "R2s_LinearRegressions"),
                        tabPanel("Histograms", value = "R2s_Histograms"),
                        tabPanel("Heatmaps - Age changes", value = "R2s_Flats"),
                        tabPanel("Volcano Plots - Baseline", value = "R2s_VolcanosBaseline"),
                        tabPanel("Volcano Plots - Age changes", value = "R2s_Volcanos"),
                        tabPanel("Scatterplots - Baseline", value = "R2s_Scatterplots"),
                        tabPanel("Scatterplots - Age changes", value = "R2s_ScatterplotsAgingRates"),
                        tabPanel("Scatterplots - Age changes summaries", value = "R2s_ScatterplotsSummaries")
             ),
             navbarMenu("Regression Coefficients",
                        tabPanel("Linear Regressions", value = "Coefficients_LinearRegressions"),
                        tabPanel("Histograms", value = "Coefficients_Histograms"),
                        tabPanel("Heatmaps - Baseline", value = "Coefficients_Heatmaps"),
                        tabPanel("Heatmaps - Age changes", value = "Coefficients_Flats"),
                        tabPanel("Volcano Plots - Baseline", value = "Coefficients_VolcanosBaseline"),
                        tabPanel("Volcano Plots - Age changes", value = "Coefficients_Volcanos"),
                        tabPanel("Scatterplots - Baseline", value = "Coefficients_Scatterplots"),
                        tabPanel("Scatterplots - Age changes", value = "Coefficients_ScatterplotsAgingRates"),
                        tabPanel("Scatterplots - Age changes summaries", value = "Coefficients_ScatterplotsSummaries")
             )
  ),
  uiOutput("container"))


server = function(input, output, session) {
  output$container <- renderUI({
    folder <- gsub( "_.*$", "", input$Home)
    source(paste("servers", "/", folder, "/", input$Home, "_server", ".R", sep = ""), local = TRUE)
    source(paste("uis", "/", folder, "/", input$Home, "_ui", ".R", sep = ""), local=TRUE)$value
  })
}


shinyApp(ui, server)


