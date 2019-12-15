fluidPage(
  titlePanel("Aging Biomarkers Co-Dependencies"),
  jumbotron_size("Introduction", "<p> This shiny app presents the results of our paper \"Age-dependent co-dependency structure of biomarkers in healthy human populations\". We used 50 biomarkers sampled from 27,508 healthy individuals aged from 20 to 80 years old from the NHANES cohort to investigate how the co-dependency of human biomarkers changes with age. The biomarkers are blood biomarkers (e.g cholesterol, glucose, albumin), urine biomarkers (e.g urine albumin) and anthropometrics (e.g height, BMI, blood pressure)<p>
<p> The results are organized in four parts: \"Biomarkers\", \"Correlations\", \"Predictabilities\" and \"Regression coefficients\". For each of those indicators, we calculated baseline values (using data from the full age range) and examined the trajectories with age. For both baseline values and age trajectories, we compared the results between sexes and ethnicities (Non-Hispanic Whites, Hispanics and Non-Hispanic Blacks).<p>
<p> For the details about each of those indicators, please see the tabs on the navigation bar, and advice on how to best use the features of the app in the respective window below. <p>
                 ", size_header=2, button = FALSE),
  hr(),
  fluidRow(
    jumbotron_size("Biomarkers", "<p>Hypothesis: the values of the biomarkers change with age.<p>
                             <p>Results: 92% of the 50 biomarkers significantly change with age. The biomarker that increases the most with age is systolic blood pressure. <p>"
                   , size_header=2, buttonID = "biomarkers", buttonLabel = "Details"),
    jumbotron_size("Correlations", "<p>Hypothesis: the correlations between biomarkers change with age. <p>
<p> Results: 33% of the 1225 correlations significantly change with age. The mean absolute value of the correlations significantly decreases with age. The correlation between height and systolic blood pressure strongly decreases with age (correlation=0.4 at age 20 versus -0.3 at age 80). Strong baseline differences are observed between sexes and between ethnicities, as well as some differences in term of age trajectories. <p>"
                   , size_header=2, buttonID = "correlations", buttonLabel = "Details"),
    jumbotron_size("Predictabilities", "<p> Hypothesis: the predictabilities of the biomarkers change with age. (We definine \"predictability\" as the prediction accuracy obtained when predicting the values of a biomarkerhow from the values of all the other biomarkers using an elastic net.) <p>
               <p>Results: 44% of the 50 biomarkers see their predictability significantly change with age. Albumin becomes less predictable with age (R2=0.55 at age 20 versus 0.00 at age 80), while serum glucose becomes more predictable with age (R2=0.11 at age 20 versus 0.66 at age 80). The mean predictability significantly change with age. Strong baseline differences are observed between sexes and between ethnicities, as well as some differences in term of age trajectories. <p>"
                   , size_header=2, buttonID = "predictabilities", buttonLabel = "Details"), 
    jumbotron_size("Regression coefficients", "<p> Hypothesis: the predictors selected when predicting the 50 biomarkers and the values of their 2450 coefficients change with age. We calculate those coefficients by using an elastic net to predict the value of each biomarker, using the remaining 49 biomarkers as predictors.<p>
               <p>Results: we found that 1.6% of the 2450 regression coefficients significantly change with age. Few differences are found between sexes and between ethnicities."
                   , size_header=2, buttonID = "regressioncoefficients", buttonLabel = "Details")
  ),
  
  
  jumbotron_size("Discussion", "<p> In summary, we have have shown that the co-dependency structure of biomarkers changes as humans age. Further, we have evidence to support that the structure is different in males and females and between ethnicities. In general, differences between sexes are stronger than differences between ethnicities, and more differences were found between non-Hispanic Whites and non-Hispanic Blacks than between non-Hispanic Whites and Hispanics. <p>
<p> We found that 92% of the biomarkers, 33% of the pairwise correlations, 44% of the predictabilities and 1.6% of the regression coefficients show significant change with age. <p>
<p> Demographics played an influential role. First we found significant differences between sexes  at baseline (44% of the correlations, 30% of the prediction accuracies and 2.9% of the regression coefficients). In term of age trajectories we found that sex influences 2.7% of the correlations, 10% of the predictabilities and 0.4% of the regression coefficients. Last we found fewer baseline and trajectories differences between non-Hispanic Whites and non-Hispanic Blacks, and even fewer differences between non-Hispanic Whites and Hispanics.<p>
<p> Some of our results may be prone to misinterpretation due to generation bias. For example, the physiological indicator that was the most negatively correlated with age was upper leg length. This can be explained by the fact that people who are currently old are shorter than they would have been if they had had access to contemporary diets and quality of life of later generations. People who are old in 2018 have shorter upper leg length than young people in 2018, but they do not have shorter upper leg leg than in their youth. Using a longitudinal cohort, we would therefore not observe this generational bias. <p>
<p> Our strongest finding regarding changes in correlations with age was the correlation between height and systolic blood pressure. Interestingly, the correlation decreases from 0.39 for young people to -0.28 for old people, but it only decreases from 0.09 to -0.13 in males, and from 0.07 to -0.20 in females. A potential explanation includes, on average males remain taller than their female counterparts in every age group; however, young males have in average higher blood pressure than young females, whereas older males have in average lower blood pressure than their female counterparts. <p>
<p> We hypothesize that the shrinkage of the relationship between biomarkers may be indicative of the aging process. Specifically, we found that, overall, the correlation between biomarkers and their predictability significantly decreases  with age. These findings could also be explained by the accumulation of the effects of the environment as we age. For example, we found that albumin levels could not be predicted in old people; however, on the other hand, 55% of its variance can be explained in young people. Albumin levels are affected by both the diet and several common diseases, such as liver failure, heart failure, kidney damage or enteropathy. Potentially cumulative effects with time influence the liver more than other organ systems as humans age. <p>
                              ", size_header=2, button = FALSE),
  
  bsModal("Biomarkers", "Biomarkers", "biomarkers", size = "large",
          HTML("<p> The results relative to the biomarkers can be found under the \"Biomarkers\" tab in the navigation bar. This window provides a description of the analyzes that can be found in the different subtabs, as well as explanations concerning the parameters that can be chosen in those subtabs. <p>
               <h1>Analyzes</h1>
               <h3>Distributions - Baseline</h3>
               <p>Displays the distribution of the biomarkers. It is possible to compare those distributions between several demographic groups. <p>
               <h3>Distributions - Age changes</h3>
               <p>Displays the trajectories of the distributions of the biomarkers with age. It is possible to compare those trajectories and distributions between demographics.<p>
               <h3>Aging biomarkers</h3>
               <p>Linear regression and correlation values between age and the selected biomarker. Below, a volcano plot and a table summarize the strength and the significance of the association between each biomarker and age.<p>

               <h1>Parameters</h1>
               <h5>Biomarker</h5>
               <p>Biomarker to analyse, to be chosen among the 50 available.<p>
               <h5>Normalized</h5>
               <p>Whether or not the biomarkers were log transformed, centered and scaled.<p>
               <h5>Number of plots</h5>
               <p>Number of plots to display. Allow comparison between different demographics.<p>
               <h5>Sexes</h5>
               <p>Choose which sexes to include in each of the plots.<p>
               <h5>Ethnicities</h5>
               <p>Choose which ethnicities to include in each of the plots.<p>
               <h5>Age ranges</h5>
               <p>Choose the age range to evaluate in each of the plots.<p>
               ")
  ),
  
  bsModal("Correlations", "Correlations", "correlations", size = "large",
          HTML("<p> The results relative to the correlations can be found under the \"Correlations\" tab in the navigation bar. This window provides a description of the analyzes that can be found in the different subtabs, as well as explanations concerning the parameters that can be chosen in those subtabs. <p>
               <h1>Analyzes</h1>
               <h3>3D visualisation of changes with age</h3>
               <p>Display the correlation and a linear regression between the two selected biomarkers, as well as its changes with age in an interactive 3D plot.<p>
               <h3>Linear Regressions</h3>
               <p>Vizualize the change in the correlation between a chosen pair of biomarker with age. A table of the correlations and their association with age can be found below the linear regression plot.<p>
               <h3>Histograms</h3>
               <p>Distribution of the correlations and changes in the mean of the correlations with age.<p>
               <h3>Hierarchical Clustering</h3>
               <p>Hierarchical clustering performed on the correlations. For each cluster, the number in green is the bootstrap probability--the percentage of bootstraps in which the cluster was present. The number in red is called the approximated-unbiased p-value. AU is a better estimation of the unbiased p-value than BP, and the red boxes circle the significant clusters, based on this criteria, with alpha=0.95 (a cluster is marked as significant if its AU is greater than 95). <p>
               <h3>Heatmaps - Baseline </h3>
               <p>Vizualize the correlation matrix using heatmaps. If a comparison between two groups is selected (e.g Males vs Females), the upper left triangle of the heatmap are the values of the correlations for the first group (e.g males) and the lower right triangle of the heatmap is the difference between the correlations in the second group and the correlations in the first group (e.g correlations(females) - correlations(males)). <p>
               <h3>Heatmaps - Age changes</h3>
               <p>Heatmap showing how the correlations between a chosen biomarkers and all the remaining ones change with age.<p>
               <h3>Volcano Plots - Baseline</h3>
               <p>Volcano plots showing which correlations are significantly different from zero. If a comparison between two demographic groups is selected, the volcano plot displays the differences between the correlations in the two groups and their significances. Below the volcano plot a table report the values for each of the 1225 correlation.<p>
               <h3>Volcano Plots - Age changes</h3>
               <p>Volcano plots showing which correlations are significantly changing with age. If a comparison between two demographic groups is selected, the volcano plot shows which correlations are changing at significantly different rates in the two groups. Below the volcano plot a table reports the values for each of the 1225 correlation.<p>
               <h3>Scatterplots - Baseline</h3>
               <p>Scatterplots showing the correlations of the 1225 correlations between the two groups being compared.<p>
               <h3>Scatterplots - Age changes</h3>
               <p>Scatterplots showing the correlations of the rates of change of the correlations between the two groups being compared, only including the correlations which significantly changed in both compared groups.<p>
               <h3>Scatterplots - Age changes summary</h3>
               <p>Heatmaps showing how the correlation of the 1225 biomarkers correlations between demographic groups change with age. Below the heamaps, linear regressions are quantifying this trend. We found that we were underpowered to draw significant conclusions from this analysis. In theory it would for example allow us to determine if, we age, the differences between males and females tend to decrease.<p>

               <h1>Parameters</h1>
               <h5>Metric</h5>
               <p>The metric to use to calculate the correlation between two biomarkers. The results we reported in the paper were exclusively computed using the pearson correlation, which is also the metric by default when displaying results with our app. Alternatively, we looked at the correlations between our biomarkers in a different way: we first calculated the regression coefficients by predicting each biomarker using the remaining 49 biomarkers and an elastic net. Then to compute the correlation between two biomarkers, we used the correlation between the regression coefficients for the 48 biomarkers that were used to predict both of them. Finally, it is possible to display the difference between the results obtained using the Pearson correlation, and the results obtained using a Pearson correlation between the linear regression coefficients. <p>
               <h5>Demographic group</h5>
               <p>The demographic subset of the samples on which to run the analysis. The differences between the values obtained in two demographic groups can be observed by selecting \"Group1\" vs \"Group1\". The results displayed are the values for the second group minus the value for the first group. (e.g Males vs Females corresponds to values(Females) - values(Males).) <p>
               <h5>Age ranges</h5>
               <p>Choose the range of the ages to include in the analysis.<p>
               <h5>Biomarkers</h5>
               <p> The first and the second biomarkers on which to compute the correlation. <p>
               <h5>Analysis</h5>
               <p>Display the correlations (default) or the variance of the correlations. We hypothesize that the variance of the correlations between biomarkers change with age. We tested this hypothesis but came to the conclusion that we are underpower to draw significant conclusions. Results should therefore be analyzed with caution, but we make them available in this shiny app.<p>
               <h5>Normalized</h5>
               <p>Whether or not to log transform, center and scale the biomarkers.<p>
               <h5>Normed</h5>
               <p>Substract the baseline results (the analysis ran using the full age range, selecting all the samples) to the other results. This option makes it easier to notice changes with age.<p>
               <h5>Absolute values of the correlations</h5>
               <p>Use the signed values or the absolute values of the correlations.<p>
               <h5>Correlations significantly changing with age only</h5>
               <p>Include all the correlations in the analysis, or limit the analysis to the correlations for which a significant change with age was detected.<p>
               <h5>Number of plots</h5>
               <p>Number of plots to display. Allows comparison between different demographics.<p>
               ")
  ),
  
  bsModal("Predictabilities", "Predictabilities", "predictabilities", size = "large",
          HTML("<p> The results relative to the predictabilities can be found under the \"Predictabilities\" tab in the navigation bar. This window provides a description of the analyzes that can be found in the different subtabs, as well as explanations concerning the parameters that can be chosen in those subtabs. <p>
               <h1>Analyzes</h1>
               <h3>Linear Regressions</h3>
               <p>Vizualize the change in the predictability of a chosen biomarker with age. A table of the biomarkers'predictability and their association with age can be found below the linear regression plot.<p>
               <h3>Histograms</h3>
               <p>Distribution of the predictabilities and changes in the mean of the predictabilities with age.<p>
               <h3>Heatmaps</h3>
               <p>Heatmap showing how the predictabilities of the biomarkers change with age.<p>
               <h3>Volcano Plots - Baseline</h3>
               <p>Volcano plots showing which predictabilities are significantly different from zero. If a comparison between two demographic groups is selected, the volcano plot displays the differences between the predictabilities in the two groups and their significances. Below the volcano plot a table report the values for each of the 50 biomarkers predictabilities.<p>
               <h3>Volcano Plots - Age changes</h3>
               <p>Volcano plots showing which predictabilities are significantly changing with age. If a comparison between two demographic groups is selected, the volcano plot shows which predictabilities are changing at significantly different rates in the two groups. Below the volcano plot a table reports the values for each of the 50 biomarkers.<p>
               <h3>Scatterplots - Baseline</h3>
               <p>Scatterplots showing the correlations of the 50 biomarkers predictabilities between the two groups being compared.<p>
               <h3>Scatterplots - Age changes</h3>
               <p>Scatterplots showing the correlations of the rates of change of the predictabilities between the two groups being compared, only including the predictabilities which significantly changed in both compared groups.<p>
               <h3>Scatterplots - Age changes summary</h3>
               <p>Heatmaps showing how the correlation of the 50 biomarkers predictabilities between demographic groups change with age. Below the heamaps, linear regressions are quantifying this trend. We found that we were underpowered to draw significant conclusions from this analysis. In theory it would for example allow us to determine if, we age, the differences between males and females tend to decrease.<p>
               
               <h1>Parameters</h1>
               <h5>Demographic group</h5>
               <p>The demographic subset of the samples on which to run the analysis. The differences between the values obtained in two demographic groups can be observed by selecting \"Group1\" vs \"Group1\". The results displayed are the values for the second group minus the value for the first group. (e.g Males vs Females corresponds to values(Females) - values(Males).) <p>
               <h5>Age ranges</h5>
               <p>Choose the range of the ages to include in the analysis.<p>
               <h5>Biomarkers</h5>
               <p> The target biomarker, whose values are being predicted. <p>
               <h5>Method</h5>
               <p>Whether the predictabilities were estimated by building a model on each age bin, or by building a single model on all the samples and then looking at how it performed on the different age bins.
               <h5>Analysis</h5>
               <p>Display the predictabilities (default) or the variance of the predictabilities. We hypothesize that the variance of the predictabilities between biomarkers change with age. We tested this hypothesis but came to the conclusion that we are underpower to draw significant conclusions. Results should therefore be analyzed with caution, but we make them available in this shiny app.<p>
               <h5>Normed</h5>
               <p>Substract the baseline results (the analysis ran using the full age range, selecting all the samples) to the other results. This option makes it easier to notice changes with age.<p>
               <h5>R2s significantly changing with age only</h5>
               <p>Include all the predictabilities in the analysis, or limit the analysis to the predictabilities for which a significant change with age was detected.<p>
               <h5>Training or Testing</h5>
               <p>Use the prediction accuracies obtained on the training set or on the testing set.<p>
               <h5>Samples used to calculate the predictability of the biomarker(s)</h5>
               <p>We slit the data into two halves of the same size. We used the first half as the training set and the second half as the testing set, then switched them around. This way each sample has been used once as a testing sample and has an associated prediction. It is possible to select which halves are included in the calculation of the predictability.<p>
               ")
  ),
  
  bsModal("RegressionCoefficients", "Regression Coefficients", "regressioncoefficients", size = "large",
          HTML("<p> The results relative to the regression coefficients can be found under the \"Regression Coefficients\" tab in the navigation bar. This window provides a description of the analyzes that can be found in the different subtabs, as well as explanations concerning the parameters that can be chosen in those subtabs. <p>
               <h1>Analyzes</h1>
               <h3>Linear Regressions</h3>
               <p>Vizualize the change in the regression coefficient for a chosen target biomarker and a chosen predictor biomarker. A table of the regression coefficients and their association with age can be found below the linear regression plot.<p>
               <h3>Histograms</h3>
               <p>Distribution of the regression coefficients and changes in the mean of the regression coefficients with age.<p>
               <h3>Heatmaps - Baseline </h3>
               <p>Vizualize the regression coefficients using heatmaps. Each column corresponds to a target, and each row to a predictor.<p>
               <h3>Heatmaps - Age changes</h3>
               <p>Heatmap showing how all the predictors for a chosen target biomarker change with age, or how a chosen biomarker used as a predictor see the values of its coefficients change with age when used to predict each of the other biomarkers.<p>
               <h3>Volcano Plots - Baseline</h3>
               <p>Volcano plots showing which regression coefficients are significantly different from zero. If a comparison between two demographic groups is selected, the volcano plot displays the differences between the regression coefficients in the two groups and their significances. Below the volcano plot a table report the values for each of the 2450 regression coefficients.<p>
               <h3>Volcano Plots - Age changes</h3>
               <p>Volcano plots showing which regression coefficients are significantly changing with age. If a comparison between two demographic groups is selected, the volcano plot shows which regression coefficients are changing at significantly different rates in the two groups. Below the volcano plot a table reports the values for each of the 2450 regression coefficients.<p>
               <h3>Scatterplots - Baseline</h3>
               <p>Scatterplots showing the correlations of the 2450 regression coefficients between the two groups being compared.<p>
               <h3>Scatterplots - Age changes</h3>
               <p>Scatterplots showing the correlations of the rates of change of the regression coefficients between the two groups being compared, only including the regression coefficients which significantly changed in both compared groups.<p>
               <h3>Scatterplots - Age changes summary</h3>
               <p>Heatmaps showing how the correlation of the 2450 regression coefficients between demographic groups change with age. Below the heamaps, linear regressions are quantifying this trend. We found that we were underpowered to draw significant conclusions from this analysis. In theory it would for example allow us to determine if, we age, the differences between males and females tend to decrease.<p>
               
               <h1>Parameters</h1>
               <h5>Demographic group</h5>
               <p>The demographic subset of the samples on which to run the analysis. The differences between the values obtained in two demographic groups can be observed by selecting \"Group1\" vs \"Group1\". The results displayed are the values for the second group minus the value for the first group. (e.g Males vs Females corresponds to values(Females) - values(Males).) <p>
               <h5>Age ranges</h5>
               <p>Choose the range of the ages to include in the analysis.<p>
               <h5>Biomarkers</h5>
               <p> The first and the second biomarkers on which to compute the correlation. <p>
               <h5>Target or Predictor</h5>
               <p> Display the regression coefficients for which the biomarker was used as a target (default) or as a predictor. <p>
               
               <h5>Analysis</h5>
               <p>Display the regression coefficients (default) or the variance of the regression coefficients. We hypothesize that the variance of the regression coefficients change with age. We tested this hypothesis but came to the conclusion that we are underpower to draw significant conclusions. Results should therefore be analyzed with caution, but we make them available in this shiny app.<p>
               <h5>Normed</h5>
               <p>Substract the baseline results (the analysis ran using the full age range, selecting all the samples) to the other results. This option makes it easier to notice changes with age.<p>
               <h5>Absolute values of the correlations</h5>
               <p>Use the signed values or the absolute values of the regression coefficients.<p>
               <h5>Regression coefficients significantly changing with age only</h5>
               <p>Include all the regression coefficients in the analysis, or limit the analysis to the regression coefficients for which a significant change with age was detected.<p>
               ")
          )
  
)

  