#Square project postprocessing, sliding window

args <- c("O2", 1)
machine <- "O2"
n_cores <- 1

#load helpers, libraries, parameters
ifelse(machine == "O2", path <- "/home/al311/Aging/", path <- "/Users/Alan/Desktop/Aging/")
source(file = paste(path, "Square/Scripts/Helpers.R", sep = ""))
source(file = paste(path, "Square/Scripts/Square_helpers.R", sep = ""))

#COEFFICIENTS
#generate
generate_coefficients_sw()  #<- par  demo_groups_sw age
#plot
plot_heatmaps_coefficients_sw()  #<- par demo_groups_swS
#generate gifs
generate_gifs_coefficients() #<- par demo_groups_swS

#CORRELATION SQUARES
#generate comparisons
generate_correlations_groups_comparison_sw() #<- distances
generate_correlations_distances_comparison_sw() # demo_groups_swS
#plot
generate_correlations_plots_sw() #demo_groups_swS, distancesS
#generate gifs
generate_gifs_correlations_squares() #demo_groups_swS, distancesS

#R2S
#Compile by demo groups and age ranges
R2s_compile_by_demo_groups() #halves, sidesS
R2s_compile_by_age_ranges() #halves, sidesS
#plots
R2s_plot_by_demo_groups() #halves, sidesS demo_groups_swS
R2s_plot_by_age_ranges() #halves, sidesS c("R2S", "R2S_normed") c("demo_groups_swS", "demo_groups_sw", "groups_comps_sw")
R2s_correlations_sw() #halves, sidesS
#generate gifs
generate_gifs_R2s() #halves, sidesS







