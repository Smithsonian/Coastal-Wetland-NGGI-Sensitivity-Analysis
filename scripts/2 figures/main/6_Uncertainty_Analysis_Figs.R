# Create and export uncertainty and sensitivgity analysis tables
library(tidyverse)
library(gridExtra) # to combine ggplot graphs

# load up tables resulting from the 'Crunch Data from Monte Carlo Analysis' Script
# total_nggi_summed_iterations - the summed total emissions/removals for each of 10,000 simulations to be used in the big histogram
total_nggi_summed_iterations <- read_csv("data/outputTables/MonteCarloResults/total_nggi_summed_iterations_bothLmeanAndMean.csv")

#total_nggi_summed_summaries - summary statistics used to draw  95% CI's and determine the range of the graphs
total_nggi_summed_summaries <- read_csv("data/outputTables/MonteCarloResults/total_nggi_summed_summaries_bothLmeanAndMean.csv")

# salinity_stability_summed_iterations the summed emissions/removals for each of 10,000 simulations separated by salinity and 
#    stability classifications. Will be used in the small histograms.
salinity_stability_summed_iterations <- read_csv("data/outputTables/MonteCarloResults/salinity_stability_summed_iterations_bothLmeanAndMean.csv")

#salinity_stability_summed_summaries - summary statistics used to draw  95% CI's and determine the range of the graphs
salinity_stability_summed_summaries <- read_csv("data/outputTables/MonteCarloResults/salinity_stability_summed_summaries_bothLmeanAndMean.csv")

# Rename for consistency with other figure
total_nggi_summed_iterations_B <- total_nggi_summed_iterations %>%
  mutate(scaled_up_using = ifelse(scaled_up_using == "mean", "A Mean", "B Logmean"))
# Rename for consistency
total_nggi_summed_summaries_B <- total_nggi_summed_summaries %>%
  mutate(scaled_up_using = ifelse(scaled_up_using == "mean", "A Mean", "B Logmean"))


# Create Split Histogram where two panels show total emissions and removals separated by how they were scaled up
# make a histogram with both uncertainty analyses iterations summarised
sum_total_MillionTonnesCO2_histograms_meansLogmeans <- ggplot(data = total_nggi_summed_iterations_B, aes(x = sum_total_MillionTonnesCO2PerYear)) +
  facet_grid(.~scaled_up_using) +
  geom_vline(aes(xintercept=0), col="darkgrey", lwd=1.25) +
  geom_histogram(aes(fill = emission_or_storage), binwidth = 2) +
  theme_bw() + 
  xlab(expression(paste("Million Tonnes CO"[2],"e yr"^-1, sep=""))) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"),
                    labels = c("Emission", "Storage")) + 
  theme(legend.title=element_blank(), legend.position = "top") +
  xlim(round(range(total_nggi_summed_summaries_B$lower_99_sum_total_MillionTonnesCO2PerYear - 10, 
                   total_nggi_summed_summaries_B$upper_99_sum_total_MillionTonnesCO2PerYear + 10)))

y_maxs_for_plot_b <- ggplot_build(sum_total_MillionTonnesCO2_histograms_meansLogmeans)$data[[2]] %>%
  group_by(PANEL) %>%
  summarise(maximum_y = max(y))

sum_total_MillionTonnesCO2_histograms_meansLogmeans <- sum_total_MillionTonnesCO2_histograms_meansLogmeans + 
  geom_point(data = total_nggi_summed_summaries_B, aes(x=median_sum_total_MillionTonnesCO2PerYear, y_maxs_for_plot_b$maximum_y + 25), pch = 16, color = "black") +
  geom_segment(data = total_nggi_summed_summaries_B, aes(x=lower_ci_sum_total_MillionTonnesCO2PerYear, xend = upper_ci_sum_total_MillionTonnesCO2PerYear,
                                                       y = y_maxs_for_plot_b$maximum_y + 25, yend = y_maxs_for_plot_b$maximum_y + 25), color = "black")

(sum_total_MillionTonnesCO2_histograms_meansLogmeans)


# split separate out logmean versions to make main text figure
total_nggi_summed_iterations_logmean <- filter(total_nggi_summed_iterations,
                                               scaled_up_using == "logmean")
total_nggi_summed_summaries_logmean <- filter(total_nggi_summed_summaries,
                                      scaled_up_using == "logmean")
salinity_stability_summed_iterations_logmean <- filter(salinity_stability_summed_iterations,
                                               scaled_up_using == "logmean")
salinity_stability_summed_summaries_logmean <- filter(salinity_stability_summed_summaries,
                                              scaled_up_using == "logmean")


# Create Giant Total NGGI Histogram 
total_histograms_logmeans <- ggplot(data = total_nggi_summed_iterations_logmean, aes(x = sum_total_MillionTonnesCO2PerYear)) +
  facet_grid(.~analysis_description) +
  geom_vline(aes(xintercept=0), col="darkgrey", lwd=1.25) +
  geom_histogram(aes(fill = emission_or_storage), binwidth = 1) +
  theme_bw() + 
  xlab(expression(paste("Million Tonnes CO"[2],"e yr"^-1, sep=""))) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"),
                    labels = c("Emission", "Storage")) + 
  theme(legend.title=element_blank(), legend.position = "none")

y_maxs_for_plot_a <- ggplot_build(total_histograms_logmeans)$data[[2]] %>%
  group_by(PANEL) %>%
  summarise(maximum_y = max(y))

total_histograms_logmeans <- total_histograms_logmeans + 
  geom_point(data = total_nggi_summed_summaries_logmean, aes(x=median_sum_total_MillionTonnesCO2PerYear, y = 
                                                               y_maxs_for_plot_a$maximum_y + 20), pch = 16, color = "black") +
  geom_segment(data = total_nggi_summed_summaries_logmean, aes(x=total_nggi_summed_summaries_logmean$lower_ci_sum_total_MillionTonnesCO2PerYear, 
                                                       xend = total_nggi_summed_summaries_logmean$upper_ci_sum_total_MillionTonnesCO2PerYear,
                                                       y = y_maxs_for_plot_a$maximum_y + 20, 
                                                       yend = y_maxs_for_plot_a$maximum_y + 20), color = "black")

(total_histograms_logmeans)

# creat salinity stability histogram
salinity_stability_summed_histogram_logmean <- ggplot(data = salinity_stability_summed_iterations_logmean, aes(x = sum_total_MillionTonnesCO2PerYear)) +
  facet_wrap(salinity~stability) +
  geom_vline(aes(xintercept=0), col="darkgrey", lwd=1.25) +
  geom_histogram(aes(fill = emission_or_storage), binwidth = 0.5) +
  theme_bw() + 
  xlab(expression(paste("Million Tonnes CO"[2],"e yr"^-1, sep=""))) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"),
                    labels = c("Emission", "Storage")) + 
  theme(legend.title=element_blank(), legend.position = "top") +
  xlim(round(range(salinity_stability_summed_summaries_logmean$lower_99_sum_total_MillionTonnesCO2PerYear - 1, 
                   salinity_stability_summed_summaries_logmean$upper_99_sum_total_MillionTonnesCO2PerYear + 1)))

y_maxs_for_plot_b <- ggplot_build(salinity_stability_summed_histogram_logmean)$data[[2]] %>%
  group_by(PANEL) %>%
  summarise(maximum_y = max(y))

salinity_stability_summed_histogram_logmean <- salinity_stability_summed_histogram_logmean + 
  geom_point(data = salinity_stability_summed_summaries_logmean, aes(x=median_sum_total_MillionTonnesCO2PerYear, 
                                                                     y = y_maxs_for_plot_b$maximum_y + 100), pch = 16, color = "black") +
  geom_segment(data = salinity_stability_summed_summaries_logmean, aes(x = lower_ci_sum_total_MillionTonnesCO2PerYear, 
                                                               xend = upper_ci_sum_total_MillionTonnesCO2PerYear,
                                                               y = y_maxs_for_plot_b$maximum_y + 100, 
                                                               yend = y_maxs_for_plot_b$maximum_y + 100), color = "black")

(salinity_stability_summed_histogram_logmean)

grid.arrange(salinity_stability_summed_histogram_logmean, total_histograms_logmeans, nrow = 2, heights = c(6, 4))
