# Create and export uncertainty and sensitivgity analysis tables
library(tidyverse)
library(gridExtra) # to combine ggplot graphs

# load up tables
#total_nggi_summed_iterations
#total_nggi_summed_summaries
#salinity_stability_summed_iterations
#salinity_stability_summed_summaries

# split into mean and logmean versions

# make a histogram with both uncertainty analyses iterations summarised


breakdown_MillionTonnesCO2_histograms <- ggplot(data = salinity_stability_summed_iterations_logmean, aes(x = sum_total_MillionTonnesCO2PerYear)) +
  facet_wrap(salinity~stability) +
  geom_vline(aes(xintercept=0), col="darkgrey", lwd=1.25) +
  geom_histogram(aes(fill = emission_or_storage), breaks = seq(-13, 8, by=0.5)) +
  theme_bw() + 
  xlab(expression(paste("Million Tonnes CO"[2],"e yr"^"-1", sep=""))) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"),
                    labels = c("Emission", "Storage")) + 
  theme(legend.title=element_blank(), legend.position = "top")

y_maxs_for_plot <- ggplot_build(breakdown_MillionTonnesCO2_histograms)$data[[2]] %>%
  group_by(PANEL) %>%
  summarise(maximum_y = max(y))

breakdown_MillionTonnesCO2_histograms <- breakdown_MillionTonnesCO2_histograms + geom_point(data = salinity_stability_summed_iterations_logmean, aes(x=median_sum_total_MillionTonnesCO2PerYear, y_maxs_for_plot$maximum_y + 20), pch = 16, color = "black") +
  geom_segment(data = salinity_stability_summed_iterations_logmean, aes(x=lower_sum_total_MillionTonnesCO2PerYear, xend = upper_sum_total_MillionTonnesCO2PerYear,
                                                                        y = y_maxs_for_plot$maximum_y + 20, yend = y_maxs_for_plot$maximum_y + 20), color = "black")
(breakdown_MillionTonnesCO2_histograms)

sum_total_MillionTonnesCO2_histograms <- ggplot(data = total_nggi_summed_iterations, aes(x = sum_total_MillionTonnesCO2)) +
  facet_grid(.~analysis_description) +
  geom_vline(aes(xintercept=0), col="darkgrey", lwd=1.25) +
  geom_histogram(aes(fill = emission_or_storage), breaks = seq(-550, 12.5, by=12.5)) +
  theme_bw() + 
  xlab(expression(paste("Million Tonnes CO"[2],"e", sep=""))) +
  scale_fill_manual(values=c("#D55E00", "#0072B2"),
                    labels = c("Emission", "Storage")) + 
  theme(legend.title=element_blank(), legend.position = "none")

y_maxs_for_plot_b <- ggplot_build(sum_total_MillionTonnesCO2_histograms)$data[[2]] %>%
  group_by(PANEL) %>%
  summarise(maximum_y = max(y))

sum_total_MillionTonnesCO2_histograms <- sum_total_MillionTonnesCO2_histograms + geom_point(data = total_nggi_summed_summaries, aes(x=median_sum_total_MillionTonnesCO2, y_maxs_for_plot_b$maximum_y + 5), pch = 16, color = "black") +
  geom_segment(data = total_nggi_summed_summaries, aes(x=lower_sum_total_MillionTonnesCO2, xend = upper_sum_total_MillionTonnesCO2,
                                                       y = y_maxs_for_plot_b$maximum_y + 5, yend = y_maxs_for_plot_b$maximum_y + 5), color = "black")


(sum_total_MillionTonnesCO2_histograms)

grid.arrange(breakdown_MillionTonnesCO2_histograms, sum_total_MillionTonnesCO2_histograms, nrow = 2, heights = c(6, 4))
