library(tidyverse)

saved_iterations <- c()
saved_emissions <- c()
saved_type <- c()

for (i in 1:1000) {
  palustrine.methane.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.gwp, palustrine.methane.log.sd.gwp)
  saved_iterations <- c(saved_iterations, i, i)
  saved_emissions <- c(saved_emissions, palustrine.methane.randomDraw$median, palustrine.methane.randomDraw$mean)
  saved_type <- c(saved_type, "logmean", "mean")
}

all_iterations_df_1000 <- data.frame(i = saved_iterations,
                                      emissions = saved_emissions,
                                      central_tendency = saved_type)

ggplot(data = all_iterations_df_1000, aes(emissions)) +
  geom_histogram() +
  facet_wrap(~central_tendency) + 
  geom_vline(data = data.frame(max_observed_ch4 = c(5332.5)), aes(xintercept = max_observed_ch4), color = "red") +
  xlab(expression(paste("Simulated Methane Emissions Factor (gCO"[2], "e m"^-2, " yr"^-1,")", sep=""))) +
  scale_x_log10()

all_iterations_summary_1000 <- all_iterations_df_1000 %>% 
  group_by(central_tendency) %>%
  summarise(lower_95 = quantile(emissions, 0.025),
            upper_95 = quantile(emissions, 0.975)) 

write_csv(all_iterations_summary_1000, 'data/MethaneEmissions_Iterations_Summary_1000.csv')
