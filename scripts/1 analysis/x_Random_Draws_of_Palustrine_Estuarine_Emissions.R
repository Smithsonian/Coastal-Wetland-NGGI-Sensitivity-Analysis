library(tidyverse)

saved_iterations <- c()
saved_emissions <- c()
saved_type <- c()

for (i in 1:10000) {
  palustrine.methane.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.gwp, palustrine.methane.log.sd.gwp)
  saved_iterations <- c(saved_iterations, i, i)
  saved_emissions <- c(saved_emissions, palustrine.methane.randomDraw$median, palustrine.methane.randomDraw$mean)
  saved_type <- c(saved_type, "logmean", "mean")
}

all_iterations_df_10000 <- data.frame(i = saved_iterations,
                                      emissions = saved_emissions,
                                      central_tendency = saved_type)

all_iterations_summary_10000 <- all_iterations_df_10000 %>% 
  group_by(central_tendency) %>%
  summarise(lower_95 = quantile(emissions, 0.025),
            upper_95 = quantile(emissions, 0.975)) 

(all_iterations_summary_10000)
