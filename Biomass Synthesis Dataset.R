library(tidyverse)
library(gridExtra)


krauss_2018 <- read_csv("data/Biomass/krauss_2018/derrivative/krauss_2018_site_summary.csv")
krauss_2018 <- krauss_2018 %>% 
  mutate(plot_id = as.character(plot_id))
twiley_2016 <- read_csv("data/Biomass/lter/twiley/derrivative/twiley_2016_site_summary.csv")
twiley_2016 <- twiley_2016 %>% 
  mutate(plot_id = as.character(plot_id))
simard_2006 <- read_csv("data/Biomass/simard_2006/derrivative/simmard_2006_mangrove_site_summary.csv")
simard_2006 <- simard_2006 %>% 
  mutate(plot_id = as.character(plot_id))
megonigal_1997 <- read_csv("data/Biomass/megonigal_1997/derrivative/megonigal_1997_site_summary.csv")
doughty_2015 <- read_csv("data/Biomass/doughty_2015/derrivative/doughty_2015_site_summary.csv")
craft_2013 <- read_csv("data/Biomass/lter/craft/derrivative/craft_2013_site_summary.csv")
craft_2013 <- craft_2013 %>% 
  mutate(plot_id = as.character(plot_id))
byrd_2018 <- read_csv("data/Biomass/byrd_2018/derrivative/byrd_2018_site_summary.csv")


# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

master_data <- byrd_2018 %>% 
  full_join(megonigal_1997) %>% 
  full_join(krauss_2018)   %>% 
  full_join(twiley_2016) %>% 
  full_join(craft_2013)  %>% 
  full_join(simard_2006)  %>% 
  full_join(doughty_2015) %>% 
  mutate(g_co2_m2 = mean_agb_g_m2 * 0.48 * 3.6667,
         study = str_replace(paste(toupper(substring(study_id, 1,1)), substring(study_id, 2), sep=""), "_", " "))

master_data_summary <- master_data %>% 
  group_by(vegetation_class) %>% 
  summarise(logmean = mean(log(g_co2_m2), na.rm = T),
         logsd = sd(log(g_co2_m2), na.rm = T),
         n = length(g_co2_m2))

# Copy and pasted from the two pallates in order so that the various submittors have a different
# ... color but the same ones in different frames.
emSsColor <- c("#CC79A7", "#E69F00", "#56B4E9")
forColor <- c("#D55E00", "#E69F00", "#009E73", "#F0E442", "#0072B2","#56B4E9")

forested <- subset(master_data, vegetation_class == "forest")
shrub <- subset(master_data, vegetation_class == "shrub")
emergent <- subset(master_data, vegetation_class == "emergent")
emergent_shrub <- subset(master_data, vegetation_class == "emergent" |  vegetation_class == "shrub")

forest_x <- seq(0.1, max(forested$g_co2_m2), by = 100) 
forest_y <- dlnorm(forest_x, master_data_summary$logmean[2], master_data_summary$logsd[2])

forest_predict_df <- data.frame(forest_x, forest_y)

emSs_x <- seq(0.1, max(emergent_shrub$g_co2_m2), by = 10) 
em_y <- dlnorm(emSs_x, master_data_summary$logmean[1], master_data_summary$logsd[1])
ss_y <- dlnorm(emSs_x, master_data_summary$logmean[3], master_data_summary$logsd[3])

emSs_predict_df <- data.frame(x = c(emSs_x, emSs_x),
                     y = c(em_y, ss_y),
                     vegetation_class = c(rep("emergent", length(em_y)),
                                          rep("shrub", length(ss_y))))
write_csv(master_data_summary, "data/Biomass/biomass_gCO2_distribution_summaries.csv")

forest_plot <- ggplot(data=forested, aes(x = g_co2_m2)) +
  geom_histogram(color = "darkgrey", fill = "lightgrey", (aes(y=..density..))) +
  geom_line(lwd=1, col="black", data=forest_predict_df, aes(x=forest_x,forest_y)) +
  geom_rug(aes(color=study)) + 
  # To use for line and point colors, add
  scale_colour_manual(values=forColor) +
  theme_bw() +
  xlab(expression(paste("Mean Above Ground Biomass for Plot (gCO"["2"], " m"^"-2",")", sep=""))) +
  facet_grid(.~vegetation_class)


emergent_shrub_plot <- ggplot(data=emergent_shrub, aes(x = g_co2_m2)) +
  geom_histogram(color = "darkgrey", fill = "lightgrey", (aes(y=..density..))) +
  geom_line(lwd=1, col="black", data=emSs_predict_df, aes(x=x,y=y)) +
  geom_rug(aes(color=study)) + 
  # To use for line and point colors, add
  scale_colour_manual(values=emSsColor) +
  theme_bw() +
  xlab(expression(paste("Mean Above Ground Biomass for Plot (gCO"["2"], " m"^"-2",")", sep=""))) +
  facet_grid(.~vegetation_class)

grid.arrange(emergent_shrub_plot, forest_plot, nrow = 2, heights = c(4, 6))
