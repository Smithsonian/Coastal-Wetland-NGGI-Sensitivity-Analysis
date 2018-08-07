# script for creating supplemental biomass figure
library(tidyverse)
library(gridExtra)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

master_data <- read_csv("data/Biomass/biomass_gCO2_master_summaries.csv")
master_data_summary <- read_csv("data/Biomass/biomass_gCO2_distribution_summaries.csv")

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
