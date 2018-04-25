### Let's make a graph for CH4 emissions to show the different splits we could make
library(tidyverse)

methane <- read_csv("data/Methane/derivative/Methane Synthesis Knox.csv")
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")

salinity_breaks <- data.frame(x = c(15, 5), 
                              salinity_class_breaks = c("IPCC", "C-CAP"))

ggplot(data = methane, aes(x = Salinity.ppt, y = CH4.flux)) +
  geom_point(aes(shape = Method), size = 4, fill="grey") +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste("Methane Emissions (gCH"["4"]," m"^"-2", " yr"^"-1",")", sep="")))  +
  scale_shape_manual(values=c(22, 24),
                    labels = c("Eddy Covariance", "Static Chamber")) +
  theme(legend.position = c(0.75,0.6)) +
  geom_vline(data = salinity_breaks, aes(xintercept = x, lty=salinity_class_breaks), col = "darkred") +
  labs(lty = "Salinity Class")
