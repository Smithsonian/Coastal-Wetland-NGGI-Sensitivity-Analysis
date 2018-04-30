library(tidyverse)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sensitivityAnalysisDF <- read_csv("data/outputTables/SensitivityAnalysisResults.csv")

sensitivityAnalysisDF_top10 <- sensitivityAnalysisDF[1:10,]

sensitivityAnalysisDF_top10 <- sensitivityAnalysisDF_top10 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_top10)))

ggplot(sensitivityAnalysisDF_top10, aes(x=rank, y=effectMillionTonnesCO2)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  xlab(NULL) +
  ylab(expression(paste("Uncertainty Effect on Total CONUS Accounting (million Tonnes CO"[2], "e)", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_top10$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())

sensitivityAnalysisDF_top15 <- sensitivityAnalysisDF[1:15,]

sensitivityAnalysisDF_top15 <- sensitivityAnalysisDF_top15 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_top15)))

ggplot(sensitivityAnalysisDF_top15, aes(x=rank, y=effectMillionTonnesCO2)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  xlab(NULL) +
  ylab(expression(paste("Effect of Uncertainty on Total Flux (million Tonnes CO"[2], "e)", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_top15$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())

sensitivityAnalysisDF_top20 <- sensitivityAnalysisDF[1:20,]

sensitivityAnalysisDF_top20 <- sensitivityAnalysisDF_top20 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_top20)))

ggplot(sensitivityAnalysisDF_top20, aes(x=rank, y=effectMillionTonnesCO2)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  labs(title="Sensitivity Analysis", 
       subtitle="Coastal Wetland NGGI (2006-2011)") +
  xlab(NULL) +
  ylab(expression(paste("Uncertainty Effect on Total CONUS Accounting (million Tonnes CO"[2], "e)", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_top20$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())

sensitivityAnalysisDF_top30 <- sensitivityAnalysisDF[1:30,]

sensitivityAnalysisDF_top30 <- sensitivityAnalysisDF_top30 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_top30)))

ggplot(sensitivityAnalysisDF_top30, aes(x=rank, y=effectMillionTonnesCO2)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  labs(title="Sensitivity Analysis", 
       subtitle="Coastal Wetland NGGI (2006-2011)") +
  xlab(NULL) +
  ylab(expression(paste("Uncertainty Effect on Total CONUS Accounting (million Tonnes CO"[2], "e)", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_top30$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())
