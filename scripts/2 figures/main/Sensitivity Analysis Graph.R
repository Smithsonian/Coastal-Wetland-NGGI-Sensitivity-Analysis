library(tidyverse)
library(gridExtra)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sensitivityAnalysisDF_regional <- read_csv("data/outputTables/sensitivityAnalysisResults/regional/sensitivityAnalysisResults.csv")
sensitivityAnalysisDF_regional <- mutate(sensitivityAnalysisDF_regional, effectMillionTonnesCO2PerYear = effectMillionTonnesCO2/5)
sensitivityAnalysisDF_regional_top15 <- sensitivityAnalysisDF_regional[1:15,]
sensitivityAnalysisDF_regional_top15 <- sensitivityAnalysisDF_regional_top15 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_regional_top15)))


sensitivityAnalysisDF_national <- read_csv("data/outputTables/sensitivityAnalysisResults/national/sensitivityAnalysisResults.csv")
sensitivityAnalysisDF_national <- mutate(sensitivityAnalysisDF_national, effectMillionTonnesCO2PerYear = effectMillionTonnesCO2/5)
sensitivityAnalysisDF_national_top15 <- sensitivityAnalysisDF_national[1:15,]
sensitivityAnalysisDF_national_top15 <- sensitivityAnalysisDF_national_top15 %>%
  mutate(rank = rev(1:nrow(sensitivityAnalysisDF_national_top15)))

sensitivityAnalysisPlot_top15_regional <- ggplot(sensitivityAnalysisDF_regional_top15, aes(x=rank, y=effectMillionTonnesCO2PerYear)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2PerYear, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  xlab(NULL) +
  ylab(expression(paste("Effect of Uncertainty on Total Flux (million Tonnes CO"[2], "e yr"^-1, ")", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_regional_top15$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())

sensitivityAnalysisPlot_top15_regional_noLab <- sensitivityAnalysisPlot_top15_regional +
  guides(color=FALSE,
         pch = FALSE) +
  ggtitle("Logmeans")

sensitivityAnalysisPlot_top15_national <- ggplot(sensitivityAnalysisDF_national_top15, aes(x=rank, y=effectMillionTonnesCO2PerYear)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  geom_segment(aes(y=effectMillionTonnesCO2PerYear, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) + 
  xlab(NULL) +
  ylab(expression(paste("Effect of Uncertainty on Total Flux (million Tonnes CO"[2], "e yr"^-1, ")", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_national_top15$parameter)) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank()) +
  ggtitle("Means")

compare_plots <- grid.arrange(sensitivityAnalysisPlot_top15_regional_noLab,
                              sensitivityAnalysisPlot_top15_national,
                              ncol = 2,
                              widths = c(1,1.75))

compare_plots

sensitivityAnalysisDF_national_top15_combine <- mutate(sensitivityAnalysisDF_national_top15,
                                                       scaleOfAnalysis = "A Mean")
sensitivityAnalysisDF_regional_top15_combine <- mutate(sensitivityAnalysisDF_regional_top15,
                                                       scaleOfAnalysis = "B Logmean")
sensitivityAnalysisDF_regional_top15_combo <- rbind(sensitivityAnalysisDF_national_top15_combine,
                                                    sensitivityAnalysisDF_regional_top15_combine)

sensitivityAnalysisPlot_top15_combo <- ggplot(sensitivityAnalysisDF_regional_top15_combo, aes(x=rank, y=effectMillionTonnesCO2PerYear)) +
  geom_point(stat="identity", size=3, aes(color = type, pch = type)) + 
  facet_wrap(~scaleOfAnalysis) +
  geom_segment(aes(y=effectMillionTonnesCO2PerYear, 
                   yend=0, 
                   x=rank, 
                   xend=rank, color=type)) +
  xlab(NULL) +
  ylab(expression(paste("Effect of Uncertainty on Total Flux (million Tonnes CO"[2], "e yr"^-1, ")", sep=""))) +
  scale_x_discrete(limits = rev(sensitivityAnalysisDF_regional_top15_combo$rank)) +
  geom_text(aes(label=sensitivityAnalysisDF_regional_top15_combo$parameter), nudge_x=.2, nudge_y = c(-55, rep(2.5,29)), 
            vjust = 0, hjust=0) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.title=element_blank())
