# Summarise Total gCO2 per mapped pixel for joining to C-CAP table.
# Create and export summary data
library(tidyverse) # to run data plyr and ggplot operations
library(gridExtra) # to combine ggplot graphs

total_df.saved.iterations <- read_csv("data/outputTables/MonteCarloResults1/total_saved_iterations.csv")

# Define assumptions re: land cover classes
{
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Snow/Ice')
  
  palustrineWetlands <- c('Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland')
  
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
                      'Cultivated', 'Pasture/Hay',
                      'Unconsolidated Shore', 'Water',
                      'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')
  
  forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
                 'Palustrine Forested Wetland',
                 'Estuarine Forested Wetland'
  )
  
  scrubShrubVeg <- c('Scrub/Shrub',
                     'Palustrine Scrub/Shrub Wetland',
                     'Estuarine Scrub/Shrub Wetland'
  )
  
  emergentVeg <- c('Cultivated', 'Pasture/Hay', 
                   'Grassland',
                   'Palustrine Emergent Wetland',
                   'Estuarine Emergent Wetland'
  )
  
  nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
             'Unconsolidated Shore', 'Bare Land', 'Water', 
             'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
             'Tundra',
             'Snow/Ice'
  )
  
  classify_by_salinity <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
    if (class_time1 %in% estuarineWetlands | class_time2 %in% estuarineWetlands) {
      salinity_class <- "Estuarine"
    } else if (class_time1 %in% palustrineWetlands | class_time2 %in% palustrineWetlands) {
      salinity_class <- "Palustrine"
    } else {
      salinity_class <- NA
    }
    return(salinity_class)
  }
  
  classify_by_stability <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
    if (class_time1 %in%  estuarineWetlands | class_time1  %in%  palustrineWetlands | class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
      if (class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
        stability_class <- "Stable and Gains"
      } else if ((! class_time2 %in% estuarineWetlands) & (! class_time2 %in% palustrineWetlands)) {
        stability_class <- "Losses"
      } else {
        stability_class <- NA
      }
    } else {
      stability_class <- NA 
    }
    return(stability_class)
  }
  
}

{
  total_df.saved.iterations <- as_tibble(total_df.saved.iterations)
  sector_and_total_mapping_outputs <- total_df.saved.iterations %>%
    mutate(total_tonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6,
           soil_tonnesCO2perMappedPixel = soil_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6, 
           biomass_tonnesCO2perMappedPixel = biomass_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6,
           methane_tonnesCO2perMappedPixel = methane_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6) %>%
    group_by(class_2006, abbrev_2006, class_2010, abbrev_2010, class, abbrev) %>%
    summarise(mapped_med =  median(mappedPixelCount, na.rm = T),
              mapped_min = quantile(mappedPixelCount, 0.025, na.rm =T),
              mapped_max = quantile(mappedPixelCount, 0.975, na.rm =T),
              mapped_ci = mapped_max - mapped_min,
              estima_med =  median(estimated_pixel_count, na.rm = T),
              estima_min = quantile(estimated_pixel_count, 0.025, na.rm =T),
              estima_max = quantile(estimated_pixel_count, 0.975, na.rm =T),
              estima_ci = mapped_max - mapped_min,
              total_med  = median(total_tonnesCO2perMappedPixel, na.rm = T),
              total_min = quantile(total_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              total_max = quantile(total_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              total_ci = (total_max - total_min),
              soil_med  = median(soil_tonnesCO2perMappedPixel, na.rm = T),
              soil_min = quantile(soil_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              soil_max = quantile(soil_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              soil_ci = (soil_max - soil_min),
              bmass_med  = median(biomass_tonnesCO2perMappedPixel, na.rm = T),
              bmass_min = quantile(biomass_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              bmass_max = quantile(biomass_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              bmass_ci = (bmass_max - bmass_min),
              ch4_med  = median(methane_tonnesCO2perMappedPixel, na.rm = T),
              ch4_min = quantile(methane_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              ch4_max = quantile(methane_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              ch4_ci = (ch4_max - ch4_min)) %>%
    filter(complete.cases(mapped_med, estima_med, total_med, soil_med, bmass_med, ch4_med))
  
  write_csv(sector_and_total_mapping_outputs, "data/outputTables/MonteCarloResults1/sector_and_total_mapping_outputs.csv")
  
  # Total NGGI
  total_nggi_summed_iterations <- total_df.saved.iterations %>%
    mutate(analysis_description = "Total NGGI") %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(analysis_description, iteration_number) %>%
    summarise(sum_total_MillionTonnesCO2 = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T))
  write_csv(total_nggi_summed_iterations, "data/outputTables/MonteCarloResults1/total_nggi_summed_iterations.csv")
  
  total_nggi_summed_summaries <- total_nggi_summed_iterations %>%
    group_by(analysis_description) %>%
    summarise(median_sum_total_MillionTonnesCO2 = median(sum_total_MillionTonnesCO2),
              lower_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.025),
              upper_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.975))
  
  total_df.saved.iterations["salinity"] <- mapply(classify_by_salinity,
                                                  class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                  class_time2 = as.character(total_df.saved.iterations$class_2010))
  total_df.saved.iterations["stability"] <- mapply(classify_by_stability,
                                                   class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                   class_time2 = as.character(total_df.saved.iterations$class_2010))
  total_df.saved.iterations["analysis_description"] <- mapply(paste, total_df.saved.iterations$salinity, " ", 
                                                              total_df.saved.iterations$stability, sep="")
  
  
  total_nggi_summed_iterations["emission_or_storage"] <- ifelse(total_nggi_summed_iterations$sum_total_MillionTonnesCO2 >= 0, "storage", "emission")
  
  
  sum_total_MillionTonnesCO2_histograms <- ggplot(data = total_nggi_summed_iterations, aes(x = sum_total_MillionTonnesCO2)) +
    facet_grid(.~analysis_description) +
    geom_histogram(aes(fill = emission_or_storage), breaks = seq(-128, 20, by=4)) +
    geom_vline(data=abline_df, color = "black", aes(xintercept = x)) +
    theme_bw() + 
    xlab(expression(paste("Million Tonnes CO"[2],"e", sep=""))) +
    scale_fill_manual(values=c("#D55E00", "#0072B2"),
                      labels = c("Emission", "Storage")) + 
    theme(legend.title=element_blank(), legend.position = "none")
  (sum_total_MillionTonnesCO2_histograms)
  
  
  # NGGI by salinity and stability
  salinity_stability_summed_iterations <- total_df.saved.iterations %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(salinity, stability, analysis_description, iteration_number) %>%
    summarise(sum_total_MillionTonnesCO2 = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T))
  
  salinity_stability_summed_summaries <- salinity_stability_summed_iterations %>% 
    group_by(analysis_description) %>%
    summarise(median_sum_total_MillionTonnesCO2 = median(sum_total_MillionTonnesCO2),
              lower_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.025),
              upper_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.975))
  combined_sum_summaries <- rbind(total_nggi_summed_summaries, salinity_stability_summed_summaries)
  write_csv(combined_sum_summaries, "data/outputTables/MonteCarloResults1/combined_sum_summaries.csv")
  
  abline_df <- data.frame(x = c(0, 0, 0, 0), 
                          salinity = c("Estuarine", "Estuarine", "Palustrine", "Palustrine"),
                          stability = c("Stable and Gains", "Losses", "Stable and Gains", "Losses"))
  
  
  salinity_stability_summed_iterations["emission_or_storage"] <- ifelse(salinity_stability_summed_iterations$sum_total_MillionTonnesCO2 >= 0, "storage", "emission")
  
  breakdown_MillionTonnesCO2_histograms <- ggplot(data = salinity_stability_summed_iterations, aes(x = sum_total_MillionTonnesCO2)) +
    facet_wrap(salinity~stability) +
    geom_histogram(aes(fill = emission_or_storage), breaks = seq(-76, 52, by=4)) +
    geom_vline(data=abline_df, color = "black", aes(xintercept = x)) +
    theme_bw() + 
    xlab(expression(paste("Million Tonnes CO"[2],"e", sep=""))) +
    scale_fill_manual(values=c("#D55E00", "#0072B2"),
                      labels = c("Emission", "Storage")) + 
    theme(legend.title=element_blank(), legend.position = "top")
  (breakdown_MillionTonnesCO2_histograms)
  
  grid.arrange(breakdown_MillionTonnesCO2_histograms, sum_total_MillionTonnesCO2_histograms, nrow = 2, heights = c(6, 4))
}
