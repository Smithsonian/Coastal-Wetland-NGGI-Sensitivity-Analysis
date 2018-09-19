# Summarise Total gCO2 per mapped pixel for joining to C-CAP table.
# Create and export summary data
library(tidyverse) # to run data plyr and ggplot operations

total_df.saved.iterations.regional <- read_csv("data/outputTables/MonteCarloResults/regional/total.savedIterations.csv")
total_df.saved.iterations.national <- read_csv("data/outputTables/MonteCarloResults/national/total.savedIterations.csv")

# Define assumptions re: land cover classes
classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 
               'Low Intensity Developed', 'Developed Open Space', 
               'Cultivated', 'Pasture/Hay', 
               'Grassland', 'Deciduous Forest', 
               'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
               'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 
               'Palustrine Emergent Wetland', 
               'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 
               'Estuarine Emergent Wetland', 
               'Unconsolidated Shore', 'Bare Land', 'Water', 
               'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
               'Snow/Ice')

palustrineWetlands <- c('Palustrine Forested Wetland', 
                        'Palustrine Scrub/Shrub Wetland', 
                        'Palustrine Emergent Wetland')
estuarineWetlands <- c('Estuarine Forested Wetland', 
                       'Estuarine Scrub/Shrub Wetland', 
                       'Estuarine Emergent Wetland')

abbrevs <- c('HID', 'MID', 'LID', 'OSD',
             'CULT', 'PAST',
             'GRS', 'DEC', 'EVR', 'MIX', 'SS',
             'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
             'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')

soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 
                    'Low Intensity Developed', 'Developed Open Space',
                    'Cultivated', 'Pasture/Hay',
                    'Unconsolidated Shore', 'Water',
                    'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')

forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
               'Palustrine Forested Wetland',
               'Estuarine Forested Wetland')

scrubShrubVeg <- c('Scrub/Shrub',
                   'Palustrine Scrub/Shrub Wetland',
                   'Estuarine Scrub/Shrub Wetland')

emergentVeg <- c('Cultivated', 'Pasture/Hay', 
                 'Grassland',
                 'Palustrine Emergent Wetland',
                 'Estuarine Emergent Wetland')

nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 
           'Low Intensity Developed', 'Developed Open Space',
           'Unconsolidated Shore', 'Bare Land', 'Water', 
           'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
           'Tundra',
           'Snow/Ice')

classify_by_salinity <- function(class_time1 = "Estuarine Emergent Wetland", 
                                 class_time2 = "Open Water") {
  if (class_time1 %in% estuarineWetlands | class_time2 %in% estuarineWetlands) {
    salinity_class <- "Estuarine"
  } else if (class_time1 %in% palustrineWetlands | 
             class_time2 %in% palustrineWetlands) {
    salinity_class <- "Palustrine"
  } else {
    salinity_class <- NA
  }
  return(salinity_class)
}

classify_by_stability <- function(class_time1 = "Estuarine Emergent Wetland", 
                                  class_time2 = "Open Water") {
  if (class_time1 %in%  estuarineWetlands | 
      class_time1  %in%  palustrineWetlands | 
      class_time2 %in% estuarineWetlands | 
      class_time2 %in% palustrineWetlands) {
    if (class_time2 %in% estuarineWetlands | 
        class_time2 %in% palustrineWetlands) {
      stability_class <- "Stable and Gains"
    } else if ((! class_time2 %in% estuarineWetlands) & 
               (! class_time2 %in% palustrineWetlands)) {
      stability_class <- "Losses"
    } else {
      stability_class <- NA
    }
  } else {
    stability_class <- NA 
  }
  return(stability_class)
}

# Estimate the Median Value for the mapping excercise 
sector_and_total_mapping_outputs_regional <- total_df.saved.iterations.regional %>%
  mutate(total_tonnesCO2perMappedPixelPerYear = total_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6 / 5,
         soil_tonnesCO2perMappedPixelPerYear = soil_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6 / 5, 
         biomass_tonnesCO2perMappedPixelPerYear = biomass_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6 / 5,
         methane_tonnesCO2perMappedPixelPerYear = methane_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6 / 5) %>%
  group_by(class_2006, abbrev_2006, class_2010, abbrev_2010, class, abbrev) %>%
  summarise(mapped_med =  median(mappedPixelCount, na.rm = T),
            mapped_min = quantile(mappedPixelCount, 0.025, na.rm =T),
            mapped_max = quantile(mappedPixelCount, 0.975, na.rm =T),
            mapped_ci = mapped_max - mapped_min,
            estima_med =  median(estimated_pixel_count, na.rm = T),
            estima_min = quantile(estimated_pixel_count, 0.025, na.rm =T),
            estima_max = quantile(estimated_pixel_count, 0.975, na.rm =T),
            estima_ci = estima_med - estima_min,
            total_med  = median(total_tonnesCO2perMappedPixelPerYear, na.rm = T),
            total_min = quantile(total_tonnesCO2perMappedPixelPerYear, 0.025, na.rm = T),
            total_max = quantile(total_tonnesCO2perMappedPixelPerYear, 0.975, na.rm = T),
            total_ci = (total_max - total_min),
            soil_med  = median(soil_tonnesCO2perMappedPixelPerYear, na.rm = T),
            soil_min = quantile(soil_tonnesCO2perMappedPixelPerYear, 0.025, na.rm = T),
            soil_max = quantile(soil_tonnesCO2perMappedPixelPerYear, 0.975, na.rm = T),
            soil_ci = (soil_max - soil_min),
            bmass_med  = median(biomass_tonnesCO2perMappedPixelPerYear, na.rm = T),
            bmass_min = quantile(biomass_tonnesCO2perMappedPixelPerYear, 0.025, na.rm = T),
            bmass_max = quantile(biomass_tonnesCO2perMappedPixelPerYear, 0.975, na.rm = T),
            bmass_ci = (bmass_max - bmass_min),
            ch4_med  = median(methane_tonnesCO2perMappedPixelPerYear, na.rm = T),
            ch4_min = quantile(methane_tonnesCO2perMappedPixelPerYear, 0.025, na.rm = T),
            ch4_max = quantile(methane_tonnesCO2perMappedPixelPerYear, 0.975, na.rm = T),
            ch4_ci = (ch4_max - ch4_min)) %>%
  filter(complete.cases(mapped_med, estima_med, total_med, soil_med, bmass_med, ch4_med))

# Write to file
write_csv(sector_and_total_mapping_outputs_regional, "data/outputTables/MonteCarloResults/regional/sector_and_total_mapping_outputs.csv")

# Add a column with strategy used to scale up skewed variables
total_df.saved.iterations.regional <- total_df.saved.iterations.regional %>% 
  mutate(scaled_up_using = "logmean")

total_df.saved.iterations.national <- total_df.saved.iterations.national %>% 
  mutate(scaled_up_using = "mean")

# compile files
total_df.saved.iterations <- rbind(total_df.saved.iterations.regional, total_df.saved.iterations.national)
# Total NGGI
total_nggi_summed_iterations <- total_df.saved.iterations %>%
  mutate(analysis_description = "Total NGGI") %>%
  mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
  group_by(analysis_description, iterationCode, scaled_up_using) %>%
  summarise(sum_total_MillionTonnesCO2PerYear = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T) / 5) %>%
  arrange(scaled_up_using, iterationCode)

total_nggi_summed_iterations["emission_or_storage"] <- 
  ifelse(total_nggi_summed_iterations$sum_total_MillionTonnesCO2PerYear >= 0, 
         "storage", "emission")

# write total number of iterations to its own file
# This will be used for the large histogram in the uncertainty analysis figure
#  and the comparison between using logmean and mean uncertainty emissions factors in supplemental information.
write_csv(total_nggi_summed_iterations, "data/outputTables/MonteCarloResults/total_nggi_summed_iterations_bothLmeanAndMean.csv")

# Calculate CIs, upper and lower 99% intervals, and min and max values
total_nggi_summed_summaries <- total_nggi_summed_iterations %>%
  group_by(analysis_description, scaled_up_using) %>%
  summarise(median_sum_total_MillionTonnesCO2PerYear = median(sum_total_MillionTonnesCO2PerYear),
            lower_ci_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.025),
            upper_ci_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.975),
            lower_99_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.01),
            upper_99_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.99),
            min_sum_total_MillionTonnesCO2PerYear = min(sum_total_MillionTonnesCO2PerYear),
            max_sum_total_MillionTonnesCO2PerYear = max(sum_total_MillionTonnesCO2PerYear))

# write to files
write_csv(total_nggi_summed_summaries, "data/outputTables/MonteCarloResults/total_nggi_summed_summaries_bothLmeanAndMean.csv")

# New analysis
# Do the same summaries but split by analyses type
total_df.saved.iterations["salinity"] <- mapply(classify_by_salinity,
                                                class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                class_time2 = as.character(total_df.saved.iterations$class_2010))
total_df.saved.iterations["stability"] <- mapply(classify_by_stability,
                                                 class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                 class_time2 = as.character(total_df.saved.iterations$class_2010))
total_df.saved.iterations["analysis_description"] <- mapply(paste, total_df.saved.iterations$salinity, " ", 
                                                            total_df.saved.iterations$stability, sep="")

# NGGI by salinity and stability
salinity_stability_summed_iterations <- total_df.saved.iterations %>%
  mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
  group_by(salinity, stability, analysis_description, iterationCode, scaled_up_using) %>%
  summarise(sum_total_MillionTonnesCO2PerYear = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T) / 5) %>%
  arrange(scaled_up_using, iterationCode)

salinity_stability_summed_iterations["emission_or_storage"] <- ifelse(salinity_stability_summed_iterations$sum_total_MillionTonnesCO2PerYear >= 0, "storage", "emission")

write_csv(salinity_stability_summed_iterations, "data/outputTables/MonteCarloResults/salinity_stability_summed_iterations_bothLmeanAndMean.csv")


salinity_stability_summed_summaries <- salinity_stability_summed_iterations %>% 
  group_by(analysis_description, salinity, stability, scaled_up_using) %>%
  summarise(median_sum_total_MillionTonnesCO2PerYear = median(sum_total_MillionTonnesCO2PerYear),
            lower_ci_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.025),
            upper_ci_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.975),
            lower_99_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.01),
            upper_99_sum_total_MillionTonnesCO2PerYear = quantile(sum_total_MillionTonnesCO2PerYear, 0.99),
            min_sum_total_MillionTonnesCO2PerYear = min(sum_total_MillionTonnesCO2PerYear),
            max_sum_total_MillionTonnesCO2PerYear = max(sum_total_MillionTonnesCO2PerYear))

# write to files
write_csv(salinity_stability_summed_summaries, "data/outputTables/MonteCarloResults/salinity_stability_summed_summaries_bothLmeanAndMean.csv")

{
  GWP_vs_SGWP_median_values<- read_csv("data/outputTables/GWP_vs_SGWP_median_values.csv")
  
  GWP_vs_SGWP_median_values["salinity"] <- mapply(classify_by_salinity,
                                                  class_time1 = as.character(GWP_vs_SGWP_median_values$class_2006),
                                                  class_time2 = as.character(GWP_vs_SGWP_median_values$class_2010))
  GWP_vs_SGWP_median_values["stability"] <- mapply(classify_by_stability,
                                                   class_time1 = as.character(GWP_vs_SGWP_median_values$class_2006),
                                                   class_time2 = as.character(GWP_vs_SGWP_median_values$class_2010))
  
  GWP_vs_SGWP_median_values_salinity_stability_summaries <- GWP_vs_SGWP_median_values %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(salinity, stability, analysis_type) %>%
    summarise(sum_total_MillionTonnesCO2PerYear = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T) / 5)
  
  GWP_vs_SGWP_median_values_total_sum_summaries <- GWP_vs_SGWP_median_values %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(analysis_type) %>%
    summarise(sum_total_MillionTonnesCO2PerYear = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T) / 5)
  
  GWP_vs_SGWP_median_values_summed_iterations <- GWP_vs_SGWP_median_values_summed_iterations[order(GWP_vs_SGWP_median_values_summed_iterations$analysis_type),]
  
  write_csv(GWP_vs_SGWP_median_values_summed_iterations, "data/outputTables/GWP_vs_SGWP_median_values_summed_iterations.csv")
  
}

