library(foreign)
library(tidyverse)

total_df.saved.iterations <- read_csv("data/outputTables/MonteCarloResults1/total_saved_iterations.csv")

impoundedWetlands <- read.dbf("data/impoundedFarmedWetlandArea/CCAP_EstPalbelowMHHWSIntersectingNWIImpoundedDitchedDrainedFarmed_FarmedBelowMHHWS_180614.dbf")

impoundedWetlands <- impoundedWetlands[order(-impoundedWetlands$Count),]


# Define assumptions re: land cover classes
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
               'Estuarine Forested Wetland')

scrubShrubVeg <- c('Scrub/Shrub',
                   'Palustrine Scrub/Shrub Wetland',
                   'Estuarine Scrub/Shrub Wetland')

farmedWetlands <- c('Cultivated', 'Pasture/Hay')

emergentVeg <- c('Cultivated', 'Pasture/Hay', 
                 'Grassland',
                 'Palustrine Emergent Wetland',
                 'Estuarine Emergent Wetland')

nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
           'Unconsolidated Shore', 'Bare Land', 'Water', 
           'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
           'Tundra',
           'Snow/Ice')

classify_by_wetland_type <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
  if (class_time1 %in% estuarineWetlands | class_time2 %in% estuarineWetlands) {
    wetland_class <- "Estuarine"
  } else if (class_time1 %in% palustrineWetlands | class_time2 %in% palustrineWetlands) {
    wetland_class <- "Palustrine"
  } else if (class_time1 %in% farmedWetlands | class_time2 %in% farmedWetlands)
    wetland_class <- "Farmed"
  else {
    salinity_class <- NA
  }
  return(wetland_class)
}

classify_by_stability <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
  if (class_time1 %in%  estuarineWetlands | class_time1  %in%  palustrineWetlands | class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
    if (class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
      stability_class <- "Stable and Gains"
    } else if ((! class_time2 %in% estuarineWetlands) & (! class_time2 %in% palustrineWetlands)) {
      stability_class <- "Losses"
    } else {
      stability_class <- ""
    }
  } else {
    stability_class <- "" 
  }
  return(stability_class)
}

total_df.saved.iterations["wetland_type"] <- mapply(classify_by_wetland_type,
                                                class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                class_time2 = as.character(total_df.saved.iterations$class_2010))
total_df.saved.iterations["stability"] <- mapply(classify_by_stability,
                                                 class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                 class_time2 = as.character(total_df.saved.iterations$class_2010))
total_df.saved.iterations["analysis_description"] <- mapply(paste, total_df.saved.iterations$wetland_type, " ", 
                                                            total_df.saved.iterations$stability, sep="")


estimated_areas <- total_df.saved.iterations %>%
  group_by(iteration_number, analysis_description) %>%
  summarise(sum_estimated_pixel_count = sum(estimated_pixel_count)) %>%
  mutate(sum_estimated_pixel_count_million_ha = sum_estimated_pixel_count * 900 / 10000 / 1E6) %>%
  group_by(analysis_description) %>%
  summarise(min_sum_estimated_pixel_count_million_ha = quantile(sum_estimated_pixel_count_million_ha, 0.025),
            med_sum_estimated_pixel_count_million_ha = quantile(sum_estimated_pixel_count_million_ha, 0.5),
            max_sum_estimated_pixel_count_million_ha = quantile(sum_estimated_pixel_count_million_ha, 0.975))

write_csv(estimated_areas, "data/outputTables/stateInventory/national_scale_estimated_wetland_areas_180615.csv")

impoundedWetlands["wetland_type"] <- mapply(classify_by_wetland_type,
                                                    class_time1 = as.character(impoundedWetlands$X2006_Clas),
                                                    class_time2 = as.character(impoundedWetlands$X2010_Clas))
impoundedWetlands["stability"] <- mapply(classify_by_stability,
                                                 class_time1 = as.character(impoundedWetlands$X2006_Clas),
                                                 class_time2 = as.character(impoundedWetlands$X2010_Clas))
impoundedWetlands["analysis_description"] <- mapply(paste, impoundedWetlands$wetland_type, " ", 
                                                    impoundedWetlands$stability, sep="")


estimated_impoundedFarmed <- impoundedWetlands %>%
  group_by(analysis_description) %>%
  summarise(sum_mapped_pixel_count = sum(Count)) %>%
  mutate(sum_mapped_pixel_count_million_ha = sum_mapped_pixel_count * 900 / 10000 / 1E6) %>%
  select(-sum_mapped_pixel_count)

write_csv(estimated_impoundedFarmed, "data/outputTables/stateInventory/national_scale_impoundedDrainedFarmed_areas_180615.csv")
