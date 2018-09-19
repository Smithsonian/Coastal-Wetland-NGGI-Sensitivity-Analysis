# Creates a master dataset of all of the various sources for biomass data

library(tidyverse)

krauss_2018 <- read_csv("data/Biomass/krauss_2018/derivative/krauss_2018_site_summary.csv")
krauss_2018 <- krauss_2018 %>% 
  mutate(plot_id = as.character(plot_id))
twiley_2016 <- read_csv("data/Biomass/lter/twiley/derivative/twiley_2016_site_summary.csv")
twiley_2016 <- twiley_2016 %>% 
  mutate(plot_id = as.character(plot_id))
simard_2006 <- read_csv("data/Biomass/simard_2006/derivative/simmard_2006_mangrove_site_summary.csv")
simard_2006 <- simard_2006 %>% 
  mutate(plot_id = as.character(plot_id))
megonigal_1997 <- read_csv("data/Biomass/megonigal_1997/derivative/megonigal_1997_site_summary.csv")
doughty_2015 <- read_csv("data/Biomass/doughty_2015/derivative/doughty_2015_site_summary.csv")
craft_2013 <- read_csv("data/Biomass/lter/craft/derivative/craft_2013_site_summary.csv")
craft_2013 <- craft_2013 %>% 
  mutate(plot_id = as.character(plot_id))
byrd_2018 <- read_csv("data/Biomass/byrd_2018/derivative/byrd_2018_site_summary.csv")

master_data <- byrd_2018 %>% 
  full_join(megonigal_1997) %>% 
  full_join(krauss_2018)   %>% 
  full_join(twiley_2016) %>% 
  full_join(craft_2013)  %>% 
  full_join(simard_2006)  %>% 
  full_join(doughty_2015) %>% 
  mutate(g_co2_m2 = mean_agb_g_m2 * 0.48 * 3.6667,
         study = str_replace(paste(toupper(substring(study_id, 1,1)), substring(study_id, 2), sep=""), "_", " "))

write_csv(master_data, "data/Biomass/biomass_gCO2_master_summaries.csv")

master_data_summary <- master_data %>% 
  group_by(vegetation_class) %>% 
  summarise(logmean = mean(log(g_co2_m2), na.rm = T),
            logsd = sd(log(g_co2_m2), na.rm = T),
            mean = mean(g_co2_m2, na.rm = T),
            n = length(g_co2_m2))

write_csv(master_data_summary, "data/Biomass/biomass_gCO2_distribution_summaries.csv")

