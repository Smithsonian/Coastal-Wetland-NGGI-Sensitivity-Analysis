# Script writing functions for clipping Pat Megonigal's 1997 SAS code and paper
# Freshwater Forested Biomass Datasets

# load necessary packages
library(tidyverse)
source('scripts/0 formatting/forested_biomass/allometric_equations.R') # load up allometric equations


# load the file skipping first 5 lines with text
megonigal_tff <- read_tsv("data/Biomass/megonigal_1997/original/Dbhall.txt", skip=5) 
megonigal_species_codes <- read_csv("data/Biomass/megonigal_1997/derivative/megonigal_1997_species_code_table.csv")
megoginal_leaf_productivity <- read_csv ("data/Biomass/megonigal_1997/derivative/megonigal_1997_leaf_production.csv") # leaf biomass

# assign variable names conforming to best practices
names(megonigal_tff) <- c("site_id", "plot_id", "tree_id", "species_code", "live_or_dead", "1987", "1988", "1989")

# make a tall form version of the leaf productivity data
megoginal_leaf_productivity_summary <- megoginal_leaf_productivity %>% 
  gather("year", "leaf_production_g_m2_yr", "year_1987":"year_1988") %>% 
  mutate(year = as.numeric(str_replace(year, "year_", ""))) %>% 
  group_by(site_id, plot_id) %>% 
  summarise(mean_leaf_production_g_m2_yr = mean(leaf_production_g_m2_yr, na.rm=T))

# make tall form using a dply gather function
megonigal_tff_tall <- megonigal_tff %>% 
  gather("year", "dbh", '1987':'1989') %>% # key = year, value = dby, gather columns with years in them
  mutate(year = as.numeric(year), # force year to be numeric
         plot_area_m2 = ifelse(site_id == "VERRET", 1000, 500)) %>% # add plot size as a variable
  filter(live_or_dead != "F")  %>% # remove False values for live or dead
  left_join(megonigal_species_codes, by = "species_code")  # joing genus and species from the attached table

# replace study specific coding with standardized coding
megonigal_tff_tall$live_or_dead[megonigal_tff_tall$live_or_dead == "L"] <- "live" # change L to live
megonigal_tff_tall$live_or_dead[megonigal_tff_tall$live_or_dead == "D"] <- "dead" # change D to dead
megonigal_tff_tall[megonigal_tff_tall==-99.9] <- NA # NA values
megonigal_tff_tall[megonigal_tff_tall==-99.0] <- NA # NA values
megonigal_tff_tall$genus[is.na(megonigal_tff_tall$genus)] <- "Other" # Change NA values to 'Other'
megonigal_tff_tall$species[is.na(megonigal_tff_tall$species)] <- "spp" # Change NA values to 'spp'

megonigal_tff_tall["study_id"] <- rep('megonigal_1997', nrow(megonigal_tff_tall)) # add a study ID

# Run function on the total AGB table
agb <- mapply(estimate_tff_agb_from_dbh_megonigal_1997, 
              dbh = megonigal_tff_tall$dbh, 
              genus = as.character(megonigal_tff_tall$genus),
              species = as.character(megonigal_tff_tall$species))
megonigal_tff_tall["agb"] <- agb

# write tall version of file to derivative files
write_csv(megonigal_tff_tall, "data/Biomass/megonigal_1997/derivative/megonigal_1997_forest_plots_tall.csv")

megonigal_1997_site_summary <- megonigal_tff_tall %>% 
  filter(live_or_dead == "live") %>% # remove dead plants
  group_by(study_id, site_id, plot_id, year, plot_area_m2) %>%  # group by plots and year
  summarise(sum_agb = sum(agb, na.rm=T)) %>%  # summarize all of the trees within a plot
  mutate(sum_agb_g_m2 = (sum_agb / plot_area_m2)) %>%  # calculate the gOM per m2
  group_by(study_id, site_id, plot_id) %>% # group by plot
  summarise(mean_agb_g_m2 = mean(sum_agb_g_m2, na.rm=T))  # average across years

megonigal_1997_site_summary["vegetation_class"] <- rep("forest", nrow(megonigal_1997_site_summary))

write_csv(megonigal_1997_site_summary, "data/Biomass/megonigal_1997/derivative/megonigal_1997_site_summary.csv")

ggplot(data=megonigal_1997_site_summary, aes(x=mean_agb_g_m2)) +
  geom_density()
