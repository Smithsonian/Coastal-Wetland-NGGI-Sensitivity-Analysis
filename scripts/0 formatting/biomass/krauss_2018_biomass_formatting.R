library(tidyverse)
source('scripts/0 formatting/forested_biomass/allometric_equations.R') # load up allometric equations

# Load Ken Krauss's data
krauss_tff <- read_csv("data/Biomass/krauss_2018/original/Carbonbudgetass/TFFW_Carbon_Budget_Stand_Structure_2005_and_2012.csv")

# Load derrivative species names
krauss_species_codes <- read_csv("data/Biomass/krauss_2018/derrivative/krauss_common_names_table.csv")

genus_species <- unique(paste(krauss_species_codes$genus, krauss_species_codes$species, sep="_"))

# Change names accoridng to best practices
names(krauss_tff) <- c("river", "site", "year", "plot_id", "tree_id", "species_code", "dbh", "live_or_dead")

# define no data and live/dead values in standardized way
krauss_tff[krauss_tff == '.'] <- NA # NA values
krauss_tff$live_or_dead[krauss_tff$live_or_dead == 1] <- "live" # change 1 to 'live' 
krauss_tff$live_or_dead[krauss_tff$live_or_dead == 0] <- "dead" # change 0  to 'dead'

# Load leaf productivity data
krauss_leaf_production <- read_csv("data/Biomass/krauss_2018/original/Carbonbudgetass/TFFW_Carbon_Budget_Wood_Increment_and Litterfall_2005_2015.csv")

krauss_leaf_production_summary <- krauss_leaf_production %>% 
  filter(Variable == "Litterfall") %>%
  select(Year, River, Upper, Middle, Lower) %>% 
  gather("site", "leaf_production_g_m2_yr", Upper:Lower) %>% 
  mutate(site_id = paste(River, site, sep="_"),
         year=Year) %>% 
  select(site_id, year, leaf_production_g_m2_yr) %>%
  group_by(site_id) %>% 
  summarise(mean_leaf_production_g_m2_yr = mean(leaf_production_g_m2_yr, na.rm = T))

# create side_id's and join to species code table
krauss_tff_tall <- krauss_tff %>% 
  mutate(site_id = paste(river, "_", site, sep="")) %>% # create a species code by pasting genus and species together
  select(site_id, plot_id, tree_id, species_code, live_or_dead, year, dbh) %>%  # select important variables
  left_join(krauss_species_codes, by = "species_code") # joing data table to derrivative table of species names 
  
agb <- mapply(estimate_general_agb_from_dbh_jones_2003, 
              dbh = krauss_tff_tall$dbh, 
              genus = as.character(krauss_tff_tall$genus), 
              species = as.character(krauss_tff_tall$species))

# add a study id code
krauss_tff_tall["agb"] <- agb
krauss_tff_tall["study_id"] <- rep("krauss_2018", nrow(krauss_tff_tall))
krauss_tff_tall["plot_area_m2"] <- rep(500, nrow(krauss_tff_tall))
View(krauss_tff_tall)

# put columns in a better order
krauss_tff_tall <- krauss_tff_tall %>% 
  select(study_id, site_id, plot_id, tree_id,
         genus, species, species_code, live_or_dead, 
         year, dbh, plot_area_m2, agb)

# check for species that didn't have an allometric equation
krauss_tff_tall_NA <- krauss_tff_tall %>% 
  filter(is.na(agb))
print(unique(paste(krauss_tff_tall_NA$genus, krauss_tff_tall_NA$species, sep="_")))

# write tall version of file to derrivative files
write_csv(krauss_tff_tall, "data/Biomass/krauss_2018/derrivative/krauss_2018_forest_plots_tall.csv")

krauss_2018_site_summary <- krauss_tff_tall %>% 
   filter(live_or_dead == "live") %>% # remove dead plants
   group_by(study_id, site_id, plot_id, year, plot_area_m2) %>%  # group by plots and year
   summarise(sum_agb = sum(agb, na.rm=T)) %>%  # summarize all of the trees within a plot
   mutate(sum_agb_g_m2 = (sum_agb / plot_area_m2)) %>%  # calculate the gOM per m2
   group_by(study_id, site_id, plot_id) %>% # group by plot
   summarise(mean_agb_g_m2 = mean(sum_agb_g_m2, na.rm=T))

krauss_2018_site_summary["vegetation_class"] <- rep("forest", nrow(krauss_2018_site_summary)) # Metadata states all are forest plots


write_csv(krauss_2018_site_summary, "data/Biomass/krauss_2018/derrivative/krauss_2018_site_summary.csv")
 
ggplot(data=krauss_2018_site_summary, aes(x=mean_agb_g_m2)) +
   geom_density()
