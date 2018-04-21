# Simard's data uses a variable range rather than a fixed range
library(tidyverse)
source('scripts/0 formatting/biomass/allometric_equations.R') # load up allometric equations
baf <- 5

simard_2006 <- read_csv("data/Biomass/simard_2006/derrivative/Florida_Trees.csv")

simard_2006_tall <- simard_2006 %>% 
  separate(plot_name, c("site_id", "plot_id"), "_")

names(simard_2006_tall) <- c("site_id", "plot_id", "tree_id", "live_or_dead", "genus", "species", 
                             "fia_code", "dbh", "tree_height", "notes", "use_for_allometry")

simard_2006_tall["study_id"] <- rep("simard_2006", by=nrow(simard_2006_tall)) # add a study id column

agb <- mapply(estimate_mangrove_agb_from_dbh_simard_2006, simard_2006_tall$dbh, simard_2006_tall$genus, simard_2006_tall$species)
simard_2006_tall["agb"] <- agb
simard_2006_tall$live_or_dead <- ifelse(simard_2006_tall$live_or_dead == 1, "live", "dead")

site_plot_information <- simard_2006_tall %>% 
  group_by(study_id, site_id, plot_id) %>% 
  tally() %>% 
  mutate(tree_area_m2_to_plot_area_ha = n * baf,
         tree_area_m2_to_plot_area_m2 = tree_area_m2_to_plot_area_ha / 10000)

simard_2006_tall <- simard_2006_tall %>% 
  mutate(tree_area_m2 = (3.1415 * (dbh / 2)^2 / 10000)) %>% 
  left_join(site_plot_information) %>% 
  mutate(plot_size_m2 = tree_area_m2 / tree_area_m2_to_plot_area_m2,
         agb_m2 = agb / plot_size_m2) %>% 
  select(study_id, site_id, plot_id, tree_id, live_or_dead, genus, species, dbh, tree_height,
         agb, tree_area_m2, n, tree_area_m2_to_forest_area_m2, plot_size_m2, agb_m2) %>% 
  filter(! is.na(dbh))

# write summary to derrivative files
write_csv(simard_2006_tall, "data/Biomass/simard_2006/derrivative/simmard_2006_mangrove_plots_tal.csv")

# Create 
simard_2006_site_summary <- simard_2006_tall %>% 
  group_by(study_id, site_id, plot_id) %>%  # group by plots and year
  summarise(mean_agb_g_m2 = mean(agb_m2, na.rm=T),
            mean_tree_height = mean(tree_height, na.rm=T)) %>%   # average accross years
  mutate(vegetation_class = ifelse(mean_tree_height >= 5, "forest", "shrub")) # we have tree heights so let's additionally categorize as forest or shrub

write_csv(simard_2006_site_summary, "data/Biomass/simard_2006/derrivative/simmard_2006_mangrove_site_summary.csv")

ggplot(data=simard_2006_site_summary, aes(x=mean_agb_g_m2)) +
  geom_density()


