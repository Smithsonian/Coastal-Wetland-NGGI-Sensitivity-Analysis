# Formatting doughty data
library(tidyverse)
source('scripts/0 formatting/forested_biomass/allometric_equations.R') # load up allometric equations

# don't read original file, read one formatted by hand
doughty_2015 <- read_csv("data/Biomass/doughty_2015/derrivative/doughty_2015_formatted.csv")

doughty_2015_species_codes <- as_tibble(data.frame(species_code = c("RM", "AG", "LR"),
                                                   genus = c("Rhizophora", "Avicennia", "Laguncularia"),
                                                   species = c("mangle", "germinans", "racemosa"),
                                                   stringsAsFactors = F)) 
doughty_2015B <- doughty_2015 %>% 
  left_join(doughty_2015_species_codes)

agb <- mapply(estimate_mangrove_agb_from_dbh_doughty_2015, dbh = doughty_2015B$dbh,
                                                   d30 = doughty_2015B$d30,
                                                   genus = doughty_2015B$genus,
                                                   species = doughty_2015B$species)
doughty_2015B["agb"] <- agb
doughty_2015B["study_id"] <- rep("doughty_2015", by=nrow(doughty_2015B)) # add a study id column

# write tall form of data frame to derrivative files
write_csv(doughty_2015B, "data/Biomass/doughty_2015/derrivative/doughty_2015_tall.csv")

# plot level synthesis
doughty_2015_site_summary <- doughty_2015B %>% 
  group_by(study_id, site_id, plot_id, plot_area_m2) %>%  # group by plots and year
  summarise(sum_agb = sum(agb, na.rm=T),
            mean_tree_height = mean(tree_height, na.rm=T)) %>%  # summarize all of the trees within a plot
  mutate(sum_agb_g_m2 = (sum_agb / plot_area_m2)) %>%  # calculate the gOM per m2
  group_by(study_id, site_id, plot_id) %>% # group by plot
  summarise(mean_agb_g_m2 = mean(sum_agb_g_m2, na.rm=T),
            mean_tree_height = mean(mean_tree_height, na.rm=T)) %>%   # average accross years
  mutate(vegetation_class = ifelse(mean_tree_height >= 5, "forest", "shrub")) # we have tree heights so let's additionally categorize as forest or shrub

write_csv(doughty_2015_site_summary, "data/Biomass/doughty_2015/derrivative/doughty_2015_site_summary.csv")

ggplot(data=doughty_2015_site_summary, aes(x=mean_agb_g_m2)) +
  geom_density()
