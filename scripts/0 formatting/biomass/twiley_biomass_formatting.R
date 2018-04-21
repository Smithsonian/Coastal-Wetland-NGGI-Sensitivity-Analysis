library(tidyverse)
library(lubridate)
source('scripts/0 formatting/biomass/allometric_equations.R') # load up allometric equations

# Mangrove data from LTER
twiley_mangrove <- read_csv("data/Biomass/lter/twiley/original/archive_knb-lter-fce.1113.3_152217261453162253/LT_PP_Rivera_002.txt")

# Create a species code table
#   A= Avicennia germinans
#   C= Conacarpus erectus
#   L= Laguncularia racemosa
#   R= Rhizophora mangle
species_code_table <- as.tbl(data.frame(species_code = c("A", "C", "L", "R"),
                                 genus = c("Avicennia", "Conacarpus", "Laguncularia", "Rhizophora"),
                                 species = c("germinans", "erectus", "racemosa", "mangle"), stringsAsFactors = F))

# rename using best practices
names(twiley_mangrove) <- c("site_id", "date", "plot_id", "tree_id", 
                            "species_code", "dbh", "tree_height")

twiley_mangrove[twiley_mangrove == -9999] <- NA # format no data values 
twiley_mangrove["study_id"] <- rep("twilley_2016", by=nrow(twiley_mangrove)) # add a study id column
twiley_mangroveB <- twiley_mangrove %>%
  separate(date, c("year", "month", "day"), "-") %>% # separate months days and years
  left_join(species_code_table) %>% # join species code table
  # order columns
  select(study_id, site_id, plot_id, tree_id, year, month, day, species_code, genus, species, dbh, tree_height)

agb <- mapply(estimate_mangrove_agb_from_dbh_smith_2006, twiley_mangroveB$dbh, twiley_mangroveB$genus, twiley_mangroveB$species)
twiley_mangroveB["agb"] <- agb
twiley_mangroveB["plot_area_m2"] <- rep(400, nrow(twiley_mangroveB))

# write tall form of data frame to derrivative files
write_csv(twiley_mangroveB, "data/Biomass/lter/twiley/derrivative/twiley_2016_mangrove_plots_tall.csv")

# Create 
twiley_2016_site_summary <- twiley_mangroveB %>% 
  group_by(study_id, site_id, plot_id, year, plot_area_m2) %>%  # group by plots and year
  summarise(sum_agb = sum(agb, na.rm=T),
            mean_tree_height = mean(tree_height, na.rm=T)) %>%  # summarize all of the trees within a plot
  mutate(sum_agb_g_m2 = (sum_agb / plot_area_m2)) %>%  # calculate the gOM per m2
  group_by(study_id, site_id, plot_id) %>% # group by plot
  summarise(mean_agb_g_m2 = mean(sum_agb_g_m2, na.rm=T),
            mean_tree_height = mean(mean_tree_height, na.rm=T)) %>%   # average accross years
  mutate(vegetation_class = ifelse(mean_tree_height >= 5, "forest", "shrub")) # we have tree heights so let's additionally categorize as forest or shrub

write_csv(twiley_2016_site_summary, "data/Biomass/lter/twiley/derrivative/twiley_2016_site_summary.csv")

ggplot(data=twiley_2016_site_summary, aes(x=mean_agb_g_m2)) +
  geom_density()
