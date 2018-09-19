library(tidyverse)
library(lubridate)

# Package ID: knb-lter-gce.627.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Forest survey of species richness and basal area for two tidal forest plots at GCE 11 
#   on the Altamaha River in Southeast Georgia in December 2013.
# Data set creator:    - Georgia Coastal Ecosystems LTER Project 
# Data set creator: Dr. Christopher Craft - Indiana University at Bloomington 
# Metadata Provider:    -  
# Contact:    - GCE-LTER Information Manager   - gcelter@uga.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

# Text of metadata here http://gce-lter.marsci.uga.edu/public/datasets/metadata/PLT-GCED-1711a_1_0-META.TXT

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-gce/627/4/dc935121c0dbbf887b4f43fe8c28f1a9" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=5
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site_Code",     
                 "Plot",     
                 "Quadrant",     
                 "Tree_Species",     
                 "DBH",     
                 "Tag_Number",     
                 "BasalArea"    ), check.names=TRUE)

craft_2013_tff <- as_tibble(dt1)
print(unique(craft_2013_tff$Tree_Species))

# Tree_Species: AcRu = Acer rubrum, CaCa = Carpinus caroliniana, FrPe = Fraxinus 
#   pennsylanica, LiSt = Liquidambar styraciflua, MaVi = Magnolia virginiana, NyAq = 
#   Nyssa aquatica, NyBi = Nyssa sylvatica var. biflora, QuLy = Quercus lyrata, TaDi = 
#   Taxodium distichum, Oak = Quercus spp.
craft_species_codes <- as_tibble(data.frame(species_code = c("AcRu", "CaCa", "FrPe", "LiSt", "MaVi", "NyAq", 
                                                             "NyBi", "Oak",  "QuLy", "TaDi"),
                                 genus = c("Acer", "Carpinus", "Fraxinus", "Liquidambar", "Magnolia", "Nyssa", 
                                           "Nyssa", "Quercus", "Quercus", "Taxodium"),
                                 species = c("rubrum", "caroliniana", "pennsylanica", "styraciflua", 
                                             "virginiana", "aquatica", "sylvatica", "spp", "lyrata", 
                                             "distichum"), stringsAsFactors = F))

# standardize names
names(craft_2013_tff) <- c("site_id", "plot_id", "quadrant_id", "species_code", "dbh", "tree_id", "basal_area")

# create side_id's and join to species code table
craft_2013_tff_tall <- craft_2013_tff %>% 
  left_join(craft_species_codes) # joing data table to derivative table of species names 

# etimate_general_agb_from_dbh_jones_2003
agb <- mapply(etimate_general_agb_from_dbh_jones_2003, 
              dbh = craft_2013_tff_tall$dbh, 
              genus = as.character(craft_2013_tff_tall$genus), 
              species = as.character(craft_2013_tff_tall$species))

# add a study id code
craft_2013_tff_tall["agb"] <- agb
craft_2013_tff_tall["study_id"] <- rep("craft_2013", nrow(craft_2013_tff_tall))
craft_2013_tff_tall["plot_area_m2"] <- rep(1000, nrow(craft_2013_tff_tall)) # 0.1-ha = 1000 square meters
View(craft_2013_tff_tall)


# put columns in a better order
craft_2013_tff_tall <- craft_2013_tff_tall %>% 
  select(study_id, site_id, plot_id, quadrant_id, tree_id,
         genus, species, species_code, 
        dbh, plot_area_m2, agb)

# check for species that didn't have an allometric equation
craft_2013_tff_tall_NA <- craft_2013_tff_tall %>% 
  filter(is.na(agb))
print(unique(paste(craft_2013_tff_tall_NA$genus, craft_2013_tff_tall_NA$species, sep="_")))

# write tall version of file to derivative files
write_csv(craft_2013_tff_tall, "data/Biomass/lter/craft/derivative/craft_2013_forest_plots_tall.csv")

# create plot summaries of grams above ground biomass per meter squared
craft_2013_site_summary <- craft_2013_tff_tall %>% 
  group_by(study_id, site_id, plot_id, plot_area_m2) %>%  # group by plots and year
  summarise(sum_agb = sum(agb, na.rm=T)) %>%  # summarize all of the trees within a plot
  mutate(sum_agb_g_m2 = (sum_agb / plot_area_m2)) %>%  # calculate the gOM per m2
  group_by(study_id, site_id, plot_id) %>% # group by plot
  summarise(mean_agb_g_m2 = mean(sum_agb_g_m2, na.rm=T)) # average accross years
# Metadata states all are forest plots
craft_2013_site_summary["vegetation_class"] <- rep("forest", nrow(craft_2013_site_summary)) 

# write site summary to derivative files
write_csv(craft_2013_site_summary, "data/Biomass/lter/craft/derivative/craft_2013_site_summary.csv")

# plot to visualize results
ggplot(data=craft_2013_site_summary, aes(x=mean_agb_g_m2)) +
  geom_histogram()
