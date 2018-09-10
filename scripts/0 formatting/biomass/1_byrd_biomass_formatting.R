# Formatting for Byrd 2018 biomass data

kristinBiomass <- read_csv("data/Biomass/byrd_2018/derivative/BiomassSamples.csv")
kristinBiomass <- subset(kristinBiomass, biomass_gm > 0)

vegetation_class <- ifelse(kristinBiomass$sp1 == "Iva frutescens" & kristinBiomass$pc_sp1 >= 0.50, "shrub", "emergent")
kristinBiomass["vegetation_class"] <- vegetation_class
kristinBiomass["study_id"] <- rep("byrd_2018", nrow(kristinBiomass))

kristinBiomassB <- kristinBiomass %>% 
  select(study_id = study_id,
         site_id = tier3site, 
         plot_id = quadid,
         mean_agb_g_m2 = biomass_gm,
         vegetation_class = vegetation_class
         )

write_csv(kristinBiomassB, "data/Biomass/byrd_2018/derivative/byrd_2018_site_summary.csv")
