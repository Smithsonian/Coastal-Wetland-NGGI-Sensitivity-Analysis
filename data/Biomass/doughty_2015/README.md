# doughty_2015
Please cite this data as:

Doughty, C. L., Langley, J. A., Walker, W. S., Feller, I. C., Schaub, R., & Chapman, S. K. (2016). Mangrove range expansion rapidly increases coastal wetland carbon storage. Estuaries and Coasts, 39(2), 385-396.  

The [original](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/doughty_2015/original) subfolder contains files as they were received. 

The [derivative](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/doughty_2015/derivative) subfolder contains working files used in the data manipulation process, most importantly [doughty_2015_site_summary.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/doughty_2015/derivative/doughty_2015_site_summary.csv) which contains the attributes:  
  study_id: Unique study level identification made up of the author’s family name and publication year  
  site_id: Unique site-name generated from source.  
  plot_id: Unique plot code generated from source.  
  mean_agb_g_m2: Mean above ground biomass in grams per square meter.  
  mean_tree_height: Mean tree height in meters.  
  vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.  

Data frame manipulations are documented in [scripts/0 formatting/biomass/3_doughty_mangrove_biomass_formatting.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/3_doughty_mangrove_biomass_formatting.R)
