# simard_2006

Please cite this data as:  

Simard, M., Zhang, K., Rivera-Monroy, V. H., Ross, M. S., Ruiz, P. L., Castañeda-Moya, E., ... & Rodriguez, E. (2006). Mapping height and biomass of mangrove forests in Everglades National Park with SRTM elevation data. Photogrammetric Engineering & Remote Sensing, 72(3), 299-311.  

The [original](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/simard_2006/original) subfolder contains files as they were received.  

The [derivative](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/simard_2006/derivative) subfolder contains working files used in the data manipulation process, most importantly [simmard_2006_mangrove_site_summary.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/simard_2006/derivative/simmard_2006_mangrove_site_summary.csv) which contains the attributes:  
study_id: Unique study level identification made up of the author’s family name and publication year.  
site_id: Unique site-name generated from source.  
plot_id: Unique plot code generated from source.  
mean_agb_g_m2: Mean above ground biomass in grams per square meter.  
mean_tree_height: Mean tree height in meters.  
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.  

Data frame manipulations are documented in [scripts/0 formatting/biomass/6_simard_2006 _biomass_formatting.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/6_simard_2006_biomass_formatting.R)
