# craft

Please cite this data as:  

Craft, Christopher B. 2017. Dendrometer band measurement data from two tidal forest plots at GCE 11 on the Altamaha River in Southeast Georgia from December 2014 to December 2016. Georgia Coastal Ecosystems LTER Project, University of Georgia, Long Term Ecological Research Network. http://dx.doi.org/10.6073/pasta/63da736bc564aa2514e63c29b771ecf7  

The [original](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/lter/craft/original) subfolder contains files as they were downloaded.  

The [derivative](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/lter/craft/derivative) subfolder contains working files used in the data manipulation process, most importantly [craft_2013_site_summary.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/lter/craft/derivative/craft_2013_site_summary.csv) which contains the attributes:  
study_id: Unique study level identification made up of the author’s family name and publication year.  
site_id: Unique site-name generated from source.  
plot_id: Unique plot code generated from source.  
mean_agb_g_m2: Mean above ground biomass in grams per square meter.  
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.  

Data frame manipulations are documented in [scripts/0 formatting/biomass/2_craft_2013 _biomass_formatting.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/2_craft_2013_biomass_formatting.R)
