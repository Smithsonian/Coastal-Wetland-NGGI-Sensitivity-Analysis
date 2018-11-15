#Biomass 

The Biomass folder contains files used in our synthesis of above ground biomass. 

The [byrd_2018](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/byrd_2018), [doughty_2015](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/doughty_2015), [krauss_2018](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/krauss_2018), [lter](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/lter), [megonigal_1997](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/megonigal_1997), [simard_2006](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/simard_2006) subfolders contain datasets brought into the data synthesis effort. 

[biomass_gCO2_master_summaries.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/biomass_gCO2_distribution_summaries.csv) – syntheses of all above ground biomass datasets into a common format.
attributes include:
study_id: Unique study level identification made up of the author’s family name and publication year
site_id: Unique site-name generated from source. 
plot_id: Unique plot code generated from source.
mean_agb_g_m2: Mean above ground biomass in grams per square meter.
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.
mean_tree_height: If available, mean tree height in meters
g_co2_m2: Mean above ground biomass in grams of CO<sub>2</sub> per square meter.
study – same as study_id, but better readability for graphing

[biomass_gCO2_distribution_summaries.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/biomass_gCO2_master_summaries.csv) – aboveground biomass summary statistics needed for the monte carlo analysis.
attributes include:
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.
logmean: Mean of the natural-log transformed mean above ground biomass in ln(grams of CO<sub>2</sub> per square meter).	
logsd: Standard deviation of the natural-log transformed mean above ground biomass in ln(grams of CO<sub>2</sub> per square meter).
n: Number of plots analyzed
