# krauss_2018

Please cite this data as:  

Krauss, K.W., Noe, G.B., Duberstein, J.A., Conner, W.H., Jones, M.C., Bernhardt, C.E., Cormier, Nicole, and From, A.S., 2018, Carbon budget assessment of tidal freshwater forested wetland and oligohaline marsh ecosystems along the Waccamaw and Savannah rivers, U.S.A. (2005-2016): U.S. Geological Survey data release, https://doi.org/10.5066/F7TM7930. (release date, 1/19/2018)  

The [original](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/krauss_2018/original) subfolder contains files as they were downloaded.  

The [derivative](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/krauss_2018/derivative) subfolder contains working files used in the data manipulation process, most importantly [krauss_2018_site_summary.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/krauss_2018/derivative/krauss_2018_site_summary.csv) which contains the attributes:  
study_id: Unique study level identification made up of the author’s family name and publication year.  
site_id: Unique site-name generated from source.  
plot_id: Unique plot code generated from source.  
mean_agb_g_m2: Mean above ground biomass in grams per square meter.  
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.  

Data frame manipulations are documented in [scripts/0 formatting/biomass/4_krauss_2018 _biomass_formatting.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/4_krauss_2018_biomass_formatting.R)
