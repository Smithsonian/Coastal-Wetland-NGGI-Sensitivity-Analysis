# Byrd_2018

Please cite this data as:

Byrd, K.B., Ballanti, L.R., Thomas, N.M., Nguyen, D.K., Holmquist, J.R., Simard, M., Windham-Myers, L., Schile, L.M., Parker, V.T., Callaway, J.C., Vasey, M.C., Herbert, E.R., Davis, M.J., Woo, I., De La Cruz, S., Kroeger, K.D., Gonneea, M.E., O'Keefe Suttles, J., Megonigal, J.P., Lu, M., McFarland, E.K., Brooks, H.E.A., Drake, B.G., Peresta, G., Peresta, A., Troxler, T., and Castaneda-Moya, E., 2017, Biomass/Remote Sensing dataset: 30m resolution tidal marsh biomass samples and remote sensing data for six regions in the conterminous United States: U.S. Geological Survey data release, https://doi.org/10.5066/F77943K8.

The [original](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/byrd_2018/original) subfolder contains files as they were received.

The [derivative](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass/byrd_2018/derivative) subfolder contains the same BiomassSamples file as original, reformatted as a .csv file and [byrd_2018_site_summary.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/Biomass/byrd_2018/derivative/byrd_2018_site_summary.csv).  
Attributes include:  
study_id: Unique study level identification made up of the author’s family name and publication year.  
site_id: Unique site-name generated from source.  
plot_id: Unique plot code generated from source.  
mean_agb_g_m2: Mean above ground biomass in grams per square meter.  
vegetation_class: Rough vegetation class according to the Coastal Change Analysis Program definitions.  

Data frame manipulations are documented in [scripts/0 formatting/biomass/1_byrd_biomass_formatting.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/1_byrd_biomass_formatting.R)
