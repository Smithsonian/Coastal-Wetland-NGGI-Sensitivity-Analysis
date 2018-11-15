# outputTables

This subfolder contains multiple files tracking and summarizing the outputs of the mapping efforts, the Monte Carlo uncertainty propagation, and the sensitivity analysis.  

The [MonteCarloResults](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/outputTables/MonteCarloResults) subfolder contains multiple files tracking and summarizing the outputs of the Monte Carlo analysis.  

The [sensitivityAnalysisResults](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/outputTables/sensitivityAnalysisResults) subfolder contains multiple files tracking and summarizing the outputs of the sensitivity analysis.  


[GWP_vs_SGWP_median_values.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/GWP_vs_SGWP_median_values.csv) – Median values broken down by Coastal Change Analysis Program (C-CAP) landcover class, by greenhouse gas flux sector, and by method for converting methane to CO<sub>2</sub> equivalents.  

Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class Abbreviation  
class – 2006 to 2011 C-CAP change class  
abbrev	 - 2006 to 2011 C-CAP change class abbreviation  
variableType – Whether mapped area is treated as a fixed or random variable  
mappedPixelCount – Mean mapped 30 x 30 meter pixel count  
mappedPixelCountSD – Standard deviation of the mapped 30 x 30 meter pixel count  
estimated_pixel_count – Mean estimated 30 x 30 meter pixel count  
total_gCO2perM2 – Total greenhouse gas flux in grams of CO<sub>2</sub> equivalent per meter square  
soil_gCO2perM2 - Soil greenhouse gas flux in grams of CO<sub>2</sub> per meter square	 
biomass_gCO2perM2 - Biomass greenhouse gas flux in grams of CO<sub>2</sub> per meter square  
methane_gCO2perM2 - Methane greenhouse gas flux in grams of CO<sub>2</sub> equivalent per meter square  
analysis_type – Code indicating whether a global warming potential (GWP) or sustained global warming/cooling potential (SGW/CP) were used to convert methane to CO<sub>2</sub> equivalent  


[GWP_vs_SGWP_median_values_summed_iterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/GWP_vs_SGWP_median_values_summed_iterations.csv) – Sum total fluxes in million tonnes of CO<sub>2</sub> equivalents per year broken down by salinity, stability, and by method for converting methane to CO<sub>2</sub> equivalents.  

Attributes include:  
salinity - Salinity class as defined by Coastal Change Analysis Program. Either Estuarine or Palustrine.  
stability – _Stable and Gains_ include wetlands that do not change and other landcover classes that convert to wetlands. _Losses_ include wetlands that convert to other landcover classes.  
analysis_type	- Code indicating whether a global warming potential (GWP) or sustained global warming/cooling potential (SGW/CP) were used to convert methane to CO<sub>2</sub> equivalent.  
sum_total_MillionTonnesCO2PerYear – Sum total greenhouse gas flux in million tonnes of CO<sub>2</sub> equivalents per year.  


[totalMappedPixels_coastalLands.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/totalMappedPixels_coastalLands.csv) – Area of wetlands as classified by the coastal change analysis program with palustrine wetland area constrained by a “coastal lands” definition, counting any wetland falling below the elevation of Mean Higher High Water Spring Tides, whether or not they are actually tidal. This analysis uses a probabilistic coastal lands map and so considers all palustrine lands as random rather than fixed values.  

Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class Abbreviation  
class – 2006 to 2011 C-CAP change class  
abbrev	 - 2006 to 2011 C-CAP change class abbreviation  
variableType – Whether mapped area is treated as a fixed or random variable  
mappedPixelCount – Mean mapped 30 x 30 meter pixel count  
mappedPixelCountSD – Standard deviation of the mapped 30 x 30 meter pixel count  


[totalMappedPixels_NWI.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/totalMappedPixels_NWI.csv) - Area of wetlands as classified by the coastal change analysis program with palustrine wetland area constrained by the National Wetlands Inventory, including any palustrine wetland categorized as tidal by NWI.  

Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class Abbreviation  
class – 2006 to 2011 C-CAP change class  
abbrev	 - 2006 to 2011 C-CAP change class abbreviation  
variableType – Whether mapped area is treated as a fixed or random variable  
mappedPixelCount – Mean mapped 30 x 30 meter pixel count  
mappedPixelCountSD – Standard deviation of the mapped 30 x 30 meter pixel count  
