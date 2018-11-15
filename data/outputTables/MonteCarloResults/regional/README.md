# regional 

This folder contains working files and results of a version of the Monte Carlo analysis which unscaled lognormally distributed variables using exponentiated log-means rather than means.  

[ccap2010perPixelScalers.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/ccap2010perPixelScalers.savedIterations.csv) – Estimated to mapped are ratio for each Coastal Change Analysis Program 2010/2011 class broken down by Monte Carlo Iteration.  
HID – High intensity developed  
MID – Medium intensity developed  
LID – Low intensity developed  
OSD – developed open space  
CULT - Cultivated  
PAST – Pasture/Hay  
GRS - Grassland  
DEC – Deciduous forest  
EVR – Evergreen forest  
MIX – Mixed forest  
SS – Scrub/Shrub  
PFW – Palustrine forested wetland  
PSS – Palustrine scrub/shrub wetland  
PEM – Palustrine emergent marsh  
EFW – Estuarine forested wetland  
ESS – Estuarine scrub/shrub  
EEM – Estuarine emergent wetland  
UCS – Unconsolidated shore  
BAR – Bare land  
OW – Open water  
PAB – Palustrine aquatic bed  
EAB – Estuarine aquatic bed  
SNOW – Snow  

iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration  


[cncPerPixelScalers.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/cncPerPixelScalers.savedIterations.csv) - Estimated to mapped are ratio for Coastal Change Analysis Program 2006 to 2011 change detection broken down by Monte Carlo Iteration.  
Attributes include:  
No.Change – Estimated to mapped are ratio for categories that did not change between 2006 and 2011.  
Change – Estimated to mapped are ratio for categories that did change between 2006 and 2011.  
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration.  


[palustrineMappedPixels.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/palustrineMappedPixels.savedIterations.csv) – Simulated 30 x 30 meter pixel counts for all Coastal Change Analysis Program palustrine categories, broken down by Monte Carlo iteration.  

Each column is representative of a 2006 to 2011 category represented by the following abbreviations separated by “_”. For example a change category of Estuarine Emergent Wetland to Estuarine Scrub/Shrub is represented by EEM_ESS.  

Abbreviations follow:  
HID – High intensity developed  
MID – Medium intensity developed  
LID – Low intensity developed  
OSD – developed open space  
CULT - Cultivated  
PAST – Pasture/Hay  
GRS - Grassland  
DEC – Deciduous forest  
EVR – Evergreen forest  
MIX – Mixed forest  
SS – Scrub/Shrub  
PFW – Palustrine forested wetland  
PSS – Palustrine scrub/shrub wetland  
PEM – Palustrine emergent marsh  
EFW – Estuarine forested wetland  
ESS – Estuarine scrub/shrub  
EEM – Estuarine emergent wetland  
UCS – Unconsolidated shore  
BAR – Bare land  
OW – Open water  
PAB – Palustrine aquatic bed  
EAB – Estuarine aquatic bed  
SNOW - Snow  

Final attribute:  
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration


[storageAndEmissions.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/storageAndEmissions.savedIterations.csv) – Emissions factors and emissions factor components, broken down by Monte Carlo iteration.  
soil.burial – Soil burial rate in grams CO<sub>2</sub> per square meter per year.  
soil.carbon.density – Soil carbon density in grams CO<sub>2</sub> per square meter.  
emergent.biomass – Emergent biomass in grams CO<sub>2</sub> per square meter.  
scrub.shrub.biomass – Scrub/shrub biomass in grams CO<sub>2</sub> per square meter.  
forested.biomass – Forested biomass in grams CO<sub>2</sub> per square meter.  
estuarine.methane – Estuarine methane emissions in grams CO<sub>2</sub> equivalent per square meter per year, estimated using global warming potential.  
palustrine.methane - Palustrine methane emissions in grams CO<sub>2</sub> equivalent per square meter per year, estimated using global warming potential.  
estuarine.methane.sgwp - Estuarine methane emissions in grams CO<sub>2</sub> equivalent per square meter per year, estimated using sustained global warming/cooling potential.  
palustrine.methane.sgwp - Palustrine methane emissions in grams CO<sub>2</sub> equivalent per square meter per year, estimated using sustained global warming/cooling potential.  
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration.  



[sector_and_total_mapping_outputs.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/sector_and_total_mapping_outputs.csv) – Summary statistics for mapped area, estimated area, and both total and sector wide per area emissions and removals for each Coastal Change Analysis Program 2006 to 2011 land cover change category.  
Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class abbreviation  
class – 2006 to 2011 C-CAP change class  
abbrev	 - 2006 to 2011 C-CAP change class abbreviation  
mapped_med	- Median mapped pixel count  
mapped_min - 2.5% quantile mapped pixel count  
mapped_max	- 97.5% quantile mapped pixel count  
mapped_ci – Difference between 95.5% and 2.5% quantiles for mapped pixel count  
estima_med – Median estimated pixel count  
estima_min - 2.5% quantile estimated pixel count  
estima_max - 97.5% quantile estimated pixel count  
estima_ci - Difference between 95.5% and 2.5% quantiles for estimated pixel count  
total_med – Total median tonnes CO<sub>2</sub>equivalent per year per mapped pixel  
total_min - 2.5% quantile for total CO<sub>2</sub>equivalent per year per mapped pixel  
total_max - 97.5% quantile for total CO<sub>2</sub>equivalent per year per mapped pixel  
total_ci	 - Difference between 95.5% and 2.5% quantiles for total CO<sub>2</sub>equivalent per year per mapped pixel  
soil_med – Median soil flux in CO<sub>2</sub> per year per mapped pixel  
soil_min - 2.5% quantile for soil flux in CO<sub>2</sub> per year per mapped pixel  
soil_max - 97.5% quantile for soil flux in CO<sub>2</sub> per year per mapped pixel  
soil_ci	- Difference between 95.5% and 2.5% quantiles for soil flux in CO<sub>2</sub> per year per mapped pixel  
bmass_med – Median biomass flux in CO<sub>2</sub> per year per mapped pixel  
bmass_min - 2.5% quantile for biomass flux in CO<sub>2</sub> per year per mapped pixel  
bmass_max - 97.5% quantile for biomass flux in CO<sub>2</sub> per year per mapped pixel  
bmass_ci - Difference between 95.5% and 2.5% quantiles for biomass flux in CO<sub>2</sub> per year per mapped pixel  
ch4_med – Median methane flux in CO<sub>2</sub> equivalent per year per mapped pixel  
ch4_min - 2.5% quantile for methane flux CO<sub>2</sub> equivalent per year per mapped pixel  
ch4_max - 97.5% quantile for methane flux in CO<sub>2</sub> equivalent per year per mapped pixel  
ch4_ci - Difference between 95.5% and 2.5% quantiles for methane flux in CO<sub>2</sub> equivalent per year per mapped pixel  


[total.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/regional/total.savedIterations.csv) – Stored results of Monte Carlo simulations for each Coastal Change Analysis Program 2006 to 2011 land cover change category considered in the analysis.  
Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class abbreviation  
class – 2006 to 2011 C-CAP change class  
abbrev	 - 2006 to 2011 C-CAP change class abbreviation  
variableType – Whether mapped area is treated as a fixed or random variable  
mappedPixelCount – Mapped 30 x 30 meter pixel count  
estimated_pixel_count – Estimated 30 x 30 meter pixel count  
total_gCO2perM2 – Total greenhouse gas flux in grams of CO<sub>2</sub> equivalent per meter square  
soil_gCO2perM2 - Soil greenhouse gas flux in grams of CO<sub>2</sub> per meter square	 
biomass_gCO2perM2 - Biomass greenhouse gas flux in grams of CO<sub>2</sub> per meter square  
methane_gCO2perM2 - Methane greenhouse gas flux in grams of CO<sub>2</sub> equivalent per meter square  
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration  
