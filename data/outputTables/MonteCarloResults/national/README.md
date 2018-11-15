# national

This folder contains working files and results of a version of the Monte Carlo analysis which unscaled lognormally distributed variables using means rather than exponentiated log-means.

[ccap2010perPixelScalers.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/national/ccap2010perPixelScalers.savedIterations.csv) – Estimated to mapped are ratio for each Coastal Change Analysis Program 2010/2011 class broken down by Monte Carlo Iteration.  
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
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration.  


[cncPerPixelScalers.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/national/cncPerPixelScalers.savedIterations.csv) - Estimated to mapped are ratio for Coastal Change Analysis Program 2006 to 2011 change detection broken down by Monte Carlo Iteration.  

No.Change – Estimated to mapped are ratio for categories that did not change between 2006 and 2011.  
Change – Estimated to mapped are ratio for categories that did change between 2006 and 2011.  
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration.  


[palustrineMappedPixels.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/national/palustrineMappedPixels.savedIterations.csv) – Simulated 30 x 30 meter pixel counts for all Coastal Change Analysis Program palustrine categories, broken down by Monte Carlo iteration.  

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
iterationCode - Integer between 1 and 10,000 indicating the Monte Carlo iteration.  


[storageAndEmissions.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/national/storageAndEmissions.savedIterations.csv) – Emissions factors and emissions factor components, broken down by Monte Carlo iteration.  

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


[total.savedIterations.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/national/total.savedIterations.csv) – Stored results of Monte Carlo simulations for each Coastal Change Analysis Program 2006 to 2011 land cover change category considered in the analysis.  

Attributes include:  
class_2006 – 2006 C-CAP Class  
abbrev_2006 - 2006 C-CAP Class abbreviation  
class_2010 – 2010/2011 C-CAP Class  
abbrev_2010 - 2010/2011 C-CAP Class Abbreviation  
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
