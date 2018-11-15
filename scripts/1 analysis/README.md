# 1 analysis

This file contains scripts for the main Monte Carlo, Sensitivity Analysis, and creating summary statistics from those analyses.

[1_CCAP_Class_Pixel_Counts.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/1%20analysis/1_CCAP_Class_Pixel_Counts.R) – This script takes a full 2006 to 2010/2011 Coastal Change Analysis Program raster .dbf  files as an input and outputs tables of pixel counts for 2010/2011 classifications and 2006 to 2010/2011 change or no change.  

[2_Simulating_NGGI.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/1%20analysis/2_Simulating_NGGI.R) – This script contains code for importing multiple datasources, formatting them into common units, and defining them as probability distributions. It then runs two different versions of a Monte Carlo Uncertainty Assessment, and the Sensitivity Analysis, and outputs multiple tables.  

[3_Crunch_Data_from_Monte_Carlo_Operations.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/1%20analysis/3_Crunch_Data_from_Monte_Carlo_Operations.R) – This script takes the results of the Monte Carlo and sensitivity analyses and simplifies them so that they are ready for reporting and graphing.  

[4_Random_Draws_of_Palustrine_Estuarine_Emissions.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/1%20analysis/4_Random_Draws_of_Palustrine_Estuarine_Emissions.R) – This script compares both quantification of methane emissions using exponentiated log means and means, and calculating confidence intervals using Monte Carlo Analysis, and the Cox Method. This analysis is presented in the text’s supplemental information.  

