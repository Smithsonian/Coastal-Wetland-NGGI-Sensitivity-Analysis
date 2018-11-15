# MonteCarloResults

This folder contains the working files and results of two different Monte Carlo analyses that were run for this paper.  

The [national](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/outputTables/MonteCarloResults/national) subfolder contains results of a version of the Monte Carlo analysis which unscaled lognormally distributed variables using means rather than exponentiated log-means. This approach causes these variables to skew positive, but an argument can be made that it is more representative of at the national scale. These results were produced for the sake of comparison and are discussed only in the text’s supplemental information.

The [regional](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/outputTables/MonteCarloResults/regional) subfolder contains results of a version of the Monte Carlo analysis which unscaled lognormally distributed variables using exponentiated log-means. This approach avoids the long-tail positive skew and approximates a median value. An argument can be made that it is more representative of at the regional scale estimates.  


[salinity_stability_summed_iterations_bothLmeanAndMean.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/salinity_stability_summed_iterations_bothLmeanAndMean.csv) – Sum total fluxes in million tonnes of CO<sub>2</sub> equivalents per year broken down by salinity, stability, by Monte Carlo iteration number, and by method used for scaling up lognormally distributed emissions factors.    

attributes include:  
salinity - Salinity class as defined by Coastal Change Analysis Program. Either Estuarine or Palustrine.  
stability – _Stable and Gains_ include wetlands that do not change and other landcover classes that convert to wetlands. _Losses_ include wetlands that convert to other landcover classes.	 
analysis_description – Code indicating combined salinity and stability classification.  
iterationCode – Integer between 1 and 10,000 indicating the Monte Carlo iteration.  
scaled_up_using - Indicates whether lognormally distributed variables were scaled up using a _mean_ or exponentiated _logmean_.  
sum_total_MillionTonnesCO2PerYear - Sum total greenhouse gas flux in million tonnes of CO<sub>2</sub> equivalents per year.  
emission_or_storage – Indicates whether the simulation represents a net-emissions or net-storage scenario.  


[salinity_stability_summed_summaries_bothLmeanAndMean.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/salinity_stability_summed_summaries_bothLmeanAndMean.csv) – Summary statistics of all Monte Carlo uncertainty assessment iterations broken down by salinity, stability, and by method used for scaling up lognormally distributed emissions factors.  

attributes include:  
analysis_description - Code indicating combined salinity and stability classification.  
salinity	- Salinity class as defined by Coastal Change Analysis Program. Either Estuarine or Palustrine.  
stability - Stable and Gains_ include wetlands that do not change and other landcover classes that convert to wetlands. _Losses_ include wetlands that convert to other landcover classes.	 
scaled_up_using - Indicates whether lognormally distributed variables were scaled up using a _mean_ or exponentiated _logmean_.  
median_sum_total_MillionTonnesCO2PerYear – Median of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
lower_ci_sum_total_MillionTonnesCO2PerYear – Lower 95% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
upper_ci_sum_total_MillionTonnesCO2PerYear – Upper 95% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
lower_99_sum_total_MillionTonnesCO2PerYear - Lower 99% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
upper_99_sum_total_MillionTonnesCO2PerYear - Upper 99% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
min_sum_total_MillionTonnesCO2PerYear – Minimum of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
max_sum_total_MillionTonnesCO2PerYear – Maximum of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  


[total_nggi_summed_iterations_bothLmeanAndMean.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/total_nggi_summed_iterations_bothLmeanAndMean.csv) - Sum total fluxes in million tonnes of CO<sub>2</sub> equivalents per year broken down by Monte Carlo iteration number, and by method used for scaling up lognormally distributed emissions factors.  

Attributes include:  
analysis_description - Code indicating combined that this is a total national greenhouse gas inventory calculation
iterationCode – Integer between 1 and 10,000 indicating the Monte Carlo iteration.  
scaled_up_using - Indicates whether lognormally distributed variables were scaled up using a _mean_ or exponentiated _logmean_.  
sum_total_MillionTonnesCO2PerYear - Sum total greenhouse gas flux in million tonnes of CO<sub>2</sub> equivalents per year.  
emission_or_storage – Indicates whether the simulation represents a net-emissions or net-storage scenario.  


[total_nggi_summed_summaries_bothLmeanAndMean.csv](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/data/outputTables/MonteCarloResults/total_nggi_summed_summaries_bothLmeanAndMean.csv) - Summary statistics of all Monte Carlo uncertainty assessment iterations broken down by method used for scaling up lognormally distributed emissions factors.   

Attributes include:  
analysis_description - Code indicating combined that this is a total national greenhouse gas inventory calculation  
scaled_up_using - Indicates whether lognormally distributed variables were scaled up using a _mean_ or exponentiated _logmean_.  
median_sum_total_MillionTonnesCO2PerYear – Median of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
lower_ci_sum_total_MillionTonnesCO2PerYear – Lower 95% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
upper_ci_sum_total_MillionTonnesCO2PerYear – Upper 95% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
lower_99_sum_total_MillionTonnesCO2PerYear - Lower 99% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
upper_99_sum_total_MillionTonnesCO2PerYear - Upper 99% confidence interval of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
min_sum_total_MillionTonnesCO2PerYear – Minimum of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
max_sum_total_MillionTonnesCO2PerYear – Maximum of all sum total greenhouse gas flux simulations in million tonnes of CO<sub>2</sub> equivalents per year.  
