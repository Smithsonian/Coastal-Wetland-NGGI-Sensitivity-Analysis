# biomass

This folder contains ordered scripts for ingesting biomass datasets as they were received and reformatted for machine readability, reformats them to a common structure, and compiles them into a single file.  

[0_allometric_equations.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/0_allometric_equations.R) – contains allometric equations cited in the text and used to estimate above ground biomass from plot based plant measurements. For (Megonigal et al. 1997) we used allometric equations listed within. We note one correction to appendix 2; Fraxinus spp. DBH values need to be converted from centimeters to inches before input into the function, mass needs to be converted pounds to kilograms after output. For (Krauss et al. 2018) and (Craft et al. 2017) we utilized generic genus-specific allometric equations listed in (Jenkins et al. 2003). For mangroves we used to following allometric equations. For (Simard et al. 2006) and (Doughty et al. 2016) we used allometric equations listed therein. For (Twilley, Rivera-Monroy, and Castaneda 2018) we used allometric equations originating from (Smith and Whelan 2006).

Scripts numbered 1-7 correspond to the datasets cited in the [data/biomass](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/tree/master/data/Biomass) folder.  

[biomass_synthesis_dataset.R](https://github.com/Smithsonian/Coastal-Wetland-NGGI-Sensitivity-Analysis/blob/master/scripts/0%20formatting/biomass/biomass_synthesis_dataset.R) – Contains code for compiling data from scripts 1-7 into a single file.  

Craft, Christopher B., Mckenna Stahl, Sarah Widney, and Dontrece Smith. 2017. “Forest Survey of Species Richness and Basal Area for Two Tidal Forest Plots at GCE 11 on the Altamaha River in Southeast Georgia in December 2013.” Georgia Coastal Ecosystems LTER Project; University of Georgia; Long Term Ecological Research Network. https://doi.org/10.6073/pasta/95771dba9e839a57c74b9ae806569877.  

Doughty, Cheryl L., J. Adam Langley, Wayne S. Walker, Ilka C. Feller, Ronald Schaub, and Samantha K. Chapman. 2016. “Mangrove Range Expansion Rapidly Increases Coastal Wetland Carbon Storage.” Estuaries and Coasts 39 (2): 385–96.  

Jenkins, Jennifer C., David C. Chojnacky, Linda S. Heath, and Richard A. Birdsey. 2003. “National-Scale Biomass Estimators for United States Tree Species.” Forest Science 49 (1): 12–35.  

Krauss, K. W., G. B. Noe, J. A. Duberstein, W. H. Conner, M. C. Jones, C. E. Bernhardt, N. Cormier, and A. S. From. 2018. “Carbon Budget Assessment of Tidal Freshwater Forested Wetland and Oligohaline Marsh Ecosystems along the Waccamaw and Savannah Rivers, U.S.A. (2005-2016).” U.S. Geological Survey data release. https://doi.org/10.5066/F7TM7930.  

Megonigal, J. Patrick, William H. Conner, Steven Kroeger, and Rebecca R. Sharitz. 1997. “ABOVEGROUND PRODUCTION IN SOUTHEASTERN FLOODPLAIN FORESTS: A TEST OF THE SUBSIDY–STRESS HYPOTHESIS.” Ecology 78 (2): 370–84.  

Simard, Marc, Keqi Zhang, Victor H. Rivera-Monroy, Michael S. Ross, Pablo L. Ruiz, Edward Castañeda-Moya, Robert R. Twilley, and Ernesto Rodriguez. 2006. “Mapping Height and Biomass of Mangrove Forests in Everglades National Park with SRTM Elevation Data.” Photogrammetric Engineering & Remote Sensing 72 (3): 299–311.  

Smith, Thomas J., and Kevin R. T. Whelan. 2006. “Development of Allometric Relations for Three Mangrove Species in South Florida for Use in the Greater Everglades Ecosystem Restoration.” Wetlands Ecology and Management 14 (5): 409–19.  

Twilley, R., V. Rivera-Monroy, and E. Castaneda. 2018. “Mangrove Forest Growth from the Shark River Slough, Everglades National Park (FCE), South Florida from January 1995 to Present.” Environmental Data Initiative. https://doi.org/10.6073/pasta/0c8f485c7095dfed160e66b9b959f470.  
