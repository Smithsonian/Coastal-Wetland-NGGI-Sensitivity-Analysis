README
================
James R Holmquist

Overview
========

Files and code used to generate analyses in the Environmental Research Letters paper *Uncertainty in United States coastal wetland greenhouse gas inventorying*.

#### Contact: <holmquistj@si.edu>

<br>

### Manuscript Abstract

Coastal wetlands store carbon dioxide (CO<sub>2</sub>) and emit CO<sub>2</sub> and methane (CH<sub>4</sub>) making them an important part of greenhouse gas (GHG) inventorying. In the contiguous United States (CONUS), a coastal wetland inventory was recently calculated by combining maps of wetland type and change with soil, biomass, and CH4 flux data from a literature review. We assess uncertainty in this developing carbon monitoring system to quantify confidence in the inventory process itself and to prioritize future research. We provide a value-added analysis by defining types and scales of uncertainty for assumptions, burial and emissions datasets, and wetland maps, simulating 10,000 iterations of a simplified version of the inventory, and performing a sensitivity analysis. Coastal wetlands were likely a source of net-CO<sub>2</sub>-equivalent (CO<sub>2</sub>e) emissions from 2006 to 2011. Although stable estuarine wetlands were likely a CO<sub>2</sub>e sink, this effect was counteracted by catastrophic soil losses in the Gulf Coast, and CH<sub>4</sub> emissions from tidal freshwater wetlands. The direction and magnitude of total CONUS CO<sub>2</sub> flux were most sensitive to uncertainty in emissions and burial data, and assumptions about how to calculate the inventory. Critical data uncertainties included CH4 emissions for stable freshwater wetlands and carbon burial rates for all coastal wetlands. Critical assumptions included the average depth of soil affected by erosion events, the method used to convert CH<sub>4</sub> fluxes to CO<sub>2</sub>e, and the fraction of carbon lost to the atmosphere following an erosion event. The inventory was relatively insensitive to mapping uncertainties. Future versions could be improved by collecting additional data, especially the depth affected by loss events, and by better mapping salinity and inundation gradients relevant to key GHG fluxes.

<br>

![](figs/main/1%20NGGI%20Mapping%20Slide.jpg) Fig. 1: The three mapping layers used in our coastal wetland greenhouse gas inventory viewed for San Francisco Bay. A. 2011 Coastal Change Analysis Program (C-CAP) Land Cover Classifications. B. 2006 to 2011 C-CAP Change Map (Basemap Â© ESRI, used with permission). C. A probabilistic coastal lands map, showing the probability elevation is below twice highest monthly tide level, mean higher high water for spring tides (MHHWS).

<br> <br>

![](figs/main/7%20Sensitivity%20Analysis%203%20by%206%20180808.jpg) Fig. 7: These fifteen inputs introduced the most uncertainty into the Coastal Wetland National Greenhouse Gas Inventory according to a one-at-a-time sensitivity analysis. GWP: Global Warming Potential, SGWP: Sustained GWP, SGCP: Sustained Global Cooling Potential, NWI: National Wetlands Inventory, EAB: Estuarine Aquatic Bed, OW: Open Water, UCS: Unconsolidated Shore, PAB: Palustrine Aquatic Bed.

<br>

Citation Information
--------------------

Please cite the code and derrivative analytics gathered here as:

Holmquist, J.R., Windham-Myers, L., Bernal, B., Byrd, K. B., Crooks, S., Gonneea, M. E., Herold, N., Knox, S. H., Kroeger, K., McCombs, J., Megonigal, J. P., Meng, L., Morris, J. T., Sutton-Grier, A. E., Troxler, T. G., Weller, D. E. (2018). Uncertainty in United States coastal wetland greenhouse gas inventorying. Environmental Research Letters. <https://doi.org/10.1088/1748-9326/aae157>.

If reusing the data synthesized for these analyses please take care to cite the original works. These sources are listed in the orinal paper and its supplemental information, as well as in accompanying README files associated with particular datasets.

Repository Structure
--------------------

This repository is broken up into the main body and several subfolders. The main body contains this README file, an R Project, which will allow users to re-run R scripts while accessing the repository's directory as the working directory, a .gitattributes and a .gitignore file, which provide .git functionality to the repository, and self-descriptive subfolders containing data, figures, and scripts.

data - contains all of the data files used and created in this analysis. These are organized by sector (Biomass, MengReview \[used for Carbon Burial Rates\], Methane, WetlandArea) as well as output tables, which contain derrivative results from the monte carlo and sensitivity analyses. Each data source's origin or citation is listed in separate readme files, and we use original and derivative subfolders to separate files as they were recieved or downloaded from those formatted and machine readable for analysis.

figs - contains figures that appear in the paper. They are separated into main and si (supplemental information). The figures are numbered as they appear in the texts.

scripts - contains the scripts used for formatting, monte carlo and sensitivity analyses, and creating figures. They are broken down as such into ordered subfolders (0 formatting, 1 analysis, 2 figures). Each subfolder contains ordered R scripts that access and output to the data folders.
