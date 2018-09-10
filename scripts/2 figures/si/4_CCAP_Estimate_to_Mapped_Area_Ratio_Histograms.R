# needed libraries
{ 
  library(foreign)
  library(tidyverse)
}

# load needed data files
{
  ccapCol <- read_csv("data/WetlandArea/CCAP/ccapCols.csv")
  ccap2010 <- read.dbf("data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf")
  palustrineMappedPixels <- read.csv("data/outputTables/MonteCarloResults/regional/palustrineMappedPixels.savedIterations.csv")
  # input table from raster dataset of palustrine under NWI assumption
  palustrineNwi <- read.dbf("data/WetlandArea/Palustrine/nwi/CCAP2006to2010_wTab_PalMaskedByNwi.dbf", as.is = T)
  ccap2010perPixelScalers <- read_csv("data/outputTables/MonteCarloResults/regional/ccap2010perPixelScalers.savedIterations.csv")
  cncPerPixelScalers <- read_csv("data/outputTables/MonteCarloResults/regional/cncPerPixelScalers.savedIterations.csv")
}

# set up assumptions
{
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Snow/Ice')
  
  palustrineWetlands <- c('Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland')
  
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  abbrevs.2 <- c('HID', 'MID', 'LID', 'OSD',
                 'CULT', 'PAST',
                 'GRS', 'DEC', 'EVR', 'MIX', 'SS',
                 'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
                 'UCS', 'BARE', 'OW', 'PAB', 'EAB', 'SNOW')
}


# Histograms for key wetland classes estimated:mapped ratio
# EEM ESS EFO PEM 
# PFO PSS OW UCS
# EAB EAB # Change # No.Change 

m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11), byrow = T, nrow=3)
layout(mat = m)
par(mar=c(1.25,1,2.5,1), oma=c(3,2.75,0,0), family="ArialMT")
#layout.show(n=11)

x.range.hists <- range(ccap2010perPixelScalers$EEM, ccap2010perPixelScalers$ESS, ccap2010perPixelScalers$EFW,
                       ccap2010perPixelScalers$PEM, ccap2010perPixelScalers$PSS, ccap2010perPixelScalers$PFW,
                       ccap2010perPixelScalers$OW, cncPerPixelScalers$No.Change, cncPerPixelScalers$Change)
target.breaks <- seq(x.range.hists[1]-0.01, x.range.hists[2]+0.01, by=0.025)

ccapCol.sub <- subset(ccapCol, class == 'EEM')
hist(ccap2010perPixelScalers$EEM, main="Estuarine Emergent", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'ESS')
hist(ccap2010perPixelScalers$ESS, main="Estuarine Scrub/Shrub", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'EFW')
hist(ccap2010perPixelScalers$EFW, main="Estuarine Forested", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PEM')
hist(ccap2010perPixelScalers$PEM, main="Palustrine Emergent", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PSS')
hist(ccap2010perPixelScalers$PEM, main="Palustrine Scrub/Shrub", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PFW')
hist(ccap2010perPixelScalers$PFW, main="Palustrine Forested", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'OW')
hist(ccap2010perPixelScalers$OW, main="Open Water", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'UCS')
hist(ccap2010perPixelScalers$UCS, main="Unconsolidated Shore", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'EAB')
hist(ccap2010perPixelScalers$EAB, main="Estuarine Aquatic Bed", xlim=range(ccap2010perPixelScalers$EAB), col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = 75)

hist(cncPerPixelScalers$No.Change, main="No Change", xlim=range(x.range.hists), col="black", breaks = target.breaks)
hist(cncPerPixelScalers$Change, main="Change", xlim=range(x.range.hists), col="red", breaks = target.breaks)
mtext("Estimated to Mapped Area Ratio", side=1, line=1.5, outer = T)
mtext("frequency", side=2, line=1.25, outer = T)

print(quantile(cncPerPixelScalers$Change, c(0.025,0.5,0.975)))
