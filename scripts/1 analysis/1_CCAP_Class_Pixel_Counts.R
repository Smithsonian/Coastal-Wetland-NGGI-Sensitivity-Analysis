library(foreign) # to read the .dbf file housing the raster summary table

abbrevs <- c('HID', 'MID', 'LID', 'OSD', 'CULT', 'PAST', 'GRS', 'DEC', 'EVR', 'MIX', 'SS', 'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM', 'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
               'Cultivated', 'Pasture/Hay', 
               'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
               'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
               'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
               'Unconsolidated Shore', 'Bare Land', 'Water', 
               'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed', 
               'Snow/Ice')

# load total ccap data
ccap <- read.dbf(paste(getwd(), "/data/AllStates2006to2010wGreatLakes_170720B.dbf", sep=""))

pixels <-c() # empty storage vector
for (i in 1:length(classOrder)) { # for each unique class
  sub_df <- subset(ccap, X2006_Clas == classOrder[i]) # subset ccap for 2010 class
  n = sum(sub_df$Count) # summ the pixel counts in that class
  pixels <- c(pixels, n) # store pixel count
}

ccapPixelCounts <- data.frame(classes = classOrder, abbrevs = abbrevs, pixels = pixels)
ccapPixelCounts <- subset(ccapPixelCounts, pixels > 0)

write.table(ccapPixelCounts, paste(getwd(), "/data/ccapPixelCounts.csv", sep=""), sep=",", row.names=F)

change_df <- subset(ccap, as.character(X2006_Clas) != as.character(X2010_Clas)) # subset all changes
change_pixels <- sum(change_df$Count)

noChange_df <- subset(ccap, as.character(X2006_Clas) == as.character(X2010_Clas)) # subset all no changes
noChange_pixels <- sum(noChange_df$Count)

cncPixelCounts <- data.frame(classes = c("No.Change", "Change"), pixels = c(noChange_pixels, change_pixels))
write.table(cncPixelCounts, paste(getwd(), "/data/cncPixelCounts.csv", sep=""), sep=",", row.names=F)