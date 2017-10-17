# this will simulate the NGGI Based on Available Data
# load necessary packages 
{
  library(foreign) # to read .dbf files
  library(MASS) # to create multivariate normal distri 
}

# load files
{
  ccap <- read.dbf(paste(getwd(), "/data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf", sep=""))
  
  ccap_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/CCAP2010AccuracyAssessment.csv", sep=""), row.names = 1))
  ccap_area <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/ccapPixelCounts.csv", sep=""))
  ccap_area <- ccap_area$pixels
  
  cnc_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/CCAP06to10ChangeNoChangeAccuracyAssesment.csv", sep=""), row.names = 1))
  cnc_area <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/cncPixelCounts.csv", sep=""))
  cnc_area <- cnc_area$pixels
  
  palustrineFilePath <- paste(getwd(), "/data/WetlandArea/Palustrine/PalustrinePixelCounts/_AllCONUS/tables", sep="")
  
  # Soils table
  soils <- read.table(paste(getwd(), "/data/HolmquistSoilSynthesis/Cal_Val_Points_171016.txt", sep=""), header=T)
  
  # create a table with all C mass values
  cmMatrix <- as.matrix(data.frame(CM000_010 = soils$vCM000_010, 
                                   CM010_020 = soils$vCM010_020,
                                   CM020_030 = soils$vCM020_030,
                                   CM030_040 = soils$vCM030_040,
                                   CM040_050 = soils$vCM040_050,
                                   CM050_060 = soils$vCM050_060,
                                   CM060_070 = soils$vCM060_070,
                                   CM070_080 = soils$vCM070_080,
                                   CM080_090 = soils$vCM080_090,
                                   CM090_100 = soils$vCM090_100,
                                   CM100_110 = soils$vCM100_110, 
                                   CM110_120 = soils$vCM110_120,
                                   CM120_130 = soils$vCM120_130,
                                   CM130_140 = soils$vCM130_140,
                                   CM140_150 = soils$vCM140_150,
                                   CM150_160 = soils$vCM150_160,
                                   CM160_170 = soils$vCM160_170,
                                   CM170_180 = soils$vCM170_180,
                                   CM180_190 = soils$vCM180_190,
                                   CM190_200 = soils$vCM190_200))
  
  
  biomass <- read.csv(paste(getwd(), "/data/Biomass/BiomassSamples.csv", sep=""))
  biomass <- subset(biomass, biomass_gm > 0)
  
  mengBiomass <- read.csv(paste(getwd(), "/data/MengReview/mangroveAndMarshesABG.csv", sep=""))
  mangroveBiomass <- subset(mengBiomass , Ecosystem == "mangrove")
  
  methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
  
}

# define assumptions re: land cover classes
{
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Tundra',
                 'Snow/Ice')
  
  palustrineWetlands <- c('Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland')
  
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
                      'Cultivated', 'Pasture/Hay',
                      'Unconsolidated Shore', 'Water',
                      'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')
  
  forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
                 'Palustrine Forested Wetland',
                 'Estuarine Forested Wetland'
                 )
  
  ssVeg <- c('Scrub/Shrub',
             'Palustrine Scrub/Shrub Wetland',
             'Estuarine Scrub/Shrub Wetland'
             )
  
  emVeg <- c('Cultivated', 'Pasture/Hay', 
             'Grassland',
             'Palustrine Emergent Wetland',
             'Estuarine Emergent Wetland'
             )
  
  nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
             'Unconsolidated Shore', 'Bare Land', 'Water', 
             'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
             'Tundra',
             'Snow/Ice'
             )
}

# define functions for prepparing the data
{
  
}

# define functions for generating areas and average C values 
{
  
}

iterations = 1

# generate classes to anayse
class2006 <- c()
class2010 <- c()
abbrevsPalAnalysis <- c()

for (j in 1:length(classOrder)) {
  for (k in 1:length(classOrder)) {
    if ( (classOrder[j] %in% c(estuarineWetlands, palustrineWetlands)) | (classOrder[k] %in% c(estuarineWetlands, palustrineWetlands))) {
      class2006 <- c(class2006, classOrder[j])
      class2010 <- c(class2010, classOrder[k])
      if ((classOrder[j] %in% palustrineWetlands) | (classOrder[k] %in% palustrineWetlands)) {
        abbrevsPalAnalysis <- c(abbrevsPalAnalysis, paste(abbrevs[j], "_", abbrevs[k], sep=""))
      } else {
        abbrevsPalAnalysis <- c(abbrevsPalAnalysis, NA)
      }
    }
  }
}
ccapAnalysis <- data.frame(class2006, class2010, abbrevsPalAnalysis)
