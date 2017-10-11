# importing mangrove data from meng's synthesis
mengBiomass <- read.csv(paste(getwd(), "/data/MengReview/mangroveAndMarshesABG.csv", sep=""))

mangroveBiomass <- subset(mengBiomass , Ecosystem == "mangrove")
mangroveBiomass <- subset(mangroveBiomass, (AGBunits != 'gC_m2') & (AGBunits != 'MgC_ha'))

convertBiomassUnits <- function(input_measurement, input_unit, c_conversion=0.5) {
  gPerMg = 1000000
  m2PerHa = 10000
  gPerKg = 1000
  if (input_unit == "g_m2") { return(input_measurement) }
  else if (input_unit == "gC_m2") { return(input_measurement / c_conversion) }
  else if (input_unit == "MgC_ha") { return(input_measurement / c_conversion / m2PerHa * gPerMg) }
  else if (input_unit == "Mg_ha") { return(input_measurement / m2PerHa * gPerMg) }
  else if (input_unit == "kg_ha") { return(input_measurement / m2PerHa * gPerKg) }
  else {
    print('error. Cannot convert')
    return(NA)
    }
}

AGBgm2 <- mapply(convertBiomassUnits, mangroveBiomass$AGB, mangroveBiomass$AGBunits)

mangroveBiomass["AGMgPerM2"] <- AGBgm2

hist(log(mangroveBiomass$AGMgPerM2))
hist(mangroveBiomass$AGMgPerM2)

(logMeanMangrove <- mean(log(mangroveBiomass$AGMgPerM2)))
(logSdMangrove <- sd(log(mangroveBiomass$AGMgPerM2)))
(exp(logMeanMangrove+logSdMangrove))
(exp(logMeanMangrove-logSdMangrove))

meanStore <- c()
for (i in 1:10000) {
  genValues <- rlnorm(19, logMeanMangrove, logSdMangrove)
  meanStore <- c(meanStore,  mean(genValues))
}
