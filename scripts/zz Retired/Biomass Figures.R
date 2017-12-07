# K Byrd Biomass and Mangrove Synthesis Together


# load files
{

  # define Carbon Accumulation Rate Assumptions
  car <- read.csv(paste(getwd(), "/data/MengReview/PbandcsData_170926.csv", sep=""))
  
  kristinBiomass <- read.csv(paste(getwd(), "/data/Biomass/BiomassSamples.csv", sep=""))
  kristinBiomass <- subset(kristinBiomass, biomass_gm > 0)
  
  mengBiomass <- read.csv(paste(getwd(), "/data/MengReview/mangroveAndMarshesABG.csv", sep=""))
  mangroveBiomass <- subset(mengBiomass , Ecosystem == "mangrove")

}

# Define Important Conversion Factors
{
  gramsPerKg <- 1000 #1000 grams per kg
  m2PerHa <- 10000
  millionHaPerHa <- 1E6
  carbonPerBiomass <- 0.42
  gramsPerPetagram <-1E15
  m2perPixel <- 900
  carbonToCO2 <- 3.666667
}

# Prep Biomass data and convert to gCO2 eq per m2
{
  # Biomass Values from Kristen's Byrd's Remote Sensing Calibraiton dataset
  iva <- subset(kristinBiomass, (sp1 == "Iva frutescens") & (pc_sp1 >= 0.50))
  em <- subset(kristinBiomass, (! ( (sp1 == "Iva frutescens") & (pc_sp1 >= 0.50))))
  
  # convert to gCO2
  biomass.em.co2 <- em$biomass_gm * carbonPerBiomass * carbonToCO2
  biomass.ss.co2 <- iva$biomass_gm * carbonPerBiomass * carbonToCO2
  
  biomass.em.log.mean <- mean(log(biomass.em.co2))
  biomass.em.log.sd <- sd(log(biomass.em.co2))
  biomass.em.n <- nrow(em)
  
  biomass.ss.log.mean <- mean(log(biomass.ss.co2))
  biomass.ss.log.sd <- sd(log(biomass.ss.co2))
  biomass.ss.n <- nrow(iva)
  
  # Mangrove biomass data from Meng's synthesis
  mangroveBiomass <- subset(mengBiomass , Ecosystem == "mangrove")
  
  convertBiomassUnits <- function(input_measurement, input_unit, c_conversion=0.42) {
    gPerMg = 1000000
    m2PerHa = 10000
    gPerKg = 1000
    if (input_unit == "g_m2") { return(input_measuremen * c_conversion) }
    else if (input_unit == "gC_m2") { return(input_measurement) }
    else if (input_unit == "MgC_ha") { return(input_measurement / m2PerHa * gPerMg) }
    else if (input_unit == "Mg_ha") { return(input_measurement * c_conversion / m2PerHa * gPerMg) }
    else if (input_unit == "kg_ha") { return(input_measurement * c_conversion / m2PerHa * gPerKg) }
    else {
      print('error. Cannot convert')
      return(NA)
    }
  }
  
  mangrove.gCm2 <- mapply(convertBiomassUnits, mangroveBiomass$AGB, mangroveBiomass$AGBunits)
  mangrove.gCO2m2 <- mangrove.gCm2 * carbonToCO2
  
  biomass.fo.log.mean <- mean(log(mangrove.gCO2m2))
  biomass.fo.log.sd <- sd(log(mangrove.gCO2m2))
  biomass.fo.n <- length(mangrove.gCO2m2)
}

# set up plot to have three columns


par(oma=c(3,3,0,0), mar=c(1.5,1.5,1.5,1.5))

m <- matrix(c(1,2,3), nrow=1)
layout(mat=m, widths = c(1,1, 3))
layout.show(n=3)


target_x <- seq(0.01, max(biomass.em.co2, biomass.ss.co2, mangrove.gCO2m2), by=5)
d.y.em <- dlnorm(target_x, biomass.em.log.mean, biomass.em.log.sd)
#em.density <- hist(biomass.em.co2, plot=F)$density

hist(biomass.em.co2, col="grey", main=paste("Emergent (n=",biomass.em.n, ")", sep=""), ylim=c(0, max(d.y.em)), probability=T, xlim=c(0,8500))
points(target_x, d.y.em, col="green", type="l", lwd=2)

d.y.ss <- dlnorm(target_x, biomass.ss.log.mean, biomass.ss.log.sd)
hist(biomass.ss.co2, col="grey", main=paste("Scrub/Shrub (n=",biomass.ss.n, ")", sep=""), probability = T,  xlim=c(0,8500))
points(target_x, d.y.ss, col="forestgreen", type="l", lwd=2)

d.y.fo <- dlnorm(target_x, biomass.fo.log.mean, biomass.fo.log.sd)

hist(mangrove.gCO2m2, col="grey", main=paste("Forested (n=",biomass.fo.n, ")", sep=""), probability = T, ylim=c(0, max(d.y.fo)))
points(target_x, d.y.fo, col="brown", type="l", lwd=2)

mtext(expression(paste("Plant Biomass (gCO"[2], " m"^-2, ")")), side=1, line=1.5, outer=T)
mtext("Density", side=2, line=1.5, outer=T)
