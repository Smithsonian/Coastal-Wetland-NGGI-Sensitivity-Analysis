# Combined Plots for Emissions and Storage Factors
# prep methane daa
{
  methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
  head(methane)
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
  #methane <- subset(methane, CH4.flux > 0)

  # using Scott Neubauer's CO2 Eq. for SGWP and SGCP
  generateMethaneSGPs <- function(ch4) {
    if (ch4 < 0) { return(ch4 * 203)
    } else { return(ch4 * 45)}
  }
  
  ch4_co2_eq <- mapply(generateMethaneSGPs, methane$CH4.flux)
  
  #logTransform <- function(y, defined.min=min(y)) { return(log(1+y-defined.min)) }
  
  #revLogTransform <- function(log.y, defined.min) { return(exp(log.y) + defined.min - 1) }
  
  #ch4_co2_eq.min <- min(ch4_co2_eq)
  methane["ch4_co2_eq"] <- ch4_co2_eq
  est <- subset(methane, Salinity.ppt >= 5)
  est_mean <- mean(est$ch4_co2_eq)
  est_sd <- sd(est$ch4_co2_eq)
  
  pal <- subset(methane, Salinity.ppt <5)
  log.ch4_co2_eq <- log(pal$ch4_co2_eq)
  pal["log.ch4_co2_eq"] <- log.ch4_co2_eq
  
  pal_lmean <- mean(pal$log.ch4_co2_eq)
  pal_lsd <- sd(pal$log.ch4_co2_eq)
  
  (exp(pal_lmean))
  (exp(pal_lmean+pal_lsd))
  (exp(pal_lmean-pal_lsd))
}

# K Byrd Biomass and Mangrove Synthesis Together
# load biomass data
{
  
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
  carbonPerBiomass <- 0.441
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

# CAR Data
{
  car <- read.csv(paste(getwd(), "/data/MengReview/PbandcsData_170926.csv", sep=""))
  pb <- car$delSOC1[! is.na(car$delSOC1)]
  carbonToCO2 <- 3.666667
  pb <- pb * carbonToCO2
}

dev.off()
pdf("figs/Emissions Factors Probability Distribuions.pdf", height=5, width=7)

m <- matrix(c(1,2,3,3,4,5,6,6), byrow = T, nrow=2)
layout(m)
par(oma=c(2,3,0,0), mar=c(2,1,3,1), family="ArialMT")
#layout.show(n=6)

total.range.1.a <- range(pb, est$ch4_co2_eq)
total.range.1.b <- range(total.range.1.a, pal$ch4_co2_eq)

target_x.1 <- seq(1, max(c(pb)), by=5)
target_pb <- dlnorm(target_x.1, mean(log(pb)), sd(log(pb)))
pb.n <- length(pb)

pb.density <- hist(pb,plot=F)$density
hist(pb, col="grey", xlab="", probability = T, main="", xlim=range(total.range.1.a), ylim=range(0, max(pb.density, target_pb)))
mtext("Carbon Burial Rate", side=3, line=1.75, font=2)
mtext(expression(paste({}^bold("210")*bold("Pb (n=109)"))), side=3, line=0, font=2)
points(target_x.1, target_pb, col="black", type="l", lwd=2)

y_target <- seq(min(est$ch4_co2_eq), max(est$ch4_co2_eq), by=1)
target_em <- dnorm(y_target, est_mean, est_sd)
est.density <- hist(est$ch4_co2_eq, plot=F)$density
hist(est$ch4_co2_eq, probability = T, col="grey", xlab="", main="", xlim=range(total.range.1.a), ylim=range(0, max(est.density, target_em)))
mtext(expression(paste(bold("Estuarine CH")[bold("4")])), side=3, line=1.25, font=2)
mtext("(n=31)", side=3, line=0.25, font=2)
points(y_target, target_em, type="l", col="black", lwd=2)

y_target <- seq(0, max(pal$ch4_co2_eq), by=1)
log.y <- log(y_target)
target_pal <- dlnorm(y_target, pal_lmean, pal_lsd)
pal.density <- hist(pal$ch4_co2_eq, plot=F)$density
hist(pal$ch4_co2_eq, probability = T, col="grey", xlab="", main="", ylim=range(0, pal.density, target_pal))
points(y_target, target_pal, type="l", col="black", lwd=2)
mtext(expression(paste(bold("Palustrine CH")[bold("4")])), side=3, line=1.25)
mtext("(n=24)", side=3, line=0.25, font=2)

target_x <- seq(0.01, max(biomass.em.co2, biomass.ss.co2, mangrove.gCO2m2), by=5)
d.y.em <- dlnorm(target_x, biomass.em.log.mean, biomass.em.log.sd)
em.density <- hist(biomass.em.co2, plot=F)$density

hist(biomass.em.co2, col="grey", main="", ylim=c(0, max(d.y.em)), probability=T, xlim=c(0,8500))
points(target_x, d.y.em, col="black", type="l", lwd=2)
mtext("Emergent Biomass", side=3, line=1.5, font=2)
mtext(paste("(n=",toString(biomass.em.n), ")", sep=""), side=3, line=0, font=2)

d.y.ss <- dlnorm(target_x, biomass.ss.log.mean, biomass.ss.log.sd)
hist(biomass.ss.co2, col="grey", main="", probability = T,  xlim=c(0,8500))
mtext("Scrub/Shrub Biomass", side=3, line=1.5, font=2)
mtext(paste("(n=",toString(biomass.ss.n), ")", sep=""), side=3, line=0, font=2)

points(target_x, d.y.ss, col="black", type="l", lwd=2)

d.y.fo <- dlnorm(target_x, biomass.fo.log.mean, biomass.fo.log.sd)
hist(mangrove.gCO2m2, col="grey", main="", probability = T, ylim=c(0, max(d.y.fo)))
mtext("Forested Biomass", side=3, line=1.5, font=2)
mtext(paste("(n=",toString(biomass.fo.n), ")", sep=""), side=3, line=0, font=2)
points(target_x, d.y.fo, col="black", type="l", lwd=2)

mtext(expression(paste("Emissions or Storage Factors (g CO"["2"]," equivalents m"^"-2",")")), side=1, line=1, outer=T)
mtext("probability density", side=2, line = 1.5, outer=T)
dev.off()
