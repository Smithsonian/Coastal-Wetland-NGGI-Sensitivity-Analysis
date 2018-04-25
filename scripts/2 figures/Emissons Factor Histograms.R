# Combined Plots for Emissions and Storage Factors
library(tidyverse)

# prep methane daa
{
  methane <- read_csv(paste(getwd(), "/data/Methane/derivative/Methane Synthesis Knox.csv", sep=""))
  head(methane)
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
  #methane <- subset(methane, CH4.flux > 0)

  # using Scott Neubauer's CO2 Equivalents for SGWP and SGCP at 100 years
  generateMethaneSGPs <- function(ch4) {
    if (ch4 < 0) { return(ch4 * 203)
    } else { return(ch4 * 45)}
  }
  
  generateMethaneGPs <- function(ch4) { return(ch4 * 25) }
  
  # Units are in gCH4 per m2 per year
  ch4_co2_eq_sgwp <- mapply(generateMethaneSGPs, methane$CH4.flux) # Calculate CO2 equivalents
  methane["ch4_co2_eq_sgwp"] <- ch4_co2_eq_sgwp # add to the dataframe so they can be sorted
  
  ch4_co2_eq_gwp <- mapply(generateMethaneGPs, methane$CH4.flux) # Calculate CO2 equivalents
  methane["ch4_co2_eq_gwp"] <- ch4_co2_eq_gwp # add to the dataframe so they can be sorted
  
  # Estuarine Emissions Factors are Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  est.ch4 <- subset(methane, Salinity.ppt >= 5)
  estuarine.methane.n <- length(est.ch4$ch4_co2_eq_sgwp)
  estuarine.methane.mean.sgwp <- mean(est.ch4$ch4_co2_eq_sgwp)
  estuarine.methane.sd.sgwp <- sd(est.ch4$ch4_co2_eq_sgwp)
  
  estuarine.methane.mean.gwp <- mean(est.ch4$ch4_co2_eq_gwp)
  estuarine.methane.sd.gwp <- sd(est.ch4$ch4_co2_eq_gwp)
  
  # Palustrine Emissions Factors are Log Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  pal.ch4 <- subset(methane, Salinity.ppt <5)
  palustrine.methane.n <- length(pal.ch4$ch4_co2_eq_sgwp)
  
  palustrine.methane.log.mean.sgwp <- mean(log(pal.ch4$ch4_co2_eq_sgwp))
  palustrine.methane.log.sd.sgwp <- sd(log(pal.ch4$ch4_co2_eq_sgwp))
  
  palustrine.methane.log.mean.gwp <- mean(log(pal.ch4$ch4_co2_eq_gwp))
  palustrine.methane.log.sd.gwp <- sd(log(pal.ch4$ch4_co2_eq_gwp))
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

# CAR Data
{
  car <- read.csv(paste(getwd(), "/data/MengReview/PbandcsData_170926.csv", sep=""))
  pb <- car$delSOC1[! is.na(car$delSOC1)]
  pb <- pb * carbonToCO2
  
}

pb210_and_methane <- data.frame(co2e_flux = c(pb, -est.ch4$ch4_co2_eq_gwp, -pal.ch4$ch4_co2_eq_gwp), 
                                  type = c(rep("soil carbon burial", length(pb)), 
                                           rep("estuarine methane", nrow(est.ch4)), rep("palustrine methane",  nrow(pal.ch4))))

palustrine_methane <-  data.frame(co2e_flux = -pal.ch4$ch4_co2_eq_gwp, 
                                  type = rep("palustrine methane", nrow(pal.ch4)))

total.range.1.a <- range(c(pb210_and_methane$co2e_flux[pb210_and_methane$type == "soil carbon burial"], 0.1))
total.range.1.b <- range(c(pb210_and_methane$co2e_flux[pb210_and_methane$type == "estuarine methane"], 1500))
total.range.1.c <- range(c(pb210_and_methane$co2e_flux[pb210_and_methane$type == "palustrine methane"], -0.1))

target_x.1a <- seq(total.range.1.a[1], total.range.1.a[2], by=0.1)
target_x.1b <- seq(total.range.1.b[1], total.range.1.b[2], by=0.1)
target_x.1c <- seq(total.range.1.c[1], total.range.1.c[2], by=0.1)

target_y.1a <- dlnorm(target_x.1a, mean(log(pb)), sd(log(pb)))
target_y.1b <- dnorm(-target_x.1b, mean(est.ch4$ch4_co2_eq_gwp), sd(est.ch4$ch4_co2_eq_gwp))
target_y.1c <- dlnorm(-target_x.1c, mean(log(pal.ch4$ch4_co2_eq_gwp)), sd(log(pal.ch4$ch4_co2_eq_gwp)))

lines_df <- data.frame(x = c(target_x.1a, target_x.1b, target_x.1c),
                       y = c(target_y.1a, target_y.1b, target_y.1c), 
                       type = c(rep("soil carbon burial", length(target_x.1a)), rep("estuarine methane", length(target_x.1b)), rep("palustrine methane", length(target_x.1c))))


ggplot(data = pb210_and_methane, aes(x = co2e_flux)) +
  facet_grid(type~.) +
  geom_histogram(aes(y = ..density..), fill="white", color = "darkgrey") +
  geom_line(data = lines_df, aes(x = x, y = y), lwd=1.25) +
  theme_bw() +
  xlab(expression(paste("Emissions (-) or Storage (+; gCO"["2"],"e m"^"-2", " yr"^"-1",")", sep="")))
