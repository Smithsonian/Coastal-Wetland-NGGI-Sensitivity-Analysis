library(tidyverse)


# Load Sara's Methane Data
methane <- read.csv("data/Methane/derivative/Methane Synthesis Knox.csv")
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")

# Prep methane data and convert to gCO2 eq per m2 per year
{
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
  palustrine.methane.mean.sgwp <- mean(pal.ch4$ch4_co2_eq_sgwp)
  
  palustrine.methane.log.mean.gwp <- mean(log(pal.ch4$ch4_co2_eq_gwp))
  palustrine.methane.log.sd.gwp <- sd(log(pal.ch4$ch4_co2_eq_gwp))
  palustrine.methane.mean.gwp <- mean(pal.ch4$ch4_co2_eq_gwp)
}

# Soil Carbon Accumulation Rate is log normally distributed because it can't be negative and has a long positive tail
generateLogNormalMeans <- function(x.n, x.log.mean, x.log.sd) { 
  simulatedData <- rlnorm(x.n, x.log.mean, x.log.sd) # generate the simulated dataset
  simulatedMedian <- median(simulatedData) # simulated median (also log mean), for local estimates
  simulatedMean <- mean(simulatedData) # sumulate actual mean, for national estimates
  # this comes in handy when analysing the difference between local and national effects
  return(list(median = simulatedMedian, mean = simulatedMean)) 
  
}

saved_iterations <- c()
saved_emissions <- c()
saved_type <- c()

for (i in 1:10000) {
  palustrine.methane.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.gwp, palustrine.methane.log.sd.gwp)
  saved_iterations <- c(saved_iterations, i, i)
  saved_emissions <- c(saved_emissions, palustrine.methane.randomDraw$median, palustrine.methane.randomDraw$mean)
  saved_type <- c(saved_type, "logmean", "mean")
}

all_iterations_df_10000 <- data.frame(i = saved_iterations,
                                      emissions = saved_emissions,
                                      central_tendency = saved_type)

all_iterations_summary_10000 <- all_iterations_df_10000 %>% 
  group_by(central_tendency) %>%
  summarise(lower_95 = quantile(emissions, 0.025),
            median_estimate = median(emissions),
            upper_95 = quantile(emissions, 0.975)) 

# cox method according to Olsson 2005
#http://ww2.amstat.org/publications/jse/v13n1/olsson.html
log.se <- sqrt((palustrine.methane.log.sd.gwp^2/palustrine.methane.n) + 
                 (palustrine.methane.log.sd.gwp^4/(2*(palustrine.methane.n-1))))
log_ci <- log.se * 1.96

mean_of_logdist <- exp(palustrine.methane.log.mean.gwp + palustrine.methane.log.sd.gwp^2*0.5)
mean_upper <- exp((palustrine.methane.log.mean.gwp + palustrine.methane.log.sd.gwp^2*0.5)  + log_ci)
mean_lower <- exp((palustrine.methane.log.mean.gwp + palustrine.methane.log.sd.gwp^2*0.5)  - log_ci)

# View Max Empirical Data
(max(methane["ch4_co2_eq_gwp"]))

# View Simulated CI's
(all_iterations_summary_10000)

# View Cox Method CI's
(mean_lower)
(mean_of_logdist)
(mean_upper)
