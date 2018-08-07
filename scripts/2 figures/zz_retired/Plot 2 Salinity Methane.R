# Make a figure to visualize methane emissions
methane <- read.csv("data/Methane/derivative/Methane Synthesis Knox.csv")
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")

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
methane["salinity_class"] <- ifelse(methane$Salinity.ppt >= 5, "estuarine", "palustrine")
methane["salinity_class_proposed"] <- ifelse(methane$Salinity.ppt >= 0.5, ifelse(methane$Salinity.ppt >= 18, "1 brackish to saline", "2 intermediate")
                                             , "3 fresh")

ggplot(data = methane, aes(x = salinity_class, y = ch4_co2_eq_gwp)) +
  geom_violin(aes(color = salinity_class)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 150) +
  xlab("Salinity Class") +
  ylab(expression(paste("CH"[4], " Flux (gCO"[2], "e m"^-2, " yr"^-1, ")", sep=""))) +
  guides(color=FALSE) +
  theme_minimal()

ggplot(data = methane, aes(x = salinity_class_proposed, y = ch4_co2_eq_gwp)) +
  geom_violin(aes(color = salinity_class_proposed)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 150) +
  xlab("Salinity Class") +
  ylab(expression(paste("CH"[4], " Flux (gCO"[2], "e m"^-2, " yr"^-1, ")", sep=""))) +
  guides(color=FALSE) +
  theme_minimal()
