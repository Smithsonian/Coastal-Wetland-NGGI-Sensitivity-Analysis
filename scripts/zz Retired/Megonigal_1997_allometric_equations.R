library(tidyverse)
library(XML)

# allometric equations
# from megonigal et al., 1997 ecology appendix 2
allometric_equations_megonigal_1997 <- read_csv("data/Biomass/megonigal_1997/derrivative/megonigal_allometric_equations.csv")

convert_dbh_to_agb_megonigal_1997 <- function(dbh = 28,
                                              input_genus = "Quercus", input_species = "albo",
                                              diameter_units = "centimeters",
                                              allometric_equations_df = allometric_equations_megonigal_1997) {
  # This function comes from Megonigal et al. 1997 (Ecology, v. 78, p. 370-384) Appendix 2
  # Megonigal cites several sources with allometric equations for freshwater forested biomass
  # inputs include tree diameter at brest height (dbh). Specified units should be in centimeters.
  # Inputs also include genus and species. These should be characters. NA is not an acceptible input. For genus use 'other' for species use 'spp'.
  # The equations broken down by genus and species are in a separate file and should beD 
  
  # define conversions needed
  centimeters_per_inch <- 2.54
  kilograms_per_pound <- 0.453592
  
  # convert from inches to cm if necessary
  if (diameter_units == "inches" | diameter_units == "in") {
    dbh_cm <- dbh * centimeters_per_inch
  } else if (diameter_units == "centimeters" | diameter_units == "cm") {
    dbh_cm <- dbh
  } else {
    dbh_cm <- NA
  }
  
  # fliter allometric equations table to include only the species genus and diameger needed
  subset_equations <- allometric_equations_megonigal_1997 %>% # dplyr operation
    filter(genus == input_genus & species == input_species,
           min_diameter_cm <= dbh_cm & max_diameter_cm > dbh_cm)
  
  # input units needed for equation
  input_units <- subset_equations$input_units[1] 

  if (input_units == "inches") {
    input_dbh <- dbh_cm / centimeters_per_inch
  } else {
    input_dbh <- dbh_cm
  }
  
  subset_equations["input_dbh"] <- rep(input_dbh, nrow(subset_equations))
  
  perform_1st_half_of_allometric_equation <- function(d, parameter_1, parameter_2, d_input, equation_tranformation) {
    # equation forms 1: (A D^2)
    # 2: e ^ (B + (A log(D^2)))
    # 3: 10 ^ (B + (A log10(D)))
    if (is.na(d_input)) {
      d <- d
    } else if (d_input == "d_squared") {
      d <- d^2
    } else {
      d <- d
    }
    
    if (is.na(equation_tranformation)) {
      agb_output <- parameter_2 + parameter_1 * d
    } else if (equation_tranformation == "log10") {
      agb_output <- parameter_2 + parameter_1 * log10(d)
    } else if (equation_tranformation == "ln") {
      agb_output <- parameter_2 + parameter_1 * log(d)
    } else {
      agb_output <- parameter_2 + parameter_1 * d
    }
    return(agb_output)
  }
  
  perform_2nd_half_of_allometric_equation <- function(agb_sum, equation_tranformation) {
    if (is.na(equation_tranformation)) {
      agb_sum_output <- agb_sum
    } else if (equation_tranformation == "log10") {
      agb_sum_output <- 10^agb_sum
    } else if (equation_tranformation == "ln") {
      agb_sum_output <- exp(agb_sum)
    } else {
      agb_sum_output <- agb_sum
    }
    return(agb_sum_output)
  }
  
  agb_added <- subset_equations %>% 
    mutate(agb_1 = perform_1st_half_of_allometric_equation(d = input_dbh, 
                                          parameter_1 = parameter_1, parameter_2 = parameter_2,
                                          d_input = d_input, equation_tranformation = equation_tranformation))  %>% 
    group_by(genus, species, equation_tranformation) %>% 
    summarise(sum_agb_1 = sum(agb_1)) %>% 
    mutate(sum_agb = perform_2nd_half_of_allometric_equation(sum_agb_1, equation_tranformation = equation_tranformation)) %>% 
    select(genus, species, sum_agb)
  
  output_units <- subset_equations$output_units[1]
  if (output_units == "pounds") {
    agb_output_kg <- agb_added$sum_agb[1] * kilograms_per_pound
  } else {
    agb_output_kg <- agb_added$sum_agb[1]
  }
  
  return(agb_output_kg)
}


krauss_metadata_location <- "data/Biomass/krauss_2018/original_files/Carbonbudgetass/TFFW_Carbon_metadata.xml"
krauss_xmltree<- (xmlTreeParse(krauss_metadata_location))
xmlChildren(krauss_xmltree)


krauss_xmllist <- xmlToDataFrame(krauss_metadata_location)

krauss_xmllist$idinfo$taxonomy$taxoncl

convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Quercus", "alba")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Nyssa", "sylvatica")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Carya", "spp")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                 "Pinus", "taeda")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Celtis", "spp")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Taxodium", "distichum")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Quercus", "lyrata")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Quercus", "nuttalli")
convert_dbh_to_agb_megonigal_1997(dbh = 28, 
                                  "Taxodium", "distichum")

# allometric equation for Pinus taeda seems wrong
10 ^ ((1.56 + 2.59*log10(28)) + (1.57 + 2.01 * log10(28)))

10 ^ ((1.56 + 2.59*log10(28))) + 10^ ((1.57 + 2.01 * log10(28)))
