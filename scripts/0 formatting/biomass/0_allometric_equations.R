# Script with functions for allometric equations encountered when pulling together above ground biomass data 
#   for uncertainty and sensitivity analysis paper

# general agb from review in appendix 2 of Megongial et al., 1997
estimate_tff_agb_from_dbh_megonigal_1997 <- function(dbh = 12, genus = "Taxodium", species = "distichum") {
  # Inputs include diameter at breast height in cenitmeters,
  #   Genus and species are used to match to a specific allometric equations
  # Outputs will be grams of organic matter
  # Code modified from SAS code sent to James Holmquist by Pat Megonigal on 25 March 2018
  # Modifications made to be consistent with presentation in : Megonigal, J.P. et al., 1997. 
  #   ABOVEGROUND PRODUCTION IN SOUTHEASTERN FLOODPLAIN FORESTS: A TEST OF THE SUBSIDY–STRESS HYPOTHESIS. 
  #   Ecology, 78(2), pp.370–384.
  
  # If any of the inputs are NA, return NA
  if (is.na(dbh) | is.na(genus) | is.na(species)) {
    return(NA)
  } else {
    
    # First item is a vector of values that don't need to be converted from centimeters to inches and from pounds to kilograms
    # note change from Pat's code according to paper appendix 2 Fraxinus spp. do get converted
    #   they are not in this vector
    species_not_needing_empirical_conversions <- c("Nyssa aquatica", "Taxodium distichum", "Pinus taeda", "Salix nigra")
    
    if (paste(genus, species, sep = " ") %in% species_not_needing_empirical_conversions) {
      input_dbh = dbh
    } else {
      input_dbh = dbh / 2.54
    }
    
    # run through the list of possible allometric equations
    #   some need to be matched to a specific genus, some to a genus and a species, all others get categorized as Other spp.
    if (genus == "Acer" & species == "rubrum") {
      # Acer rubrum
      # this is a change from appendix 2 which says the function should be from 10 to 28. But there's no >28 version.
      #   I'm assuming it's a typo and is a > 10 function like any other
      if (dbh >= 28) {
        # this equation is not in the chart but is in the SAS code. I'm making an educated guess.
        # equation form 1
        biomass <- 1.69855*((input_dbh^2)^1.26161)
      } else if (dbh < 28 & dbh >= 10) {
        # this equation is in Appendix 2
        # equation form 1
        biomass <- 2.39959*((input_dbh^2)^1.2003) 
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Carya") {
      # Carya spp
      if (dbh >= 28) {
        # equation form 1 from appendix 2
        biomass <- 3.0015*((input_dbh^2) ^1.20454) 
      } else if (dbh < 28 & dbh >= 10) {
        # equation form 1 from appendix 2
        biomass <- 1.62114*((input_dbh^2)^1.33298) 
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Celtis") {
      # Celtis spp. 
      if (dbh >= 10) {
        # Equation form 3. Double checked SAS code against paper
        biomass <- exp(0.566+(1.25157*log((input_dbh^2))))
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Fraxinus") {
      # Fraxinus spp
      if (dbh >= 10) {
        # Equation form 1
        # this equation was in the paper but there is a different one listed in the SAS code
        biomass <- 2.669*((input_dbh^2)^1.16332)
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Liquidambar" & species == "styraciflua") {
      # Liquidambar styraciflua 
      if (dbh >= 28) {
        # Equation form 1 in appendix 2 and SAS code
        biomass <- 1.68032*((input_dbh^2)^1.27729)
      } else if (dbh < 28 & dbh >= 10) {
        # Eq. form 1. In appendix 2, not in SAS code.
        biomass <- 1.69699*((input_dbh^2)^1.27523)
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Lireodendron" & species == "tulipfera") {
      # Lireodendron tulipfera
      if (dbh >= 28) {
        # Equation form 1 in SAS code and appendix 2
        biomass <- 2.24272*((input_dbh^2)^1.19469)
      } else if (dbh < 28 & dbh >= 10) {
        # Eq. form 1. In appendix 2 but not in SAS code
        biomass <- 1.23262*((input_dbh^2)^1.3195)
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Nyssa" & species == "aquatica") {
      # Nyssa	aquatica
      # in the SAS code NYAQ and NYSY are together but in Appendix 2 they are separate
      #   I defer to paper's appendix 2 in this case
      if (dbh >= 10) {
        # Eq. in form 2 in appendix 2
        biomass <- 10^(-0.919+(2.291*log10(input_dbh)))
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Nyssa" & species == "sylvatica") {
      # Nyssa	sylvatica
      if (dbh >= 28) {
        # Equation form 1 
        # Equation is in SAS code and appendix 2
        biomass <- 1.30697*((input_dbh^2)^1.29943)
      } else if (dbh < 28 & dbh >= 10) {
        # Eq. form 1
        # Not in SAS code in appendix 2.
        biomass <- 2.43427*((input_dbh^2)^1.16974)
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Pinus" & species == "taeda") {
      # Pinus	taeda
      if (dbh >= 10) {
        # Equation form 4. Two terms boule and branches
        # checked against SAS code and appendix 2
        biomass <- (10^(1.56+(2.59*log10(input_dbh))))+
          (10^(1.57+(2.01*log10(input_dbh))))
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else if (genus == "Quercus") {
      if (species == "alba") {
        # Quercus	alba
        if (dbh >= 28) {
          # Equation form 1 in appendix 2 and SAS code 
          biomass <- 1.56965*((input_dbh^2)^1.34028)
        } else if (dbh < 28 & dbh >= 10) {
          # Eq. form 1 in appendix 2, not in SAS code.
          biomass <- 2.20767*((input_dbh^2)^1.26916)
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0
        }
      } else if (species == "laurifolia") {
        # Quercus	laurifolia
        if (dbh >= 28) {
          # Eq. form 1. In SAS code and appendix 2. 
          biomass <- 10.22597*((input_dbh^2)^0.94962)
        } else if (dbh < 28 & dbh >= 10) {
          # Eq. form 1. In appendix 2. Not in SAS code.
          biomass <- 2.89221*((input_dbh^2)^1.21296)
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0
        }
      } else if (species == "lyrata") {
        # Quercus	lyrata
        if (dbh >= 10) {
          # Eq. form 3. In SAS code and appendix 2.
          biomass <- exp(0.486+(1.25829*(log(input_dbh^2))))
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0 
        }
      } else if (species == "nigra") {
        # Quercus	nigra
        if (dbh >= 28) {
          # Eq. form 1. In appendix 2 and SAS code. 
          biomass <- 5.99898*((input_dbh^2)^1.08527)
        } else if (dbh < 28 & dbh >= 10) {
          # Eq. form 1. In appendix 2 but not SAS code. 
          biomass <- 3.15067*((input_dbh^2)^1.21955)
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0
        }
      } else if (species == "nuttalli") {
        # Quercus	nuttalli
        if (dbh >= 10) {
          # Eq. a variation of form 1. In appendix 2 and SAS code.
          biomass <- 2.83658*(input_dbh^2.38225);
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0 
        }
      } else {
        # Other Quercus species 
        if (dbh >= 28) {
          # Eq. 1 in SAS code and appendix 2.
          biomass <- 2.89492*((input_dbh^2)^1.22006)
        } else if (dbh < 28 & dbh >= 10) {
          # Eq. 1 in Appendix 2 but not SAS code.
          biomass <- 2.97559*((input_dbh^2)^1.21433)
        } else {
          # If diameter is smaller than 10 it's a sapling and doesn't get counted
          biomass <- 0 
        }
      }
    } else if (genus == "Taxodium" & species == "distichum") {
      # Taxodium distichum 
      if (dbh >= 10) {
        # Eq. form 2. In appendix 2 and SAS code.
        biomass <- 10^(-0.97+(2.34*log10(input_dbh)))
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0 
      }
    } else if (genus == "Salix" & species == "nigra") { # note: this species is in the sas code but not the paper
      # Salix nigra
      if (dbh >= 10) {
        # Eq. form 2. Not in appendix 2 but in SAS code.
        biomass <- 10^(-1.50+(2.78*log10(input_dbh)))
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    } else {
      # Any other species 
      if (dbh >= 28) {
        # Eq. form 1. In SAS code and appendix 2.
        biomass <- 1.80526*((input_dbh^2)^1.27313)
      } else if (dbh < 28 & dbh >= 10) {
        # Eq. form 1. In appendix 2 but not in SAS code.
        biomass <- 2.54671*((input_dbh^2)^1.20138)
      } else {
        # If diameter is smaller than 10 it's a sapling and doesn't get counted
        biomass <- 0
      }
    }
    
    # convert to kilograms if needed
    if (genus == "Pinus" & species == "taeda") {
      biomass_kg <- biomass / 1000 # Pinus taeda outputs g instead of kg. There is a typo in Appendix 2. 
      # Pat's SAS code confirms this.
    } else if  (paste(genus, species, sep = " ") %in% species_not_needing_empirical_conversions) {
      biomass_kg <- biomass # these ones don't need conversion
    } else {
      biomass_kg <- biomass / 2.204623 # these ones get converted from pounds to kilograms
    }
    
    # convert to grams for output
    biomss_g <- biomass_kg * 1000
    return(biomss_g)
  }
}

# general dbh to agb for north american hardwoods and softwoods outline in Jones et al., 2003
#   cited by Krauss et al., 2018
#   applied to Craft data
estimate_general_agb_from_dbh_jones_2003 <- function(dbh = 12, genus = "Pinus", species = "spp") {
  # Inputs include diameter at breast height in cenitmeters,
  #   Genus and species are used to match to a specific allometric equations
  # Outputs will be grams of organic matter
  # woodland
  # I put this first because Oaks (quercus) can be in woodland or hardwoods
  if (is.na(dbh) | is.na(genus) | is.na(species)) {
    return(NA)
  } else {
    if ((genus == "Quercus" & (species == "gambelii" | species == "hypoleucoides")) | 
        genus == "Acacia" | genus == "Cercocarpus" | genus == "Juniperus" | genus == "Prosopis") {
      #   Juniper/oak/mesquite
      B0 <- -0.7152
      B1 <- 1.7029
    }
    # hardwoods
    else if (genus == "Alnus" | genus == "Populus" | genus == "Salix") {
      #   Aspen/alder/cottonwood/willow
      B0 <- -2.2094
      B1 <- 2.3867
    } else if (genus == "Acer" | genus == "Betula") {
      #   Soft maple/birch 
      B0 <- -1.9123
      B1 <- 2.3651
    } else if (genus == "Aesculus" | genus == "Castanopsis" | genus == "Cornus" |
               genus == "Fraxinus" | genus == "Liquidambar" | genus == "Liriodendron" |
               genus == "Nyssa" | genus == "Oxydendrum" | genus == "Platanus" |
               genus == "Prunus" | genus == "Sassafras" | genus == "Tilia" | genus == "Ulmus" | 
               genus == "Persea" | genus == "Planera" | genus == "Ilex" | genus == "Carpinus") {
      #   Mixed Hardwood 
      B0 <- -2.4800
      B1 <- 2.4835
    } else if (genus == "Acer" | genus == "Carya" | genus == "Fagus" |
               genus == "Quercus") {
      #   Hard maple/oak/hickory/beech 
      B0 <- -2.0127
      B1 <- 2.4342
    }
    # softwoods
    else if (genus == "Calocedrus" | genus == "Chamaecyparis" | genus == "Thuja" |
             genus == "Juniperus" | genus == "Larix" | genus == "Sequoiadendron" |
             genus == "Taxodium") {
      #   Cedar/larch
      B0 <- -2.0336
      B1 <- 2.2592
    } else if (genus == "Pseudotsuga") {
      #   Douglas-fir
      B0 <- -2.2304
      B1 <- 2.4435
    } else if (genus == "Abies" | genus == "Tsuga") {
      #   True fir/hemlock
      B0 <- -2.5384
      B1 <- 2.4814
    } else if (genus == "Pinus") {
      #   Pine 
      B0 <- -2.5356
      B1 <- 2.4349
    } else if (genus == "Picea") {
      #   Spruce
      B0 <- -2.0773
      B1 <- 2.3323
    } else {
      # add NA values
      B0 <- NA
      B1 <- NA
    }
    
    output_agb_kg <- exp(B0 + B1*log(dbh))
    output_agh_g <- output_agb_kg * 1000
    return(output_agh_g)
  }
  
}

# Mangroves for 
#   cited in Doughty et al., 2015
#   applied to Twilley data
estimate_mangrove_agb_from_dbh_smith_2006 <- function(dbh = 12, genus ="Avicennia", species = "germinans") {
  # Inputs include diameter at breast height in cenitmeters,
  #   Genus and species are used to match to a specific allometric equations
  # Outputs will be grams of organic matter
  if (is.na(dbh) | is.na(genus) | is.na(species)) {
    # make sure none of the inputs are NA
    B0 <- NA
    B1 <- NA
  } else {
    if (genus == "Rhizophora" & species == "mangle") {
      # parameters for Rhizophora mangle
      B0 <- -0.112
      B1 <- 1.731
    } else if (genus == "Avicennia" & species == "germinans") {
      # parameters for Avicennia germinans
      B0 <- -0.395
      B1 <- 1.934
    } else if ((genus == "Laguncularia" & species == "racemosa") |
               (genus == "Conacarpus" & species == "erectus")) {
      # Parameters for Laguncularia racemosa
      # Made the conacarpus erectus the same because I couldn't find a allometric equation for it
      # ... and Simard 2006 used it instead.
      B0 <- -0.441
      B1 <- 1.930
    } else {
      # If inputs don't match an allometric equation make it a no data value
      B0 <- NA
      B1 <- NA
    }
    
    agb_kg <- 10^(B0 + B1*log10(dbh)) # run the allometric equation
    agb_g <- agb_kg * 1000 # convert from kilograms to grams
    
    return(agb_g)
  }
}

# Allometric Equations for Marc Simard's 2006 Paper
estimate_mangrove_agb_from_dbh_simard_2006 <- function(dbh = 12, genus ="Avicennia", species = "germinans") {
  # Functions from simard et al., 2006
  # ... simard et al site allometric equations developed by Fromard 1998
  # The function takes a genus a species and a diameter at breast height
  # ... and returns grams of above ground biomass
  if (is.na(dbh) | is.na(genus) | is.na(species)) {
    # If any of the inputs are missing return a no data values
    agb_kg <- NA
  } else {
    # If the data values exist
    if (dbh < 1) {
      # If the dbh is less than 1 cm apply averages derrived from the L-31 dataset (outlines in Simard et al., 2006 table 4)
      # There were additonal height classes for shrub and fringe mangroves, but none of the trees I'm analysing are below 2 m,
      # and none of the diameters are < 1 cm, so this seems fine
      if (genus == "Rhizophora" & species == "mangle") {
        # RHIMAN
        agb_kg <- 0.003406
      } else if (genus == "Laguncularia" & species == "racemosa") {
        # LAGRAC 
        agb_kg <- 0.001615
      } else if (genus == "Avicennia" & species == "germinans") {
        # AVIGER
        agb_kg <- 0.001301
      } else if (genus == "Conocarpus" & species == "erectus") {
        # CONERE
        agb_kg <- 0.001615
      } else {
        # Nothing
        agb_kg <- NA
      }
    } else if (dbh >= 1& dbh <5) {
      # apply second set of averages
      # If the dbh are between 1 and 5 cm apply averages derrived from the L-31 dataset 
      #   als outlined in Simard et al., 2006 Table 4.
      if (genus == "Rhizophora" & species == "mangle") {
        # RHIMAN 
        agb_kg <- 1.740
      } else if (genus == "Laguncularia" & species == "racemosa") {
        # LAGRAC 
        agb_kg <- 8.416
      } else if (genus == "Avicennia" & species == "germinans") {
        # AVIGER
        agb_kg <- 3.251
      } else if (genus == "Conocarpus" & species == "erectus") {
        # CONERE
        agb_kg <- 8.416
      } else {
        # Nothing
        agb_kg <- NA
      }
    } else {
      # If diameter is greater than 5 cm
      # ... apply allometric equations
      # Form: B0 * dbh ^ B1
      if (genus == "Rhizophora" & species == "mangle") {
        # RHIMAN
        B0 <- 0.1282
        B1 <- 2.6
      } else if (genus == "Avicennia" & species == "germinans") {
        # AVIGER
        B0 <- 0.1400
        B1 <- 2.4
      } else if ((genus == "Conocarpus" & species == "erectus") | 
                 (genus == "Laguncularia" & species == "racemosa")) {
        # CONERE and LAGREC are lumped together with a size dependent equation.
        if (dbh < 10) {
          B0 <- 0.1023
          B1 <- 2.5
        } else {
          # Note: simard uses the same values and equations for Conocarpus erectus and Rhizophora mangle
          # ... because there was no allometric equation for Conocarpus erectus.
          B0 <- 0.1282
          B1 <- 2.6
        }
      } else {
        # Nothing
        B0 <- NA
        B1 <- NA 
      }
      agb_kg <- B0 * dbh^B1 # apply parameters from allometric equation
    }
  } 
  agb_g <- agb_kg * 1000 # convert from kilograms to grams
  return(agb_g)
}

# Allometric Equations from Cheryl Doughty's 2015 paper
estimate_mangrove_agb_from_dbh_doughty_2015 <- function(dbh = 1, d30 = NA, genus ="Avicennia", species = "germinans") {
  # Function written to convert above ground biomass from diameter at breast height, or diamter at 30 cm from the ground
  # ... to above ground biomass.
  # Allometric equations come from: 
  # ... Doughty et al., 2015, Mangrove range expansion rapidly increases coastal wetland carbon storage. 
  # ...Estuaries and Coasts
  # Allometric equations are lited in Supplemental Tab. 2.
  # Inputs include diameter at breast height OR diameter above 30 cm for dwarf Launcularia racemosa, genus and species
  # Output is above ground biomass in grams.
  if (is.na(genus) | is.na(species) | (is.na(dbh) & is.na(d30))) {
    # If any of the inputs are missing return a no data values
    agb_kg <- NA
  } else {
    if (genus == "Rhizophora" & species == "mangle") {
      # equation from Smith and Whelen 
      agb_kg <- 10^(1.731*log10(dbh) - 0.112) # note the table lists Dr instead of DBH but excel file formula is the same
    } else if (genus == "Avicennia" & species == "germinans") {
      # equation from Smith and Whelen 
      if (is.na(dbh)) {
        dbh <- d30
      }
      agb_kg <- 10^(1.934*log10(dbh) - 0.395)
    } else if (genus == "Laguncularia" & species == "racemosa") {
      # If it was a dwarf shrub they would have measured d30 instead of dbh
      if (! is.na(d30)) {
        # If d30 was measured use the scrub equation
        # equation from Ross et al.
        agb_kg <- exp(1.021*log(d30 / 10) + 4.411) / 1000 # equation takes in d30 as millimeters and exports grams
      } else if (! is.na(dbh)) {
        # if d30 wasn't measured use the Sapling-Tall equation
        agb_kg <- 10^(1.930*log10(dbh) - 0.441)
      } else {
        agb_kg <- NA
      }
    } else {
      # Nothing
      agb_kg <- NA
    }
  } 
  agb_g <- agb_kg * 1000 # convert from kilograms to grams
  return(agb_g)
}
