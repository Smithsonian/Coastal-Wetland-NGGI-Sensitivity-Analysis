# Now we need a function to take the allometric equations from Megonigal et al, 1997
{
  estimate_tff_abg_biomass_from_dbh <- function(dbh = 27.1, genus = "Taxodium", species = "distichum") {
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
}

# Species to test
species_to_test <- c("Acer rubrum", "Carya spp", "Celtis spp", "Fraxinus spp",
                     "Liquidambar styraciflua", "Lireodendron tulipfera",
                     "Nyssa aquatica", "Nyssa sylvatica", "Pinus taeda",
                     "Quercus alba", "Quercus laurifolia", "Quercus lyrata", "Quercus nigra",
                     "Quercus nuttalli", "Quercus spp",
                     "Taxodium distichum", "Salix nigra",
                     "Other spp")

# print range of tree diameters  
diameters_to_test <- 10:84

df_for_visualizing_allometric_eqs <- expand.grid(dbh = diameters_to_test, species_code = species_to_test)

columns_to_add <- as.data.frame(str_split_fixed(df_for_visualizing_allometric_eqs$species_code, " ", 2))
df_for_visualizing_allometric_eqs["genus"] <- columns_to_add$V1
df_for_visualizing_allometric_eqs["species"] <- columns_to_add$V2
df_for_visualizing_allometric_eqs <- as_tibble(df_for_visualizing_allometric_eqs)

agb.test <- mapply(estimate_tff_abg_biomass_from_dbh, 
                   dbh = df_for_visualizing_allometric_eqs$dbh, 
                   genus = as.character(df_for_visualizing_allometric_eqs$genus),
                   species = as.character(df_for_visualizing_allometric_eqs$species))

df_for_visualizing_allometric_eqs["agb"] <- agb.test
ggplot(data = df_for_visualizing_allometric_eqs, aes(x = dbh, y = agb)) +
  geom_line(aes(color = species_code)) +
  facet_wrap(~species_code) +
  #scale_y_log10() + 
  ylab("Above Ground Biomass (gOM)") +
  xlab("Diameter at Breast Height (cm)") +
  theme_dark()