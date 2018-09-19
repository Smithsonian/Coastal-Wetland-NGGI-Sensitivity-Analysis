# this will simulate the NGGI Based on Available Data
set.seed(5) # set seed so that analyses are replicable 

# load necessary packages 
{
  library(foreign) # to read .dbf files
  library(MASS) # to create multivariate normal distribution
  library(tidyverse) # to run data plyr and ggplot operations
  library(truncnorm) # to create truncated normal distributions
  library(gridExtra) # to combine ggplot graphs
  library(doParallel) # needed for parralel processing
  library(foreach) # needed for parralel processing
  library(doSNOW) # for progress bars in parallel processing step
}

# Define Important Conversion Factors
{
  gramsPerKg <- 1000 #1000 grams per kg
  m2PerHa <- 10000 # 10,000 meters squared per hectare
  millionHaPerHa <- 1E6 # 1 million hectare per hectare
  carbonPerBiomass <- 0.441 # carbon per biomass according to Byrd et al., 2018
  gramsPerPetagram <-1E15 # grams per petagram
  m2perPixel <- 900 # meters sequred per 30 x 30 meter pixel
  carbonToCO2 <- 3.666667 # moles carbon coverts to moles CO2
}

# load data files from data folder
{
  # all CCAP data from 2006 to 2010 for getting estuarine subcategory pixel counts
  ccap_fullTab <- read.dbf("data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf")
  
  # load ccap accuracy assesment data so we can simulate uncertainty due to accuracy
  # load accuracy assesment csv file
  ccap_aa <- read_csv("data/WetlandArea/CCAP/2010Classes/CCAP2010AccuracyAssessment.csv")
  ccap_aa <- as.matrix(ccap_aa[, 2:ncol(ccap_aa)]) # transform to matrix
  rownames(ccap_aa) <- colnames(ccap_aa) # match rownames to header names
  # load area data
  ccap_tab <- read.csv("data/WetlandArea/CCAP/2010Classes/ccapPixelCounts.csv")
  ccap_area <- ccap_tab$pixels # turn pixel counts into a vector
  
  # same as above for change no change
  # load accuracy assesment csv file
  cnc_aa <- read_csv("data/WetlandArea/CCAP/CNC/CCAP06to10ChangeNoChangeAccuracyAssesment.csv")
  cnc_aa <- as.matrix(cnc_aa[, 2:ncol(cnc_aa)])  # transform to matrix
  rownames(cnc_aa) <- colnames(cnc_aa) # match rownames to header names
  cnc_tab <- read.csv("data/WetlandArea/CCAP/CNC/cncPixelCounts.csv") # load area data
  cnc_area <- cnc_tab$pixels  # turn pixel counts column into a vector
  
  # input table from raster dataset of palustrine under NWI assumption
  palustrineNwi <- read.dbf("data/WetlandArea/Palustrine/nwi/CCAP2006to2010_wTab_PalMaskedByNwi.dbf")
  
  # define file path for the palustrine under coastal lands assumption
  palustrineFilePath <- "data/WetlandArea/Palustrine/coastalLands/PalustrinePixelCounts/_AllCONUS/tables"
  
  # Soils Data Summary from Holmquist et al., 2018 Scientific Reports
  soilCarbonMean_gCcm3 <- 0.027  # mean is in grams carbon per cubic centimeter
  # standard deviation in grams carbon per cubic centimeter
  soilCarbonSd_gCcm3 <- 0.013
  soilCarbonN <- 8280 # number of depth intervals
  
  # load CAR data
  car <- read.csv("data/MengReview/PbandcsData_170926.csv")
  
  # Biomass Values from Kristen's Byrd's Remote Sensing Calibraiton dataset ...
  # ... as well as multiple other sources for scrub/shrub and biomass
  biomass <- read_csv("data/Biomass/biomass_gCO2_distribution_summaries.csv")
  
  # Load Sara's Methane Data
  methane <- read.csv("data/Methane/derivative/Methane Synthesis Knox.csv")
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", 
                      "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
}

# Define assumptions re: land cover classes
{
  # C-CAP Classes
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 
                 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 
                 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 
                 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 
                 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Snow/Ice')
  
  # Define Palustrine and Estuarine Wetlands
  palustrineWetlands <- c('Palustrine Forested Wetland', 
                          'Palustrine Scrub/Shrub Wetland', 
                          'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 
                         'Estuarine Scrub/Shrub Wetland', 
                         'Estuarine Emergent Wetland')
  
  # devine abbreviations
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  # Define what counts as a soil loss event when a wetland converts
  soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 
                      'Low Intensity Developed', 'Developed Open Space',
                      'Cultivated', 'Pasture/Hay',
                      'Unconsolidated Shore', 'Water',
                      'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')
  
  # Define classes that count as forested vegetation
  forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
                 'Palustrine Forested Wetland',
                 'Estuarine Forested Wetland'
                 )
  
  # Define classes that are scrub/shrub vegetation
  scrubShrubVeg <- c('Scrub/Shrub',
             'Palustrine Scrub/Shrub Wetland',
             'Estuarine Scrub/Shrub Wetland'
             )
  
  # Define classes that are emergent vegetation
  emergentVeg <- c('Cultivated', 'Pasture/Hay', 
             'Grassland',
             'Palustrine Emergent Wetland',
             'Estuarine Emergent Wetland'
             )
  # Define non vegetated classes
  nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 
             'Low Intensity Developed', 'Developed Open Space',
             'Unconsolidated Shore', 'Bare Land', 'Water', 
             'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
             'Tundra',
             'Snow/Ice'
             )
  
  # This function takes class at t1 and t2 and 
  # outputs a general salinity class Estuarine, Palustrine or Neither
  classify_by_salinity <- function(class_time1 = "Estuarine Emergent Wetland", 
                                   class_time2 = "Open Water") {
    if (class_time1 %in% estuarineWetlands | 
        class_time2 %in% estuarineWetlands) {
      salinity_class <- "Estuarine"
    } else if (class_time1 %in% palustrineWetlands | 
               class_time2 %in% palustrineWetlands) {
      salinity_class <- "Palustrine"
    } else {
      salinity_class <- NA
    }
    return(salinity_class)
  }
  
  # This function takes class at t1 and t2 and outputs a stability class: stable and gains, losses, or NA
  classify_by_stability <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
    if (class_time1 %in%  estuarineWetlands | class_time1  %in%  palustrineWetlands | class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
      if (class_time2 %in% estuarineWetlands | class_time2 %in% palustrineWetlands) {
        stability_class <- "Stable and Gains"
      } else if ((! class_time2 %in% estuarineWetlands) & (! class_time2 %in% palustrineWetlands)) {
        stability_class <- "Losses"
      } else {
        stability_class <- NA
      }
    } else {
      stability_class <- NA 
    }
    return(stability_class)
  }
  
}

# Prep the tables for estimating area
{
  # Generate Table of Classes to Analyse with Abbreviations
  class2006ToAnalyse <- c()
  class2010ToAnalyse <- c()
  abbrev2006ToAnalyse <- c()
  abbrev2010ToAnalyse <- c()
  classesToAnalyse <- c()
  abbreviationsToAnalyse <- c()
  areaVariableType <- c()
  for (m in 1:length(classOrder)) {
    for (n in 1:length(classOrder)) {
      if ((classOrder[m] %in% c(palustrineWetlands, estuarineWetlands)) | 
          (classOrder[n] %in% c(palustrineWetlands, estuarineWetlands))) {
        class2006ToAnalyse <- c(class2006ToAnalyse, classOrder[m])
        abbrev2006ToAnalyse <- c(abbrev2006ToAnalyse, abbrevs[m])
        class2010ToAnalyse <- c(class2010ToAnalyse, classOrder[n])
        abbrev2010ToAnalyse <- c(abbrev2010ToAnalyse, abbrevs[n])
        classesToAnalyse <- c(classesToAnalyse, 
                              paste(classOrder[m], " to ", classOrder[n], sep=""))
        abbreviationsToAnalyse <- c(abbreviationsToAnalyse, 
                                    paste(abbrevs[m], "_", abbrevs[n], sep=""))
        if ((classOrder[m] %in% estuarineWetlands) | 
            (classOrder[n] %in% estuarineWetlands)) { 
          areaVariableType <- c(areaVariableType, "fixed") 
          } else { areaVariableType <- c(areaVariableType, "random") } 
      }
    }
  }
  ccapClassDf <- data.frame(class_2006 = class2006ToAnalyse, 
                            abbrev_2006 = abbrev2006ToAnalyse,
                            class_2010 = class2010ToAnalyse, 
                            abbrev_2010 = abbrev2010ToAnalyse,
                            class = classesToAnalyse, 
                            abbrev = abbreviationsToAnalyse, 
                            variableType = areaVariableType)
  
  # Compile a table of all of the Fixed Mapped Areas (Estuarine)
  estCcapClassDf <- subset(ccapClassDf, variableType == "fixed")
  estuarineClassPixelCounts <- c()
  for (i in 1:nrow(estCcapClassDf)) {
    subCCAP <- subset(ccap_fullTab, Class_Name == toString(estCcapClassDf$class[i]))
    if (nrow(subCCAP) > 0 ) {
      classPixelCount <- subCCAP$Count[1] 
    } else {
      classPixelCount <- 0
    }
    estuarineClassPixelCounts <- c(estuarineClassPixelCounts, classPixelCount)
  }
  estCcapClassDf["mappedPixelCount"] <- estuarineClassPixelCounts
  
  # Compile a table of all of the 'Random' Mapped Areas (Palustrine)
  palCcapClassDf <- subset(ccapClassDf, variableType == "random")
  
  palCcapClassDf_NWI <- palCcapClassDf
  # compile an alternate table for when we compare NWI area to coastal lands
  palustrineNWIClassPixelCounts <- c()
  for (i in 1:nrow(palCcapClassDf_NWI)) {
    subCCAP <- subset(palustrineNwi, Class_Name == toString(palCcapClassDf$class[i]))
    if (nrow(subCCAP) > 0 ) {
      classPixelCount <- subCCAP$Count[1] 
    } else {
      classPixelCount <- 0
    }
    palustrineNWIClassPixelCounts <- c(palustrineNWIClassPixelCounts, 
                                       classPixelCount)
  }
  palCcapClassDf_NWI["mappedPixelCount"] <- palustrineNWIClassPixelCounts
  palCcapClassDf_NWI["variableType"] <- rep("fixed", nrow(palCcapClassDf_NWI))
}

# Prep the Soil Carbon Data and convert to gCO2 eq. per m2 per year
{
  # convert to gCO2 per m3
  # x 100 cm. x 10,000 cm2 per m2 x 1 m
  cm.mean <- soilCarbonMean_gCcm3 * carbonToCO2 * 100 * 10000
  # x 100 cm. x 10,000 cm2 per m2 x 1 m
  cm.sd <- soilCarbonSd_gCcm3 * carbonToCO2 * 100 * 10000
  cm.n <- soilCarbonN
  
  # Split carbon accumulation rates into methods
  pb <- car$delSOC1[! is.na(car$delSOC1)]
  cs <- car$delSOC2[! is.na(car$delSOC2)]
  
  # Variables for the CAR in gC per m2 per year
  # Convert to gCO2 per m2 per year
  cs <- cs * carbonToCO2
  pb <- pb * carbonToCO2
  
  # in this case we're going with Cesium dated cores
  # CAR is log normally distrbuted because it can't be negative and has a tail
  cs.n <- length(cs) 
  cs.log.mean <- mean(log(cs))
  cs.log.sd <- sd(log(cs))
  
  # We'll have these ready in case we change our minds
  pb.n <- length(pb) 
  pb.log.mean <- mean(log(pb))
  pb.log.sd <- sd(log(pb))
  
}

# Prep Biomass data and convert to gCO2 eq per m2
{
  emergent.biomass.log.mean <- biomass$logmean[biomass$vegetation_class == "emergent"]
  emergent.biomass.log.sd <-  biomass$logsd[biomass$vegetation_class == "emergent"]
  emergent.biomass.mean <-  biomass$mean[biomass$vegetation_class == "emergent"]
  emergent.biomass.n <-  biomass$n[biomass$vegetation_class == "emergent"]
  
  scrub.shrub.biomass.log.mean <-  biomass$logmean[biomass$vegetation_class == "shrub"]
  scrub.shrub.biomass.log.sd <-  biomass$logsd[biomass$vegetation_class == "shrub"]
  scrub.shrub.biomass.mean <-  biomass$mean[biomass$vegetation_class == "shrub"]
  scrub.shrub.biomass.n <-  biomass$n[biomass$vegetation_class == "shrub"]
  
  forested.biomass.log.mean <- biomass$logmean[biomass$vegetation_class == "forest"]
  forested.biomass.log.sd <- biomass$logsd[biomass$vegetation_class == "forest"]
  forested.biomass.mean <- biomass$mean[biomass$vegetation_class == "forest"]
  forested.biomass.n <- biomass$n[biomass$vegetation_class == "forest"]
}

# Prep methane data and convert to gCO2 eq per m2 per year
{
  # using Scott Neubauer's CO2 Equivalents for SGWP and SGCP at 100 years
  generateMethaneSGPs <- function(ch4) {
    if (ch4 < 0) { return(ch4 * 203)
    } else { return(ch4 * 45)}
  }
  
  generateMethaneGPs <- function(ch4) { return(ch4 * 25) }
  
  # Units are in gCH4 per m2 per year
  # Calculate CO2 equivalents
  ch4_co2_eq_sgwp <- mapply(generateMethaneSGPs, methane$CH4.flux)
  # add to the dataframe so they can be sorted
  methane["ch4_co2_eq_sgwp"] <- ch4_co2_eq_sgwp
  
  # Calculate CO2 equivalents
  ch4_co2_eq_gwp <- mapply(generateMethaneGPs, methane$CH4.flux)
  # add to the dataframe so they can be sorted
  methane["ch4_co2_eq_gwp"] <- ch4_co2_eq_gwp
  
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

# Functions for Accuracy Assesment and Unbiased Area Estimation 
# from Oloffson et al., 2014
{
  propAreaCalc <- function(input_area) {
    # simply calculates proportional area
    return(input_area / sum(as.numeric(input_area))) 
    }
  
  # makes a matrix the proportional area matrix Eq. 4
  propAreaMatrix <-function(input_matrix, input_areas) {
    # proportional area instead of raw area
    input_pAreas <- propAreaCalc(input_areas) 
    n_rowSums <- rowSums(input_matrix) # counts per map class
    # proportional counts multiplied by proportional area of the mapped class
    areaPropMatrix <- (input_matrix / n_rowSums) * input_pAreas
    return(areaPropMatrix)
  }
  
  # function for user's accuracy given a confusion matrix. Eq. 2.
  u_accuracy <- function(input_matrix, input_areas) {
    # create proportional area matrix
    # calculate confusion matrix as proportional area rather than n
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) 
    ua_output <- c()
    q <- nrow(tempPropAreaMatrix) # number of classes
    for (i in 1:q) { # for each class
      # correct classifications divided by row sum
      ua_temp <- tempPropAreaMatrix[i,i] / sum(as.numeric(tempPropAreaMatrix[i,]))
      ua_output <-c(ua_output, ua_temp)
    }
    var <- c()
    for (i in 1:length(ua_output)) { # for each map class
      # Eq. 6
      temp_var <- (ua_output[i] * (1-ua_output[i])) / 
        (sum(as.numeric(input_matrix[i,])) - 1) 
      var <- c(var, temp_var)
    }
    se <- sqrt(var)
    ci <- 1.96 * se
    out_df <- data.frame(user_accuracy = ua_output, se = se , ci = ci)
    rownames(out_df) <- row.names(input_matrix)
    return(out_df)
  }
  
  # function for producer's accuracy given a confusion matrix. Eq. 3.
  p_accuracy <- function(input_matrix, input_areas) {
    # create proportional area matrix
    # calculate confusion matrix as proportional area rather than n
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas)
    pa_output <- c()
    q <- nrow(tempPropAreaMatrix)
    for (j in 1:q) { # for each reference class in q classes
      # correct classifications divided by column sum
      pa_temp <- tempPropAreaMatrix[j,j] / sum(as.numeric(tempPropAreaMatrix[,j]))
      pa_output <-c(pa_output, pa_temp)
    }
    
    # variance from Eq. 7.
    # need UA to calculate variance in PA
    user_accuracies <- u_accuracy(input_matrix, input_areas)$user_accuracy
    # matrix of n pixels in confusion matrix
    pixel_matrix <- tempPropAreaMatrix * sum(as.numeric(input_areas))
    var <- c()
    for (j in 1:length(pa_output)) { # for each reference class
      
      # Ndotj : the estimated number of pixels in reference class j. 
      #  (column sum of the 'pixel matrix')
      Ndotj <- sum(as.numeric(pixel_matrix[,j]))
      # number of pixels in map class j : from input areas data
      Njdot <- input_areas[j] 
      # number of sampling units in map class j : from input matrix
      njdot <- sum(as.numeric(input_matrix[j,]))
      
      exp1 <- (input_areas[j]^2) * ((1 - pa_output[j]) ^2) * user_accuracies[j] * 
        (1 - user_accuracies[j]) / (sum(as.numeric(input_matrix[,j])) - 1)
      
      # it's a big equation so I break it down into 3 parts
      exp2_store <- c() # storing part 1
      for (i in 1:q) { # for i map classes in q total
        if (i != j) { # when the map class does not match the reference class
          Nidot <- input_areas[i] # total number of pixels in the map class
          exp2_temp <- Nidot^2 * 
            (input_matrix[i,j] / sum(as.numeric(input_matrix[i,]))) * 
            (1 -   (input_matrix[i,j] / sum(as.numeric(input_matrix[i,])))) / 
            (sum(as.numeric(input_matrix[i,])) - 1)
          exp2_store <- c(exp2_store, exp2_temp)
        }
      }
      exp2 <- pa_output[j]^2 * sum(as.numeric(exp2_store))
      
      
      temp_var <- (1/Ndotj^2) * (exp1 + exp2)
      var <- c(var, temp_var)
    }
    
    se <- sqrt(var)
    ci <- 1.96 * se
    
    out_df <- data.frame(producer_accuracy = pa_output, se = se, ci = ci)
    rownames(out_df) <- row.names(input_matrix)
    return(out_df)
  }
  
  # function for total agreement given a confusion matrix. Eq. 1.
  o_accuracy <- function(input_matrix, input_areas) { 
    # calculate the proportional areas
    input_pAreas <- propAreaCalc(input_areas)
    # calculate confusion matrix as proportional area rather than n
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas)
    # overall accuracy
    overall_accuracy = (sum(as.numeric(diag(tempPropAreaMatrix)))) 
    
    # variance
    # calculate users accuracies
    user_accuracies <- u_accuracy(input_matrix, input_areas)$user_accuracy
    q <- nrow(tempPropAreaMatrix) # number of classes
    step2s <- c() # empty vector for storing results from classes
    for (i in 1:q) { # for each class
      # eq. 5 TOP Wi^2 * Ui * (1-Ui)
      step1 <- input_pAreas[i]^2 * user_accuracies[i] * (1 - user_accuracies[i])
      # eq. 5 BOTTOM: step1 / (ni. - 1) row sum
      step2 <- step1 / (sum(as.numeric(input_matrix[i,])) - 1) 
      step2s <- c(step2s, step2)
    }
    var <- sum(step2s) # eq. 5 outer Sum for variance
    se <- sqrt(var) # se = sqrt(var)
    ci <- 1.96 * se # ci = 1.96 * se
    
    # the sum of the diagonal for a proportional area confusion matrix
    return(data.frame(overall_accuracy = overall_accuracy, se = se, ci = ci))
  }
  
  areaCorrections <- function(input_matrix, input_areas) {
    
    # calculate the proportional areas
    input_pAreas <- propAreaCalc(input_areas) 
    # calculate confusion matrix as proportional area rather than n
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas)
    # Eq. 9: the column sums are the estimated area proportions, 
    #  as opposed to the row sums which are the mapped area proportions
    correctedAreaProps <- colSums(tempPropAreaMatrix)
    
    se <-c() # empty vector for se
    for (k in 1:nrow(input_matrix)) { # for each class
      step2s <- c()
      for (i in 1:nrow(input_matrix)) { # for each row in the confusion matrix
        # (Wi * pik) - pik^2: Eq. 10 TOP
        se_step1 = (input_pAreas[i] * tempPropAreaMatrix[i,k]) - 
          (tempPropAreaMatrix[i,k] ^ 2)
        # step 1 / (ni. - 1): Eq. 10 BOTTOM
        se_step2 = se_step1 / (sum(as.numeric(input_matrix[i,])) - 1 )
        step2s <- c(step2s, se_step2) # store row output
      }
      # square root of the sum of the rows: Eq. 10: OUTER
      se_step3 <- sqrt(sum(as.numeric(step2s)))
      se <-c(se, se_step3)
    }
    ci = se * 1.96 # CI from SE standard estimation formula
    
    # In Eq. 11 Oloffson et al. apply the estimated area proportions and the CI 
    #  by to the total map area to calculate estimated area
    # We do something slightly different that works out to be the same in the end
    # Because we are interested in propegating uncertainty at the pixel level
    #  we need to know two things
    # 1. If a pixel is present do we need to scale up or scale down based on 
    #  inclusion / exclusion probabilities of the mapped class
    # 2. We need to know what the CIs are on that 'scaler'
    
    # per pixel scaler is the ratio of the estimated occurence 
    #  to the mapped occurence
    perPixelScaler <- (correctedAreaProps / rowSums(tempPropAreaMatrix))
    scalerSE <- se / rowSums(tempPropAreaMatrix) # se calculated for scaler
    scalerCI <- ci / rowSums(tempPropAreaMatrix) # ci calculated for scaler
    
    # calculate estimated area using input class area and pixel based scaler
    estimatedArea <- perPixelScaler * input_areas
    estimatedAreaSE <- input_areas * scalerSE # se
    estimatedAreaCI <- input_areas * scalerCI # ci
    
    return(data.frame(perPixelScaler = perPixelScaler, 
                      scalerSE = scalerSE, 
                      scalerCI = scalerCI, 
                      originalArea = input_areas, 
                      estimatedArea = estimatedArea, 
                      estimatedAreaSE = estimatedAreaSE, 
                      estimatedAreaCI = estimatedAreaCI))
  }
}

# Functions for Generating Areas
{
  # Generate Estimated to Mapped Ratios for 2010 Class or Change/No Change
  
  # simulate data by category
  simulateAA <- function(aa) {
    for (i in 1:nrow(aa)){
      class_i <- aa[i,]
      simmClass_i <- rmultinom(1, sum(class_i), class_i)
      if (i ==1){
        temp_df <- as.data.frame(t(simmClass_i))
      } else {
        temp_df <- rbind(temp_df, t(simmClass_i)) 
      }
    } 
    row.names(temp_df) <- row.names(aa)
    colnames(temp_df) <- colnames(aa)
    return(temp_df)
  }
  
  simulatePerPixelScalers <- function(aa, area) {
    simmulateAA <- simulateAA(aa)
    simulatedPerPixelScaler <- areaCorrections(simmulateAA, area)$perPixelScaler
    outputDf <- data.frame(perPixelScaler = simulatedPerPixelScaler)
    row.names(outputDf) <- row.names(aa)
    return(as.data.frame(t(as.matrix(outputDf))))
  }
  
  # Generate Probibalistic Pixel Counts for a Palustrine Category with a Table
  simulatePalustrinePixelCounts <- function(inputDf="PEM_PEM", tabDir = paste(getwd(), "/data/WetlandArea/Palustrine/PalustrinePixelCounts/_AllCONUS/tables", sep="")) {
    # check to see if the file exists
    palTab <- paste(tabDir, "/", inputDf, ".dbf", sep="")
    if (! file.exists(palTab)) {
      return(0)
    } else {
      palTab <- read.dbf(palTab)
      # load .dbf with counts and probabilities associated with the class
      if (nrow(palTab) == 0) {
        return(0)
      } else {
        countsPerProbabilityStep <- c()
        for (i in 1:nrow(palTab)) {
          probabilityToSimulate <- palTab$Value[i] / 10000
          nToSimulate <- palTab$Count[i]
          simulatedCounts <- rbinom(nToSimulate, 1, probabilityToSimulate)
          countsPerProbabilityStep <- c(countsPerProbabilityStep, sum(as.numeric(simulatedCounts)))
        }
      }
      return(sum(as.numeric(countsPerProbabilityStep)))
    }
  }
  
  # Create a Table with all Possible Wetland Classifications and Estimated Areas
  generatePalustrineAreaTable <- function() {
    storeClassPixelCounts <- c()
    for ( i in 1:nrow(palCcapClassDf)) {
      classPixelCount <- simulatePalustrinePixelCounts(inputDf = toString(palCcapClassDf$abbrev[i]))
      storeClassPixelCounts <- c(storeClassPixelCounts, classPixelCount)
    }
    outputCcapDf <- palCcapClassDf
    outputCcapDf["mappedPixelCount"] <- storeClassPixelCounts
    return(outputCcapDf)
  }
}

# functions for generating palustrine areas
{
  calculateBinomialNormalApproximation <- function(inputDf="PEM_PEM", tabDir = paste(getwd(), "/data/WetlandArea/Palustrine/coastalLands/PalustrinePixelCounts/_AllCONUS/tables", sep="")) {
    palTab <- paste(tabDir, "/", toString(inputDf), ".dbf", sep="")
    if (! file.exists(palTab)) {
      mu <- 0
      sigma <- 0
    } else {
      palTab <- read.dbf(palTab)
      # load .dbf with counts and probabilities associated with the class
      if (nrow(palTab) == 0) {
        mu <- 0
        sigma <- 0
      } else {
        value <- palTab$Value / 10000
        count <- palTab$Count
        
        mu_vect <- c()
        for (i in 1:length(value)) {
          mu_temp <- value[i] * count[i]
          mu_vect <- c(mu_vect, mu_temp)
        }
        mu <- sum(mu_vect)
        
        var_vect <- c()
        for (i in 1:length(value)) {
          var_temp <- count[i] * (value[i] * (1-value[i]))
          var_vect <- c(var_vect, var_temp)
        }
        sigma = sqrt(sum(var_vect))
      }
    }
    return(data.frame(abbrev=toString(inputDf), mu=mu, sigma=sigma))
  }
  
  # Make a table that has mu and sigma values for each of the 
  for (i in 1:nrow(palCcapClassDf)) {
    normalApproxTableRow <- calculateBinomialNormalApproximation(palCcapClassDf$abbrev[i])
    if (i == 1) {
      palustrineNormalApproximations <- normalApproxTableRow
    } else 
      palustrineNormalApproximations <- rbind(palustrineNormalApproximations, normalApproxTableRow)
  }
  
  # Function for randomly generating table of palustrine wetland pixle counts 
  #  based on binomial distribution normal approximation
  generatePalustrineAreaTableBNA <- function(normalApproxTable=palustrineNormalApproximations,
                                           joinTable=palCcapClassDf) {
    
    # generate 1 random normal value for each class
    palustrineMappedPixels <- mapply(rnorm, 1, normalApproxTable$mu, normalApproxTable$sigma)
    # remove 0 values
    palustrineMappedPixels[palustrineMappedPixels<0] <- 0
    # convert to integer
    palustrineMappedPixels <- as.integer(palustrineMappedPixels)
    joinTable["mappedPixelCount"] <- palustrineMappedPixels
    return(joinTable)
  }
}

# Functions for Generating Soil CAR, Stock, Biomass, and CH4 Values
{
  # Soil Carbon Accumulation Rate is log normally distributed 
  #  because it can't be negative and has a long positive tail
  generateLogNormalMeans <- function(x.n, x.log.mean, x.log.sd) { 
    # generate the simulated dataset
    simulatedData <- rlnorm(x.n, x.log.mean, x.log.sd)
    # simulated median (also log mean), for local estimates
    simulatedMedian <- median(simulatedData) 
    # sumulate actual mean, for national estimates
    simulatedMean <- mean(simulatedData)
    # this comes in handy when analysing the difference 
    #  between local and national effects
    return(list(median = simulatedMedian, mean = simulatedMean)) 
    
    }
  
  generateNormalMeans <- function(x.n, x.mean, x.sd) { 
    return(mean(rnorm(x.n, x.mean, x.sd))) 
    }
  
  generateTruncatedNormalMeans <- function(x.n, x.mean, 
                                           x.sd, lowest_value = 0) { 
    return(mean(rtruncnorm(n=x.n, a=lowest_value, mean = x.mean, sd=x.sd))) 
    }
  generateDepthLost <- function(depth.min = 0.5, depth.max=1.5) { 
    return(runif(1, depth.min, depth.max)) 
    } 
  
  generateSoilEmissionsFactor <- function(cMassTable, depthIntervalLost) {
    return(sum(colMeans(cMassTable[,1:depthIntervalLost], na.rm = T)))
  }
}

# create some initial (.1) data.frames with just means for all 
#  mapped area, area scale factors, and emissions/storage factors
{
  # load estuarine mapped pixels
  estuarineMappedPixels.1 = estCcapClassDf
  estuarineMappedPixels.1["mappedPixelCountSD"] <- rep(NA, nrow(estuarineMappedPixels.1))
  
  # get the average number of palustrine pixels
  palustrineMappedPixels.1 <- palCcapClassDf
  palustrineMappedPixels.1["mappedPixelCount"] <- as.integer(palustrineNormalApproximations$mu)
  palustrineMappedPixels.1["mappedPixelCountSD"] <- as.integer(palustrineNormalApproximations$sigma)
  
  # bind two tables and export for reporting purposes
  totalMappedPixels.1 <- rbind(estuarineMappedPixels.1, palustrineMappedPixels.1)
  totalMappedPixels.1 <- totalMappedPixels.1[order(-totalMappedPixels.1$mappedPixelCount), ]
  write.table(totalMappedPixels.1, "data/outputTables/totalMappedPixels_coastalLands.csv", sep=",", row.names = F)
  
  # Do the same for Palustrine NWI table
  palustrineMappedPixels.1.NWI <- palCcapClassDf_NWI
  palustrineMappedPixels.1.NWI["mappedPixelCountSD"] <- rep(NA, nrow(palustrineMappedPixels.1.NWI))
  totalMappedPixels.1.NWI <- rbind(estuarineMappedPixels.1, palustrineMappedPixels.1.NWI)
  totalMappedPixels.1.NWI <- totalMappedPixels.1.NWI[order(-totalMappedPixels.1.NWI$mappedPixelCount), ]
  write.table(totalMappedPixels.1.NWI, "data/outputTables/totalMappedPixels_NWI.csv", sep=",", row.names = F)
  
  # Per pixel scalers
  ccap2010perPixelScalers.1 <- as.data.frame(t(as.matrix(areaCorrections(ccap_aa, ccap_area)$perPixelScaler)))
  names(ccap2010perPixelScalers.1) <- row.names(ccap_aa)

  cncPerPixelScalers.1 <- as.data.frame(t(as.matrix(areaCorrections(cnc_aa, cnc_area)$perPixelScaler)))
  names(cncPerPixelScalers.1) <- row.names(cnc_aa)
  
  storageAndEmissions.1 = data.frame(soil.burial = mean(pb),
                                     soil.carbon.density = cm.mean,
                                     emergent.biomass = emergent.biomass.mean, 
                                     scrub.shrub.biomass = scrub.shrub.biomass.mean, 
                                     forested.biomass = forested.biomass.mean,
                                     estuarine.methane = estuarine.methane.mean.gwp, 
                                     palustrine.methane = palustrine.methane.mean.gwp,
                                     estuarine.methane.sgwp = estuarine.methane.mean.sgwp, 
                                     palustrine.methane.sgwp = palustrine.methane.mean.sgwp
                                     )
}

# Function for Calculating the Inventory
{
  # Inputs are mapped pixels, area scalers, and storage/emissions factors
  coastalNGGI <- function(estuarineMappedPixels = estuarineMappedPixels.1, 
                          palustrineMappedPixels = palustrineMappedPixels.1, 
                          ccap2010perPixelScalers = ccap2010perPixelScalers.1, 
                          cncPerPixelScalers = cncPerPixelScalers.1,
                          storageAndEmissions = storageAndEmissions.1,
                          depth.lost = 1, fraction.loss = 0.625,
                          gwp = T) {
    
    wetlandMappedPixels <- rbind(estuarineMappedPixels, palustrineMappedPixels)
    
    # for every class in estuarineMappedPixels and palustrineMappedPixels
    mapped_pixel_count_vect <- c()
    estimated_pixel_count_vect <- c() 
    soil_gCO2perm2_vect <- c()
    biomass_gCO2perm2_vect <-c()
    methane_gCO2perm2_vect <-c()
    
    for (i in 1:nrow(wetlandMappedPixels)) {
      class.t1 <-  wetlandMappedPixels$class_2006[i]
      class.t2 <-  wetlandMappedPixels$class_2010[i]
      abbrev.t2 <- toString(wetlandMappedPixels$abbrev_2010[i])
      
      # calculate scaled area
      {
        # Multiply number of mapped pixels by 
        #  the estimated to mapped ratio for the 2010 class 
        mapped.pixel.n = wetlandMappedPixels$mappedPixelCount[i]
        estimated.pixel.n = mapped.pixel.n * ccap2010perPixelScalers[, abbrev.t2]
        # Multiply number of mapped pixels by the 
        #  estimated to mapped ratio for the 2006-2010 change class 
        # if there's no change scale by the 'no change' estimated to mapped ratio 
        if (class.t1 == class.t2) {
          estimated.pixel.n = mapped.pixel.n * cncPerPixelScalers$No.Change[1]
          # if there's a change scale number of pixels by the 'change' 
          #  estimated to mapped ratio
        } else {
          estimated.pixel.n = estimated.pixel.n * cncPerPixelScalers$Change[1]
        }
      }
      
      # calculate soil change
      {
        # is it a wetland to wetland transition 
        #  (wetland at time 1 and wetland at time 2)
        if ((class.t1 %in% c(estuarineWetlands, palustrineWetlands)) & 
            (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
          # if it's a wetland remaining wetland then it buries carbon for 5 years
          soil.change =  storageAndEmissions$soil.burial[1] * 5
        } else {
          # is it a wetland restoration
          #  (non wetland at time 1 to wetland at time 2)
          if ((! (class.t1 %in% c(estuarineWetlands, palustrineWetlands))) & 
              (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
            # if a class goes from not wetland to wetland we assume 
            #  it starts buring carbon half way through the time step
            soil.change =  storageAndEmissions$soil.burial[1] * 2.5
          } else { # is it a wetland loss
            # is it a soil loss event, defined in the 'assumptions section'? 
            if (class.t2 %in% soilLossEvents) {
              # if this is a soil loss even, then the whole column is lost at once
              soil.change = -(storageAndEmissions$soil.carbon.density * 
                                depth.lost * fraction.loss) 
            } else {
              # if it is a wetland loss without soil loss, then 
              #  we assume it stops buring carbon halfway through the timestep 
              soil.change  = storageAndEmissions$soil.burial[1] * 2.5
            }
          }
        }
      }
      
      # calculate biomass change
      {  
        # what was the biomass at time step 1?
        if (class.t1 %in% emergentVeg) {
          biomass.t1 <- storageAndEmissions$emergent.biomass[1]
        } else if (class.t1 %in% scrubShrubVeg) {
          biomass.t1 <- storageAndEmissions$scrub.shrub.biomass[1]
        } else if (class.t1 %in% forestVeg) {
          biomass.t1 <- storageAndEmissions$forested.biomass[1]
        } else if (class.t1 %in% nonVeg)  {
          biomass.t1 <- 0
        } else {
          biomass.t1 <- 0
        }
          
        # what was the biomass in at time step 2?
        if (class.t2 %in% emergentVeg) {
          biomass.t2 <- storageAndEmissions$emergent.biomass[1]
        } else if (class.t2 %in% scrubShrubVeg) {
          biomass.t2 <- storageAndEmissions$scrub.shrub.biomass[1]
        } else if (class.t2 %in% forestVeg) {
          biomass.t2 <- storageAndEmissions$forested.biomass[1]
        } else if (class.t2 %in% nonVeg)  {
          biomass.t2 <- 0
        } else {
          biomass.t2 <- 0
        }
        # year 2 - year 1
        biomass.change = biomass.t2 - biomass.t1
      }
      
      if (gwp == T) {
        estuarine.methane <- storageAndEmissions$estuarine.methane
        palustrine.methane <- storageAndEmissions$palustrine.methane
      } else {
        estuarine.methane <- storageAndEmissions$estuarine.methane.sgwp
        palustrine.methane <- storageAndEmissions$palustrine.methane.sgwp
      }
  
      # calculate methane emissions
      {  
        # What were the emissions at the start?
        if (class.t1 %in% estuarineWetlands) {
          methane.t1 <- estuarine.methane
        } else if (class.t1 %in% palustrineWetlands) {
          methane.t1 <- palustrine.methane
        } else {
          methane.t1 <- 0
        }
        
        if (class.t2 %in% estuarineWetlands) {
          methane.t2 <- estuarine.methane
        } else if (class.t2 %in% palustrineWetlands) {
          methane.t2 <- palustrine.methane
        } else {
          methane.t2 <- 0
        }
        
        # What were the emissions at the end?
        methane.change = -(2.5*methane.t1 + 2.5*methane.t2)
      }
      
      mapped_pixel_count_vect <- c(mapped_pixel_count_vect, mapped.pixel.n)
      estimated_pixel_count_vect <- c(estimated_pixel_count_vect, estimated.pixel.n) 
      soil_gCO2perm2_vect <- c(soil_gCO2perm2_vect, soil.change)
      biomass_gCO2perm2_vect <-c(biomass_gCO2perm2_vect, biomass.change)
      methane_gCO2perm2_vect <-c(methane_gCO2perm2_vect, methane.change)
    }
    # Summarize accross the entire inventory
    total_df <- data.frame(estimated_pixel_count = estimated_pixel_count_vect)
    total_df <- cbind(wetlandMappedPixels, total_df)
    total_df["total_gCO2perM2"] <- (soil_gCO2perm2_vect + 
                                      biomass_gCO2perm2_vect + 
                                      methane_gCO2perm2_vect)
    total_df["soil_gCO2perM2"] <- soil_gCO2perm2_vect
    total_df["biomass_gCO2perM2"] <- biomass_gCO2perm2_vect
    total_df["methane_gCO2perM2"] <- methane_gCO2perm2_vect
    
    total_tonnesCO2 <- sum((total_df$estimated_pixel_count * 
                              total_df$total_gCO2perM2 * 900 / 1E6), na.rm=T)
    
    detailed_output <- list(total_tonnesCO2, total_df)
    
    return(detailed_output)
  }
}

# Function for performing a random draw of the Inventory
{
  # choose regional OR national analysis
  run_randomized_coastalNGGI <- function(scopeOfAnalysis = "national",
                                         iterationCode = NA) {
    
    require(truncnorm)
    
    estuarineMappedPixels.fixedVariables <- estCcapClassDf
    # Create Input Tables by randomly drawing from the data's probability distributions  
    # random draws for area mapping
    palustrineMappedPixels.randomDraw <- generatePalustrineAreaTableBNA()
    ccap2010perPixelScalers.randomDraw <- simulatePerPixelScalers(ccap_aa, ccap_area)
    cncPerPixelScalers.randomDraw <- simulatePerPixelScalers(cnc_aa, cnc_area)
    
    # random draws for soil carbon emissions/storage factors
    if (scopeOfAnalysis == "regional") {
      soil.burial.randomDraw <- generateLogNormalMeans(pb.n, pb.log.mean, pb.log.sd)$median
    } else if (scopeOfAnalysis == "national") {
      soil.burial.randomDraw <- generateLogNormalMeans(pb.n, pb.log.mean, pb.log.sd)$mean
    } else {
      soil.burial.randomDraw <- NA
    }
    
    depth.lost.randomDraw <- generateDepthLost()
    soil.carbon.density.randomDraw <- generateTruncatedNormalMeans(cm.n, cm.mean, cm.sd)
    fraction.loss.randomDraw <- runif(1, 0.5, 0.75)
    
    # random draws for biomass emissions/storage factors
    if (scopeOfAnalysis == "regional") {
      emergent.biomass.randomDraw <- generateLogNormalMeans(emergent.biomass.n, emergent.biomass.log.mean, emergent.biomass.log.sd)$median
      scrub.shrub.biomass.randomDraw <- generateLogNormalMeans(scrub.shrub.biomass.n, scrub.shrub.biomass.log.mean, scrub.shrub.biomass.log.sd)$median
      forested.biomass.randomDraw <- generateLogNormalMeans(forested.biomass.n, forested.biomass.log.mean, forested.biomass.log.sd)$median
    } else if (scopeOfAnalysis == "national") {
      emergent.biomass.randomDraw <- generateLogNormalMeans(emergent.biomass.n, emergent.biomass.log.mean, emergent.biomass.log.sd)$mean
      scrub.shrub.biomass.randomDraw <- generateLogNormalMeans(scrub.shrub.biomass.n, scrub.shrub.biomass.log.mean, scrub.shrub.biomass.log.sd)$mean
      forested.biomass.randomDraw <- generateLogNormalMeans(forested.biomass.n, forested.biomass.log.mean, forested.biomass.log.sd)$mean
    } else {
      emergent.biomass.randomDraw <- NA
      scrub.shrub.biomass.randomDraw <- NA
      forested.biomass.randomDraw <- NA
    }
    
    # random draws for methane emissions factors
    estuarine.methane.randomDraw <- generateNormalMeans(estuarine.methane.n, estuarine.methane.mean.gwp, estuarine.methane.sd.gwp)
    estuarine.methane.sgwp.randomDraw <- generateNormalMeans(estuarine.methane.n, estuarine.methane.mean.sgwp, estuarine.methane.sd.sgwp)
    
    if (scopeOfAnalysis == "regional") {
      palustrine.methane.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.gwp, palustrine.methane.log.sd.gwp)$median
      palustrine.methane.sgwp.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.sgwp, palustrine.methane.log.sd.sgwp)$median
    } else if (scopeOfAnalysis == "national") {
      palustrine.methane.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.gwp, palustrine.methane.log.sd.gwp)$mean
      palustrine.methane.sgwp.randomDraw <- generateLogNormalMeans(palustrine.methane.n, palustrine.methane.log.mean.sgwp, palustrine.methane.log.sd.sgwp)$mean
    } else {
      palustrine.methane.randomDraw <- NA
      palustrine.methane.sgwp.randomDraw <- NA
    }
    
    storageAndEmissions.randomDraw = data.frame(soil.burial = soil.burial.randomDraw,
                                                soil.carbon.density = soil.carbon.density.randomDraw, 
                                                emergent.biomass = emergent.biomass.randomDraw, scrub.shrub.biomass = scrub.shrub.biomass.randomDraw, forested.biomass = forested.biomass.randomDraw,
                                                estuarine.methane = estuarine.methane.randomDraw, palustrine.methane = palustrine.methane.randomDraw,
                                                estuarine.methane.sgwp = estuarine.methane.sgwp.randomDraw, palustrine.methane.sgwp = palustrine.methane.sgwp.randomDraw)
    
    # Run NGGI Function
    coastalNGGI.randomDraw <- coastalNGGI(estuarineMappedPixels=estuarineMappedPixels.fixedVariables,
                                          palustrineMappedPixels=palustrineMappedPixels.randomDraw,
                                          ccap2010perPixelScalers=ccap2010perPixelScalers.randomDraw,
                                          cncPerPixelScalers=cncPerPixelScalers.randomDraw,
                                          storageAndEmissions=storageAndEmissions.randomDraw,
                                          depth.lost = depth.lost.randomDraw, fraction.loss = fraction.loss.randomDraw
                                          )[[2]]
    
    # write output data.tables
    palustrineMappedPixels.savedIterations <- as.data.frame(t(as.matrix(palustrineMappedPixels.randomDraw$mappedPixelCount)))
    colnames(palustrineMappedPixels.savedIterations) <- palustrineMappedPixels.randomDraw$abbrev
    ccap2010perPixelScalers.savedIterations <- ccap2010perPixelScalers.randomDraw
    cncPerPixelScalers.savedIterations <- cncPerPixelScalers.randomDraw
    storageAndEmissions.savedIterations <- storageAndEmissions.randomDraw
    total_df.saved.iterations <- coastalNGGI.randomDraw
    
    output_list <- list(palustrineMappedPixels.savedIterations,
                    ccap2010perPixelScalers.savedIterations, cncPerPixelScalers.savedIterations,
                    storageAndEmissions.savedIterations, total_df.saved.iterations)
    
    for (j in 1:length(output_list)) {
      output_list[[j]]["iterationCode"] <- rep(iterationCode, nrow(output_list[[j]]))
    }
    
    return(output_list)
    }
  
  example <- run_randomized_coastalNGGI()
}
  
# run 10,000 random draws of the NGGI on the 'national' setting.
{
  # define number of iterations
  iterations <- 10000
  
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  registerDoSNOW(cl)
  
  # I looked up how to make progress bars in parallel on Stack Exchange
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # copied this solution from stack exchange 
  # https://stackoverflow.com/questions/27279164/output-list-of-two-rbinded-data-frames-with-foreach-in-r
  # takes an arbitrary number of lists x all of which much have the same structure    
  comb <- function(x, ...) {  
    mapply(rbind,x,...,SIMPLIFY=FALSE)
  }
  
  finalDataframe <- foreach(k=1:iterations, .combine = 'comb', .multicombine=TRUE,
                            .options.snow = opts) %dopar% {
    tempDF_list <- run_randomized_coastalNGGI(iterationCode = k) #calling a function

    tempDF_list #Equivalent to outputDF = rbind(outputDF, tempDF)
  }
  
  #stop cluster
  stopCluster(cl)
  
  # write to file
  write_csv(finalDataframe[[1]], "data/outputTables/MonteCarloResults/national/palustrineMappedPixels.savedIterations.csv")
  write_csv(finalDataframe[[2]], "data/outputTables/MonteCarloResults/national/ccap2010perPixelScalers.savedIterations.csv")
  write_csv(finalDataframe[[3]], "data/outputTables/MonteCarloResults/national/cncPerPixelScalers.savedIterations.csv")
  write_csv(finalDataframe[[4]], "data/outputTables/MonteCarloResults/national/storageAndEmissions.savedIterations.csv")
  write_csv(finalDataframe[[5]], "data/outputTables/MonteCarloResults/national/total.savedIterations.csv")
}

# Re-run The Inventory with median values 1 input-variable at a time 
#  for the national scale sensitivity analysis
{ 
  # for all of the parameter (input) tables generate the median, 
  #  and upper-lower (95% credible intervals)
  getColumnCIs <- function(input_df = palustrineMappedPixels.savedIterations) {
    input_df <- select(input_df, -iterationCode)
    for (i in 1:ncol(input_df)) {
      CredibleIntervals <- quantile(input_df[,i], c(0.025, 0.5, 0.975)) 
      if (i == 1) { output_df <- data.frame(CredibleIntervals) }
      else { output_df <- cbind(output_df, CredibleIntervals) }
    }
    colnames(output_df) <- colnames(input_df)
    return(output_df)
  }
  
  # import saved iteration tables from uncertainty analysis so to be used as inputs in the sensitivity analysis
  palustrineMappedPixels.savedIterations <- read.csv("data/outputTables/MonteCarloResults/national/palustrineMappedPixels.savedIterations.csv")
  ccap2010perPixelScalers.savedIterations <- read.csv("data/outputTables/MonteCarloResults/national/ccap2010perPixelScalers.savedIterations.csv")
  cncPerPixelScalers.savedIterations <- read.csv("data/outputTables/MonteCarloResults/national/cncPerPixelScalers.savedIterations.csv")
  storageAndEmissions.savedIterations <- read.csv("data/outputTables/MonteCarloResults/national/storageAndEmissions.savedIterations.csv")
  
  run_coastalNGGI_sensitivity <- function(estuarineMappedPixels = estuarineMappedPixels.1,
                                          palustrineMappedPixels.savedIterations = palustrineMappedPixels.savedIterations,
                                          palustrineMappedPixels.NWI = palustrineMappedPixels.1.NWI,
                                          ccap2010perPixelScalers.savedIterations = ccap2010perPixelScalers.savedIterations,
                                          cncPerPixelScalers.savedIterations = cncPerPixelScalers.savedIterations,
                                          storageAndEmissions.savedIterations = storageAndEmissions.savedIterations
                                          ) {
    
    # get the medians and confidence intervals for all parameters 
    palustrineMappedPixels.CIs <- getColumnCIs(palustrineMappedPixels.savedIterations)
    ccap2010perPixelScalers.CIs <- getColumnCIs(ccap2010perPixelScalers.savedIterations)
    cncPerPixelScalers.CIs <- getColumnCIs(cncPerPixelScalers.savedIterations)
    storageAndEmissions.CIs <- getColumnCIs(storageAndEmissions.savedIterations) 
    
    parameterNameStore <- c() # empty vector to store parameter names
    parameterTypeStore <- c() # empty vector to store parameter types
    # empty vector to store 'effect' of parameter on overall total
    parameterEffectStore <- c()
    
    # iterate through each parameter table
    # start with palustrineMappedPixels
    for (i in 1:ncol(palustrineMappedPixels.CIs)) {
      
      # prep the input data tables
      {
        # Inputs are mapped pixels, area scalers, and storage/emissions factors
        palustrineMappedMedians <- palustrineMappedPixels.CIs[2,]
        # analyse min 
        palustrineMappedMins <- palustrineMappedMedians
        palustrineMappedMaxs <- palustrineMappedMedians
        
        palustrineMappedMins[1,i] <- palustrineMappedPixels.CIs[1,i]
        palustrineMappedMaxs[1,i] <- palustrineMappedPixels.CIs[3,i]
        
        palustrineMappedMinsTable <- palCcapClassDf
        palustrineMappedMinsTable["mappedPixelCount"] <- c(t(palustrineMappedMins[1,]))
        palustrineMappedMinsTable["mappedPixelCountSD"] <- rep(NA, nrow(palustrineMappedMinsTable))
        
        palustrineMappedMaxsTable <- palCcapClassDf
        palustrineMappedMaxsTable["mappedPixelCount"] <- c(t(palustrineMappedMaxs[1,]))
        palustrineMappedMaxsTable["mappedPixelCountSD"] <- rep(NA, nrow(palustrineMappedMaxsTable))
      }
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedMinsTable, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedMaxsTable, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      parameterNameStore <- c(parameterNameStore, paste(gsub("_", " to ", palustrineMappedMaxsTable$abbrev[i]), " mapped area", sep=""))
      parameterTypeStore <- c(parameterTypeStore, "Coastal Lands Mapping")
      parameterEffectStore <- c(parameterEffectStore, abs(minEstimate - maxEstimate))
    }
    
    # the rest of the analyses will use this input table for palustrine pixel count
    palustrineMappedPixels.CIs.Med.Table <- palCcapClassDf
    palustrineMappedPixels.CIs.Med.Table["mappedPixelCount"] <- c(t(palustrineMappedPixels.CIs[2,]))
    palustrineMappedPixels.CIs.Med.Table["mappedPixelCountSD"] <- rep(NA, nrow(palustrineMappedPixels.CIs.Med.Table))
    
    # then move on to CCAP 2010 Class Accuracy
    for (i in 1:ncol(ccap2010perPixelScalers.CIs)) {
      ccap2010perPixelScalers.Mins <- ccap2010perPixelScalers.CIs[2,]
      ccap2010perPixelScalers.Mins[1, i] <-  ccap2010perPixelScalers.CIs[1,i]
      
      ccap2010perPixelScalers.Maxs <- ccap2010perPixelScalers.CIs[2,]
      ccap2010perPixelScalers.Maxs[1, i] <-  ccap2010perPixelScalers.CIs[3,i]
      
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.Mins, 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.Maxs, 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      parameterNameStore <- c(parameterNameStore, paste(colnames(ccap2010perPixelScalers.CIs)[i], " accuracy", sep=""))
      parameterTypeStore <- c(parameterTypeStore, "C-CAP Classification")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
      
      
    }
    
    # move on to CNC Accuracy
    for (i in 1:ncol(cncPerPixelScalers.CIs)) {
      
      cncPerPixelScalers.Mins <- cncPerPixelScalers.CIs[2,]
      cncPerPixelScalers.Mins[1, i] <- cncPerPixelScalers.CIs[1,i]
      
      cncPerPixelScalers.Maxs <- cncPerPixelScalers.CIs[2,]
      cncPerPixelScalers.Maxs[1, i] <- cncPerPixelScalers.CIs[3,i]
      
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.Mins,
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.Maxs,
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      parameterNameStore <- c(parameterNameStore, paste(gsub("\\.", " ", colnames(cncPerPixelScalers.CIs)[i]), " accuracy", sep=""))
      parameterTypeStore <- c(parameterTypeStore, "C-CAP Change Detection")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    }
    
    # move on to burial / emissions factors
    for (i in 1:(ncol(storageAndEmissions.CIs)-2)) {
      
      storageAndEmissions.Mins <- storageAndEmissions.CIs[2,]
      storageAndEmissions.Mins[1, i] <- storageAndEmissions.CIs[1,i]
      
      storageAndEmissions.Maxs <- storageAndEmissions.CIs[2,]
      storageAndEmissions.Maxs[1, i] <- storageAndEmissions.CIs[3,i]
      
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.Mins)[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.Maxs)[[1]]
      
      parameterNameStore <- c(parameterNameStore, gsub("\\.", " ", colnames(storageAndEmissions.CIs)[i]))
      parameterTypeStore <- c(parameterTypeStore, "Emissions and Storage Data")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    }
    
    # Test a Few Key Assumptions.
    # First compare calculating Palustrine Wetland Mapped Area Based on Coastal Lands
    {
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels, 
                                 palustrineMappedPixels = palustrineMappedPixels.NWI, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.Maxs)[[1]]
      
      parameterNameStore <- c(parameterNameStore, "coastal lands vs NWI")
      parameterTypeStore <- c(parameterTypeStore, "Assumption")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    }
    
    # Second compare calculating calculating methane based on Sustained Global Warming/Cooling Potential vs GWP
    {
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,], 
                                 gwp=T)
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,], 
                                 gwp=F)
      parameterNameStore <- c(parameterNameStore, "GWP vs SGW/CP")
      parameterTypeStore <- c(parameterTypeStore, "Assumption")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate[[1]] - minEstimate[[1]]))
      
      # let's make this a special case where we save the resutls
      minEstimate[[2]]["analysis_type"] <- rep("GWP", nrow(minEstimate[[2]])) 
      maxEstimate[[2]]["analysis_type"] <- rep("SGW/CP", nrow(maxEstimate[[2]])) 
      sgwp_gwp_output <- rbind(minEstimate[[2]], maxEstimate[[2]])
      write_csv(sgwp_gwp_output, "data/outputTables/GWP_vs_SGWP_median_values.csv")
      
    }
    
    # Third and Forth Compare The Assumptions of depth lost and fraction lost to open water
    {
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,],
                                 fraction.loss = 0.5+0.00625)[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,],
                                 fraction.loss = 0.75-0.00625)[[1]]
      
      parameterNameStore <- c(parameterNameStore, "fraction lost to atmosphere")
      parameterTypeStore <- c(parameterTypeStore, "Assumption")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    }
    
    # Test the depth lost to erosion
    {
      minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,],
                                 depth.lost = 0.5+0.0375)[[1]]
      
      maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                                 palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                                 ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                                 cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                                 storageAndEmissions = storageAndEmissions.CIs[2,],
                                 depth.lost = 1.5-0.0375)[[1]]
      
      parameterNameStore <- c(parameterNameStore, "depth lost to erosion")
      parameterTypeStore <- c(parameterTypeStore, "Assumption")
      parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    }
    
    sensitivityAnalysisDF <- data.frame(parameter = parameterNameStore, 
                                        type = parameterTypeStore, 
                                        effectTonnesCO2 = parameterEffectStore)
    sensitivityAnalysisDF <- sensitivityAnalysisDF[order(-sensitivityAnalysisDF$effectTonnesCO2), ]
    
    sensitivityAnalysisDF["effectMillionTonnesCO2"] <- round(sensitivityAnalysisDF$effectTonnesCO2 * 1E-6, 2)
    return(sensitivityAnalysisDF)
  }
  
  # run the sensitivity analysis function
  sensitivity_outputs_means <- run_coastalNGGI_sensitivity(estuarineMappedPixels = estuarineMappedPixels.1,
                                                           palustrineMappedPixels.savedIterations = palustrineMappedPixels.savedIterations,
                                                           palustrineMappedPixels.NWI = palustrineMappedPixels.1.NWI,
                                                           ccap2010perPixelScalers.savedIterations = ccap2010perPixelScalers.savedIterations,
                                                           cncPerPixelScalers.savedIterations = cncPerPixelScalers.savedIterations,
                                                           storageAndEmissions.savedIterations = storageAndEmissions.savedIterations)
  
  # output resulting table to data folder
  write_csv(sensitivity_outputs_means, "data/outputTables/sensitivityAnalysisResults/national/sensitivityAnalysisResults.csv") 
}

# run 10,000 iterations on the 'regional' 
#  setting for mapping excercise and comparison later.
{
  # define number of iterations
  iterations <- 10000
  
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  registerDoSNOW(cl)
  
  # I looked up how to make progress bars in parallel on Stack Exchange
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # copied this solution from stack exchange 
  # https://stackoverflow.com/questions/27279164/output-list-of-two-rbinded-data-frames-with-foreach-in-r
  # takes an arbitrary number of lists x all of which much have the same structure    
  comb <- function(x, ...) {  
    mapply(rbind,x,...,SIMPLIFY=FALSE)
  }
  
  finalDataframe <- foreach(k=1:iterations, .combine = 'comb', .multicombine=TRUE,
                            .options.snow = opts) %dopar% {
                              tempDF_list <- run_randomized_coastalNGGI(iterationCode = k,
                                                                        scopeOfAnalysis = "regional") #calling a function
                              #Equivalent to outputDF = rbind(outputDF, tempDF)
                              tempDF_list
                            }
  
  #stop cluster
  stopCluster(cl)
  
  # write to file
  write_csv(finalDataframe[[1]], "data/outputTables/MonteCarloResults/regional/palustrineMappedPixels.savedIterations.csv")
  write_csv(finalDataframe[[2]], "data/outputTables/MonteCarloResults/regional/ccap2010perPixelScalers.savedIterations.csv")
  write_csv(finalDataframe[[3]], "data/outputTables/MonteCarloResults/regional/cncPerPixelScalers.savedIterations.csv")
  write_csv(finalDataframe[[4]], "data/outputTables/MonteCarloResults/regional/storageAndEmissions.savedIterations.csv")
  write_csv(finalDataframe[[5]], "data/outputTables/MonteCarloResults/regional/total.savedIterations.csv")
}

# ... then run sensitivity analysis.
{
  # load up tables from the uncertaitny analysis so they can be inputs for the sensitivity analysis
  palustrineMappedPixels.savedIterations <- read.csv("data/outputTables/MonteCarloResults/regional/palustrineMappedPixels.savedIterations.csv")
  ccap2010perPixelScalers.savedIterations <- read.csv("data/outputTables/MonteCarloResults/regional/ccap2010perPixelScalers.savedIterations.csv")
  cncPerPixelScalers.savedIterations <- read.csv("data/outputTables/MonteCarloResults/regional/cncPerPixelScalers.savedIterations.csv")
  storageAndEmissions.savedIterations <- read.csv("data/outputTables/MonteCarloResults/regional/storageAndEmissions.savedIterations.csv")
  
  # run the sensitivity analysis function
  sensitivity_outputs_medians<- run_coastalNGGI_sensitivity(estuarineMappedPixels = estuarineMappedPixels.1,
                                                            palustrineMappedPixels.savedIterations = palustrineMappedPixels.savedIterations,
                                                            palustrineMappedPixels.NWI = palustrineMappedPixels.1.NWI,
                                                            ccap2010perPixelScalers.savedIterations = ccap2010perPixelScalers.savedIterations,
                                                            cncPerPixelScalers.savedIterations = cncPerPixelScalers.savedIterations,
                                                            storageAndEmissions.savedIterations = storageAndEmissions.savedIterations)
  
  # output resulting table to data folder
  write_csv(sensitivity_outputs_medians, "data/outputTables/sensitivityAnalysisResults/regional/sensitivityAnalysisResults.csv")
}
