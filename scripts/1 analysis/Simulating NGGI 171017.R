# this will simulate the NGGI Based on Available Data
set.seed(5) # set seed so that analyses are replicable 

# load necessary packages 
{
  library(foreign) # to read .dbf files
  library(MASS) # to create multivariate normal distribution
  library(tidyverse) # to run data plyr and ggplot operations
  library(truncnorm) # to create truncated normal distributions
  library(gridExtra) # to combine ggplot graphs
}

# Define Important Conversion Factors
{
  gramsPerKg <- 1000 #1000 grams per kg
  m2PerHa <- 10000 # 10,000 meters squared per hectare
  millionHaPerHa <- 1E6 # 1 million hectare per hectare
  carbonPerBiomass <- 0.441 # carbon per biomass according to Byrd et al., 2018 synthesis
  gramsPerPetagram <-1E15 # grams per petagram
  m2perPixel <- 900 # meters sequred per 30 x 30 meter pixel
  carbonToCO2 <- 3.666667 # moles carbon coverts to moles CO2
}

# load data files from data folder
{
  # all CCAP data from 2006 to 2010 for getting estuarine subcategory pixel counts
  ccap_fullTab <- read.dbf("data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf")
  
  # load ccap accuracy assesment data so we can simulate uncertainty due to accuracy
  ccap_aa <- read_csv("data/WetlandArea/CCAP/2010Classes/CCAP2010AccuracyAssessment.csv")
  ccap_aa <- as.matrix(ccap_aa[, 2:ncol(ccap_aa)])
  rownames(ccap_aa) <- colnames(ccap_aa)
  ccap_tab <- read.csv("data/WetlandArea/CCAP/2010Classes/ccapPixelCounts.csv")
  ccap_area <- ccap_tab$pixels
  
  # same as above for change no change
  cnc_aa <- read_csv("data/WetlandArea/CCAP/CNC/CCAP06to10ChangeNoChangeAccuracyAssesment.csv")
  cnc_aa <- as.matrix(cnc_aa[, 2:ncol(cnc_aa)])
  rownames(cnc_aa) <- colnames(cnc_aa)
  
  cnc_tab <- read.csv("data/WetlandArea/CCAP/CNC/cncPixelCounts.csv")
  cnc_area <- cnc_tab$pixels
  
  # input table from raster dataset of palustrine under NWI assumption
  palustrineNwi <- read.dbf("data/WetlandArea/Palustrine/nwi/CCAP2006to2010_wTab_PalMaskedByNwi.dbf")
  
  # define file path for the palustrine under coastal lands assumption
  palustrineFilePath <- "data/WetlandArea/Palustrine/coastalLands/PalustrinePixelCounts/_AllCONUS/tables"
  
  # Soils Data Summary from Holmquist et al., In Review
  soilCarbonMean_gCcm3 <- 0.027 
  soilCarbonSd_gCcm3 <- 0.013
  soilCarbonN <- 8280
  
  # load CAR data
  car <- read.csv("data/MengReview/PbandcsData_170926.csv")
  
  # Biomass Values from Kristen's Byrd's Remote Sensing Calibraiton dataset ...
  # ... as well as multiple other sources for scrub/shrub and biomass
  biomass <- read_csv("data/Biomass/biomass_gCO2_distribution_summaries.csv")
  
  # Load Sara's Methane Data
  methane <- read.csv("data/Methane/derivative/Methane Synthesis Knox.csv")
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
}

# Define assumptions re: land cover classes
{
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Snow/Ice')
  
  palustrineWetlands <- c('Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland')
  
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
                      'Cultivated', 'Pasture/Hay',
                      'Unconsolidated Shore', 'Water',
                      'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')
  
  forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
                 'Palustrine Forested Wetland',
                 'Estuarine Forested Wetland'
                 )
  
  scrubShrubVeg <- c('Scrub/Shrub',
             'Palustrine Scrub/Shrub Wetland',
             'Estuarine Scrub/Shrub Wetland'
             )
  
  emergentVeg <- c('Cultivated', 'Pasture/Hay', 
             'Grassland',
             'Palustrine Emergent Wetland',
             'Estuarine Emergent Wetland'
             )
  
  nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
             'Unconsolidated Shore', 'Bare Land', 'Water', 
             'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
             'Tundra',
             'Snow/Ice'
             )
  
  classify_by_salinity <- function(class_time1 = "Estuarine Emergent Wetland", class_time2 = "Open Water") {
    if (class_time1 %in% estuarineWetlands | class_time2 %in% estuarineWetlands) {
      salinity_class <- "Estuarine"
    } else if (class_time1 %in% palustrineWetlands | class_time2 %in% palustrineWetlands) {
      salinity_class <- "Palustrine"
    } else {
      salinity_class <- NA
    }
    return(salinity_class)
  }
  
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
      if ((classOrder[m] %in% c(palustrineWetlands, estuarineWetlands)) | (classOrder[n] %in% c(palustrineWetlands, estuarineWetlands))) {
        class2006ToAnalyse <- c(class2006ToAnalyse, classOrder[m])
        abbrev2006ToAnalyse <- c(abbrev2006ToAnalyse, abbrevs[m])
        class2010ToAnalyse <- c(class2010ToAnalyse, classOrder[n])
        abbrev2010ToAnalyse <- c(abbrev2010ToAnalyse, abbrevs[n])
        classesToAnalyse <- c(classesToAnalyse, paste(classOrder[m], " to ", classOrder[n], sep=""))
        abbreviationsToAnalyse <- c(abbreviationsToAnalyse, paste(abbrevs[m], "_", abbrevs[n], sep=""))
        if ((classOrder[m] %in% estuarineWetlands) | (classOrder[n] %in% estuarineWetlands)) { areaVariableType <- c(areaVariableType, "fixed") }
        else { areaVariableType <- c(areaVariableType, "random") } 
      }
    }
  }
  ccapClassDf <- data.frame(class_2006 = class2006ToAnalyse, abbrev_2006 = abbrev2006ToAnalyse,
                            class_2010 = class2010ToAnalyse, abbrev_2010 = abbrev2010ToAnalyse,
                            class = classesToAnalyse, abbrev = abbreviationsToAnalyse, variableType = areaVariableType)
  
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
    palustrineNWIClassPixelCounts <- c(palustrineNWIClassPixelCounts, classPixelCount)
  }
  palCcapClassDf_NWI["mappedPixelCount"] <- palustrineNWIClassPixelCounts
  palCcapClassDf_NWI["variableType"] <- rep("fixed", nrow(palCcapClassDf_NWI))
}

# Prep the Soil Carbon Data and convert to gCO2 eq. per m2 per year
{
  # convert to gCO2 per m2
  # convert to gCO2 per cm3
  cm.mean <- soilCarbonMean_gCcm3 * carbonToCO2 * 100 * 10000 
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
  # CAR is log normally distrbuted because it can't be negative and has a long tail
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
  biomass.em.log.mean <- biomass$logmean[biomass$vegetation_class == "emergent"]
  biomass.em.log.sd <-  biomass$logsd[biomass$vegetation_class == "emergent"]
  biomass.em.n <-  biomass$n[biomass$vegetation_class == "emergent"]
  
  biomass.ss.log.mean <-  biomass$logmean[biomass$vegetation_class == "shrub"]
  biomass.ss.log.sd <-  biomass$logsd[biomass$vegetation_class == "shrub"]
  biomass.ss.n <-  biomass$n[biomass$vegetation_class == "shrub"]
  
  biomass.fo.log.mean <- biomass$logmean[biomass$vegetation_class == "forest"]
  biomass.fo.log.sd <- biomass$logsd[biomass$vegetation_class == "forest"]
  biomass.fo.n <- biomass$n[biomass$vegetation_class == "forest"]
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
  ch4_co2_eq_sgwp <- mapply(generateMethaneSGPs, methane$CH4.flux) # Calculate CO2 equivalents
  methane["ch4_co2_eq_sgwp"] <- ch4_co2_eq_sgwp # add to the dataframe so they can be sorted
  
  ch4_co2_eq_gwp <- mapply(generateMethaneGPs, methane$CH4.flux) # Calculate CO2 equivalents
  methane["ch4_co2_eq_gwp"] <- ch4_co2_eq_gwp # add to the dataframe so they can be sorted
  
  # Estuarine Emissions Factors are Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  est.ch4 <- subset(methane, Salinity.ppt >= 5)
  methane.est.n <- length(est.ch4$ch4_co2_eq_sgwp)
  methane.est.mean.sgwp <- mean(est.ch4$ch4_co2_eq_sgwp)
  methane.est.sd.sgwp <- sd(est.ch4$ch4_co2_eq_sgwp)
  
  methane.est.mean.gwp <- mean(est.ch4$ch4_co2_eq_gwp)
  methane.est.sd.gwp <- sd(est.ch4$ch4_co2_eq_gwp)
  
  # Palustrine Emissions Factors are Log Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  pal.ch4 <- subset(methane, Salinity.ppt <5)
  methane.pal.n <- length(pal.ch4$ch4_co2_eq_sgwp)
  
  methane.pal.log.mean.sgwp <- mean(log(pal.ch4$ch4_co2_eq_sgwp))
  methane.pal.log.sd.sgwp <- sd(log(pal.ch4$ch4_co2_eq_sgwp))
  
  methane.pal.log.mean.gwp <- mean(log(pal.ch4$ch4_co2_eq_gwp))
  methane.pal.log.sd.gwp <- sd(log(pal.ch4$ch4_co2_eq_gwp))
}

# Functions for Accuracy Assesment and Unbiased Area Estimation from Oloffson et al., 2014
{
  
  propAreaCalc <- function(input_area) {return(input_area / sum(as.numeric(input_area))) } # simply calculates proportional area
  
  propAreaMatrix <-function(input_matrix, input_areas) { # makes a matrix the proportional area matrix Eq. 4
    input_pAreas <- propAreaCalc(input_areas) # proportional area instead of raw area
    n_rowSums <- rowSums(input_matrix) # counts per map class
    areaPropMatrix <- (input_matrix / n_rowSums) * input_pAreas # proportional counts multiplied by proportional area of the mapped class
    return(areaPropMatrix)
  }
  
  u_accuracy <- function(input_matrix, input_areas) { # function for user's accuracy given a confusion matrix. Eq. 2.
    # create proportional area matrix
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) # calculate confusion matrix as proportional area rather than n
    ua_output <- c()
    q <- nrow(tempPropAreaMatrix) # number of classes
    for (i in 1:q) { # for each class
      ua_temp <- tempPropAreaMatrix[i,i] / sum(as.numeric(tempPropAreaMatrix[i,])) # correct classifications divided by row sum
      ua_output <-c(ua_output, ua_temp)
    }
    var <- c()
    for (i in 1:length(ua_output)) { # for each map class
      temp_var <- (ua_output[i] * (1-ua_output[i])) / (sum(as.numeric(input_matrix[i,])) - 1) # Eq. 6
      var <- c(var, temp_var)
    }
    se <- sqrt(var)
    ci <- 1.96 * se
    out_df <- data.frame(user_accuracy = ua_output, se = se , ci = ci)
    rownames(out_df) <- row.names(input_matrix)
    return(out_df)
  }
  
  p_accuracy <- function(input_matrix, input_areas) { # function for producer's accuracy given a confusion matrix. Eq. 3.
    # create proportional area matrix
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) # calculate confusion matrix as proportional area rather than n
    pa_output <- c()
    q <- nrow(tempPropAreaMatrix)
    for (j in 1:q) { # for each reference class in q classes
      pa_temp <- tempPropAreaMatrix[j,j] / sum(as.numeric(tempPropAreaMatrix[,j])) # correct classifications divided by column summary
      pa_output <-c(pa_output, pa_temp)
    }
    
    # variance from Eq. 7.
    user_accuracies <- u_accuracy(input_matrix, input_areas)$user_accuracy # need UA to calculate variance in PA
    pixel_matrix <- tempPropAreaMatrix * sum(as.numeric(input_areas)) # matrix of n pixels in confusion matrix
    var <- c()
    for (j in 1:length(pa_output)) { # for each reference class
      
      Ndotj <- sum(as.numeric(pixel_matrix[,j])) # Ndotj : the estimated number of pixels in reference class j. (column sum of the 'pixel matrix')
      Njdot <- input_areas[j] # number of pixels in map class j : from input areas data
      njdot <- sum(as.numeric(input_matrix[j,])) # number of sampling units in map class j : from input matrix
      
      exp1 <-  (input_areas[j]^2) * ((1 - pa_output[j]) ^2) * user_accuracies[j] * (1 - user_accuracies[j]) / (sum(as.numeric(input_matrix[,j])) - 1)
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
  
  o_accuracy <- function(input_matrix, input_areas) { # function for total agreement given a confusion matrix. Eq. 1.
    input_pAreas <- propAreaCalc(input_areas) # calculate the proportional areas
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) # calculate confusion matrix as proportional area rather than n
    overall_accuracy = (sum(as.numeric(diag(tempPropAreaMatrix)))) # overall accuracy
    
    # variance
    user_accuracies <- u_accuracy(input_matrix, input_areas)$user_accuracy # calculate users accuracies
    q <- nrow(tempPropAreaMatrix) # number of classes
    step2s <- c() # empty vector for storing results from classes
    for (i in 1:q) { # for each class
      step1 <- input_pAreas[i]^2 * user_accuracies[i] * (1 - user_accuracies[i]) # eq. 5 TOP Wi^2 * Ui * (1-Ui)
      step2 <- step1 / (sum(as.numeric(input_matrix[i,])) - 1) # eq. 5 BOTTOM: step1 / (ni. - 1) row sum
      step2s <- c(step2s, step2)
    }
    var <- sum(step2s) # eq. 5 outer Sum for variance
    se <- sqrt(var) # se = sqrt(var)
    ci <- 1.96 * se # ci = 1.96 * se
    
    return(data.frame(overall_accuracy = overall_accuracy, se = se, ci = ci)) # the sum of the diagonal for a proportional area confusion matrix
  }
  
  areaCorrections <- function(input_matrix, input_areas) {
    
    input_pAreas <- propAreaCalc(input_areas) # calculate the proportional areas
    tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) # calculate confusion matrix as proportional area rather than n
    correctedAreaProps <- colSums(tempPropAreaMatrix) # Eq. 9: the column sums are the estimated area proportions, as opposed to the row sums which are the mapped area proportions
    
    se <-c() # empty vector for se
    for (k in 1:nrow(input_matrix)) { # for each class
      step2s <- c()
      for (i in 1:nrow(input_matrix)) { # for each row in the confusion matrix
        se_step1 = (input_pAreas[i] * tempPropAreaMatrix[i,k]) - (tempPropAreaMatrix[i,k] ^ 2) # (Wi * pik) - pik^2: Eq. 10 TOP
        se_step2 = se_step1 / (sum(as.numeric(input_matrix[i,])) - 1 ) # step 1 / (ni. - 1): Eq. 10 BOTTOM
        step2s <- c(step2s, se_step2) # store row output
      }
      se_step3 <- sqrt(sum(as.numeric(step2s))) # square root of the sum of the rows: Eq. 10: OUTER
      se <-c(se, se_step3)
    }
    ci = se * 1.96 # CI from SE standard estimation formula
    
    # In Eq. 11 Oloffson et al. apply the estimated area proportions and the CI by to the total map area to calculate estimated area
    # We do something slightly different that works out to be the same in the end
    # Because we are interested in propegating uncertainty at the pixel level we need to know two things
    # 1. If a pixel is present do we need to scale up or scale down based on inclusion / exclusion probabilities of the mapped class
    # 2. We need to know what the CIs are on that 'scaler'
    perPixelScaler <- (correctedAreaProps / rowSums(tempPropAreaMatrix)) # per pixel scaler is the ratio of the estimated occurence to the mapped occurence
    scalerSE <- se / rowSums(tempPropAreaMatrix) # se calculated for scaler
    scalerCI <- ci / rowSums(tempPropAreaMatrix) # ci calculated for scaler
    
    estimatedArea <- perPixelScaler * input_areas # calculate estimated area using input class area and pixel based scaler
    estimatedAreaSE <- input_areas * scalerSE # se
    estimatedAreaCI <- input_areas * scalerCI # ci
    
    return(data.frame(perPixelScaler = perPixelScaler, scalerSE = scalerSE, scalerCI = scalerCI, originalArea = input_areas, estimatedArea = estimatedArea, estimatedAreaSE = estimatedAreaSE, estimatedAreaCI = estimatedAreaCI))
  }
}

# Functions for Generating Areas
{
  # Generate Estimated to Mapped Ratios for 2010 Class or Change/No Change
  
  # simulate data by category
  simulateAA <- function(aa) {
    for (i in 1:ncol(aa)){
      class_i <- aa[,i]
      simmClass_i <- rmultinom(1, sum(class_i), class_i)
      if (i ==1){
        temp_df <- as.data.frame(simmClass_i)
      } else {
        temp_df <- cbind(temp_df, simmClass_i) 
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
  
  # Function for randomly generating table of palustrine wetland pixle counts based on binomial distribution normal approximation
  generatePalustrineAreaTableBNA <- function (normalApproxTable=palustrineNormalApproximations,
                                           joinTable=palCcapClassDf) {
    # generate 1 random normal value for each class
    palustrineMappedPixels <- mapply(rnorm, 1, normalApproxTable$mu, normalApproxTable$sigma)
    palustrineMappedPixels[palustrineMappedPixels<0] <- 0 # remove 0 values
    palustrineMappedPixels <- as.integer(palustrineMappedPixels) # convert to integer
    joinTable["mappedPixelCount"] <- palustrineMappedPixels
    return(joinTable)
  }
}

# Functions for Generating Soil CAR, Stock, Biomass, and CH4 Values
{
  # Soil Carbon Accumulation Rate is log normally distributed because it can't be negative and has a long positive tail
  generateLogNormalMeans <- function(x.n, x.log.mean, x.log.sd) { return(exp(mean(log(rlnorm(x.n, x.log.mean, x.log.sd)))) ) }
  
  generateNormalMeans <- function(x.n, x.mean, x.sd) { return(mean(rnorm(x.n, x.mean, x.sd))) }
  
  generateTruncatedNormalMeans <- function(x.n, x.mean, x.sd, lowest_value = 0) { return(mean(rtruncnorm(n=x.n, a=lowest_value,
                                                                                                    mean = x.mean, sd=x.sd))) }
  
  generateDepthLost <- function(depth.min = 0.5, depth.max=1.5) { return(runif(1, depth.min, depth.max)) } 
  
  generateSoilEmissionsFactor <- function(cMassTable, depthIntervalLost) {
    return(sum(colMeans(cMassTable[,1:depthIntervalLost], na.rm = T)))
  }
}

# create some initial (.1) data.frames with just means for all mapped area, area scale factors, and emissions/storage factors
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
  
  storageAndEmissions.1 = data.frame(soil.burial = exp(pb.log.mean),
                                     carbon.density = cm.mean,
                                     depth.lost = 1, fraction.loss = 0.625,
                                     biomass.em = exp(biomass.em.log.mean), biomass.ss = exp(biomass.ss.log.mean), biomass.fo = exp(biomass.fo.log.mean),
                                     methane.est = methane.est.mean.gwp, methane.pal = exp(methane.pal.log.mean.gwp),
                                     methane.est.sgwp = methane.est.mean.sgwp, methane.pal.sgwp = exp(methane.pal.log.mean.sgwp)
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
        # Multiply number of mapped pixels by the estimated to mapped ratio for the 2010 class 
        mapped.pixel.n = wetlandMappedPixels$mappedPixelCount[i]
        estimated.pixel.n = mapped.pixel.n * ccap2010perPixelScalers[, abbrev.t2]
        # Multiply number of mapped pixels by the estimated to mapped ratio for the 2006-2010 change class 
        if (class.t1 == class.t2) { # if there's no change scale by the 'no change' estimated to mapped ratio 
          estimated.pixel.n = mapped.pixel.n * cncPerPixelScalers$No.Change[1]
        } else { # if there's a change scale number of pixels by the 'change' estimated to mapped ratio
          estimated.pixel.n = estimated.pixel.n * cncPerPixelScalers$Change[1]
        }
      }
      
      # calculate soil change
      {
        # is it a wetland to wetland transition (wetland at time 1 and wetland at time 2)
        if ((class.t1 %in% c(estuarineWetlands, palustrineWetlands)) & (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
          soil.change =  storageAndEmissions$soil.burial[1] * 5 # if it's a wetland remaining wetland then it buries carbon for 4 years
        } else {
          # is it a wetland restoration (non wetland at time 1 to wetland at time 2)
          if ((! (class.t1 %in% c(estuarineWetlands, palustrineWetlands))) & (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
            soil.change =  storageAndEmissions$soil.burial[1] * 2.5 # if a class goes from not wetland to wetland we assume it starts buring carbon half way through the time step
          } else { # is it a wetland loss
            if (class.t2 %in% soilLossEvents) { # is it a soil loss event, defined in the 'assumptions section' near the beginning of the code? 
              soil.change = -(storageAndEmissions$carbon.density * storageAndEmissions$depth.lost * storageAndEmissions$fraction.loss) # if this is a soil loss even, then the whole column is lost at once
            } else {
              soil.change  = storageAndEmissions$soil.burial[1] * 2.5 # if it is a wetland loss without soil loss, then we assume it stops buring carbon halfway through the timestep 
            }
          }
        }
      }
      
      # calculate biomass change
      {  
        # what was the biomass at time step 1?
        if (class.t1 %in% emergentVeg) {
          biomass.t1 <- storageAndEmissions$biomass.em[1]
        } else if (class.t1 %in% scrubShrubVeg) {
          biomass.t1 <- storageAndEmissions$biomass.ss[1]
        } else if (class.t1 %in% forestVeg) {
          biomass.t1 <- storageAndEmissions$biomass.fo[1]
        } else if (class.t1 %in% nonVeg)  {
          biomass.t1 <- 0
        } else {
          biomass.t1 <- 0
        }
          
        # what was the biomass in at time step 2?
        if (class.t2 %in% emergentVeg) {
          biomass.t2 <- storageAndEmissions$biomass.em[1]
        } else if (class.t2 %in% scrubShrubVeg) {
          biomass.t2 <- storageAndEmissions$biomass.ss[1]
        } else if (class.t2 %in% forestVeg) {
          biomass.t2 <- storageAndEmissions$biomass.fo[1]
        } else if (class.t2 %in% nonVeg)  {
          biomass.t2 <- 0
        } else {
          biomass.t2 <- 0
        }
        # year 2 - year 1
        biomass.change = biomass.t2 - biomass.t1
      }
      
      if (gwp == T) {
        methane.est <- storageAndEmissions$methane.est
        methane.pal <- storageAndEmissions$methane.pal
      } else {
        methane.est <- storageAndEmissions$methane.est.sgwp
        methane.pal <- storageAndEmissions$methane.pal.sgwp
      }
  
      # calculate methane emissions
      {  
        # What were the emissions at the start?
        if (class.t1 %in% estuarineWetlands) {
          methane.t1 <- methane.est
        } else if (class.t1 %in% palustrineWetlands) {
          methane.t1 <- methane.pal
        } else {
          methane.t1 <- 0
        }
        
        if (class.t2 %in% estuarineWetlands) {
          methane.t2 <- methane.est
        } else if (class.t2 %in% palustrineWetlands) {
          methane.t2 <- methane.pal
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
    total_df["total_gCO2perM2"] <- (soil_gCO2perm2_vect + biomass_gCO2perm2_vect + methane_gCO2perm2_vect)
    total_df["soil_gCO2perM2"] <- soil_gCO2perm2_vect
    total_df["biomass_gCO2perM2"] <- biomass_gCO2perm2_vect
    total_df["methane_gCO2perM2"] <- methane_gCO2perm2_vect
    
    total_million_tonnesCO2 <- sum((total_df$estimated_pixel_count * total_df$total_gCO2perM2 * 900 / 1E6)
                                   , na.rm=T)
    
    detailed_output <- list(total_million_tonnesCO2, total_df)
    
    return(detailed_output)
  }
}

# Run 1000 iterations of the Inventory with Varying Inputs for Uncertainty Analysis
{
  # Define the number of iterations
  n.iterations = 1000
  
  # need a place to save the output files
  
  pb <- txtProgressBar(0, n.iterations) # establish progress bar
  
  # load estuarine mapped pixels
  estuarineMappedPixels.fixedVariables <- estCcapClassDf # these are fixed variables so we use the same one every time
  
  for (i in 1:n.iterations){
    setTxtProgressBar(pb, i) # forward progress bar
    
    # Create Input Tables by randomly drawing from the data's probability distributions  
    # random draws for area mapping
    palustrineMappedPixels.randomDraw <- generatePalustrineAreaTableBNA()
    ccap2010perPixelScalers.randomDraw <- simulatePerPixelScalers(ccap_aa, ccap_area)
    cncPerPixelScalers.randomDraw <- simulatePerPixelScalers(cnc_aa, cnc_area)
    
    # random draws for soil carbon emissions/storage factors
    soil.burial.randomDraw <- generateLogNormalMeans(pb.n, pb.log.mean, pb.log.sd)
    depth.lost.randomDraw <- generateDepthLost()
    carbon.density.randomDraw <- generateTruncatedNormalMeans(cm.n, cm.mean, cm.sd)
    fraction.loss.randomDraw <- runif(1, 0.5, 0.75)
    
    # random draws for biomass emissions/storage factors
    biomass.em.randomDraw <- generateLogNormalMeans(biomass.em.n, biomass.em.log.mean, biomass.em.log.sd)
    biomass.ss.randomDraw <- generateLogNormalMeans(biomass.ss.n, biomass.ss.log.mean, biomass.ss.log.sd)
    biomass.fo.randomDraw <- generateLogNormalMeans(biomass.fo.n, biomass.fo.log.mean, biomass.fo.log.sd)
    
    # random draws for methane emissions factors
    methane.est.randomDraw <- generateNormalMeans(methane.est.n, methane.est.mean.gwp, methane.est.sd.gwp)
    methane.pal.randomDraw <- generateLogNormalMeans(methane.pal.n, methane.pal.log.mean.gwp, methane.pal.log.sd.gwp)
    
    methane.est.sgwp.randomDraw <- generateNormalMeans(methane.est.n, methane.est.mean.sgwp, methane.est.sd.sgwp)
    methane.pal.sgwp.randomDraw <- generateLogNormalMeans(methane.pal.n, methane.pal.log.mean.sgwp, methane.pal.log.sd.sgwp)
    
    storageAndEmissions.randomDraw = data.frame(soil.burial = soil.burial.randomDraw,
                                                carbon.density = carbon.density.randomDraw, depth.lost = depth.lost.randomDraw, fraction.loss = fraction.loss.randomDraw, 
                                                biomass.em = biomass.em.randomDraw, biomass.ss = biomass.ss.randomDraw, biomass.fo = biomass.fo.randomDraw,
                                                methane.est = methane.est.randomDraw, methane.pal = methane.pal.randomDraw,
                                                methane.est.sgwp = methane.est.sgwp.randomDraw, methane.pal.sgwp = methane.pal.sgwp.randomDraw)
    
    # Run NGGI Function
    coastalNGGI.randomDraw <- coastalNGGI(estuarineMappedPixels=estuarineMappedPixels.fixedVariables,
                                          palustrineMappedPixels=palustrineMappedPixels.randomDraw,
                                          ccap2010perPixelScalers=ccap2010perPixelScalers.randomDraw,
                                          cncPerPixelScalers=cncPerPixelScalers.randomDraw,
                                          storageAndEmissions=storageAndEmissions.randomDraw
                                          )[[2]]
    
    coastalNGGI.randomDraw["iteration_number"] <- rep(i, nrow(coastalNGGI.randomDraw))
    
    # if it's the first iteration establish the output data.tables
    if (i == 1) {
      palustrineMappedPixels.savedIterations <- as.data.frame(t(as.matrix(palustrineMappedPixels.randomDraw$mappedPixelCount)))
      colnames(palustrineMappedPixels.savedIterations) <- palustrineMappedPixels.randomDraw$abbrev
      ccap2010perPixelScalers.savedIterations <- ccap2010perPixelScalers.randomDraw
      cncPerPixelScalers.savedIterations <- cncPerPixelScalers.randomDraw
      storageAndEmissions.savedIterations <- storageAndEmissions.randomDraw
      
      total_df.saved.iterations <- coastalNGGI.randomDraw
      
    } else { # if it's after one add the inputs and outputs as a row in the saved data frames
      palustrineMappedPixels.savedIterations <- rbind(palustrineMappedPixels.savedIterations, palustrineMappedPixels.randomDraw$mappedPixelCount)
      ccap2010perPixelScalers.savedIterations <- rbind(ccap2010perPixelScalers.savedIterations, ccap2010perPixelScalers.randomDraw)
      cncPerPixelScalers.savedIterations <- rbind(cncPerPixelScalers.savedIterations, cncPerPixelScalers.randomDraw)
      storageAndEmissions.savedIterations <- rbind(storageAndEmissions.savedIterations, storageAndEmissions.randomDraw)
      
      total_df.saved.iterations <- rbind(total_df.saved.iterations, coastalNGGI.randomDraw)

    }
    if (i == n.iterations) {
      print("Done!")
      write.table(palustrineMappedPixels.savedIterations, "data/outputTables/MonteCarloResults1/palustrineMappedPixels.savedIterations.csv", sep=",", row.names = F)
      write.table(ccap2010perPixelScalers.savedIterations, "data/outputTables/MonteCarloResults1/ccap2010perPixelScalers.savedIterations.csv", sep=",", row.names = F)
      write.table(cncPerPixelScalers.savedIterations, "data/outputTables/MonteCarloResults1/cncPerPixelScalers.savedIterations.csv", sep=",", row.names = F)
      write.table(storageAndEmissions.savedIterations, "data/outputTables/MonteCarloResults1/storageAndEmissions.savedIterations.csv", sep=",", row.names = F)
      
      # total iterations
      write.table(total_df.saved.iterations, "data/outputTables/MonteCarloResults1/total_saved_iterations.csv", sep=",", row.names = F)
       
    }
  }
}

# Summarise Total gCO2 per mapped pixel for joining to C-CAP table.
# Create and export summary data
{
  total_df.saved.iterations <- as_tibble(total_df.saved.iterations)
  sector_and_total_mapping_outputs <- total_df.saved.iterations %>%
    mutate(total_tonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6,
           soil_tonnesCO2perMappedPixel = soil_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6, 
           biomass_tonnesCO2perMappedPixel = biomass_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6,
           methane_tonnesCO2perMappedPixel = methane_gCO2perM2 * estimated_pixel_count / mappedPixelCount * 900 / 1E6) %>%
    group_by(class_2006, abbrev_2006, class_2010, abbrev_2010, class, abbrev) %>%
    summarise(median_total_tonnesCO2perMappedPixel  = median(total_tonnesCO2perMappedPixel, na.rm = T),
              lower_total_tonnesCO2perMappedPixel = quantile(total_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              upper_total_tonnesCO2perMappedPixel = quantile(total_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              uncertainty_total_tonnesCO2perMappedPixel = (upper_total_tonnesCO2perMappedPixel - lower_total_tonnesCO2perMappedPixel),
              median_soil_tonnesCO2perMappedPixel  = median(soil_tonnesCO2perMappedPixel, na.rm = T),
              lower_soil_tonnesCO2perMappedPixel = quantile(soil_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              upper_soil_tonnesCO2perMappedPixel = quantile(soil_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              uncertainty_soil_tonnesCO2perMappedPixel = (upper_soil_tonnesCO2perMappedPixel - lower_soil_tonnesCO2perMappedPixel),
              median_biomass_tonnesCO2perMappedPixel  = median(biomass_tonnesCO2perMappedPixel, na.rm = T),
              lower_biomass_tonnesCO2perMappedPixel = quantile(biomass_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              upper_biomass_tonnesCO2perMappedPixel = quantile(biomass_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              uncertainty_biomass_tonnesCO2perMappedPixel = (upper_biomass_tonnesCO2perMappedPixel - lower_biomass_tonnesCO2perMappedPixel),
              median_methane_tonnesCO2perMappedPixel  = median(methane_tonnesCO2perMappedPixel, na.rm = T),
              lower_methane_tonnesCO2perMappedPixel = quantile(methane_tonnesCO2perMappedPixel, 0.025, na.rm = T),
              upper_methane_tonnesCO2perMappedPixel = quantile(methane_tonnesCO2perMappedPixel, 0.975, na.rm = T),
              uncertainty_methane_tonnesCO2perMappedPixel = (upper_methane_tonnesCO2perMappedPixel - lower_methane_tonnesCO2perMappedPixel))

  write_csv(sector_and_total_mapping_outputs, "data/outputTables/MonteCarloResults1/sector_and_total_mapping_outputs.csv")
  
  # Total NGGI
  total_nggi_summed_iterations <- total_df.saved.iterations %>%
    mutate(analysis_description = "Total NGGI") %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(analysis_description, iteration_number) %>%
    summarise(sum_total_MillionTonnesCO2 = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T))
  write_csv(total_nggi_summed_iterations, "data/outputTables/MonteCarloResults1/total_nggi_summed_iterations.csv")
  
  total_nggi_summed_summaries <- total_nggi_summed_iterations %>%
    group_by(analysis_description) %>%
    summarise(median_sum_total_MillionTonnesCO2 = median(sum_total_MillionTonnesCO2),
           lower_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.025),
           upper_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.975))
  
  total_df.saved.iterations["salinity"] <- mapply(classify_by_salinity,
                                                  class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                  class_time2 = as.character(total_df.saved.iterations$class_2010))
  total_df.saved.iterations["stability"] <- mapply(classify_by_stability,
                                                  class_time1 = as.character(total_df.saved.iterations$class_2006),
                                                  class_time2 = as.character(total_df.saved.iterations$class_2010))
  total_df.saved.iterations["analysis_description"] <- mapply(paste, total_df.saved.iterations$salinity, " ", 
                                                            total_df.saved.iterations$stability, sep="")
  
  sum_total_MillionTonnesCO2_histograms <- ggplot(data = total_nggi_summed_iterations, aes(x = sum_total_MillionTonnesCO2)) +
    facet_grid(.~analysis_description) +
    geom_histogram(fill = "grey", color ="black") +
    geom_vline(color = "darkred", aes(xintercept = 0)) +
    theme_bw() + 
    xlab(expression(paste("Million Tonnes CO"[2],"e (+ storage and - emission)", sep="")))
  
  # NGGI by salinity and stability
  salinity_stability_summed_iterations <- total_df.saved.iterations %>%
    mutate(total_MillionTonnesCO2perMappedPixel = total_gCO2perM2 * estimated_pixel_count * 900 / 1E6 / 1E6) %>%
    group_by(salinity, stability, analysis_description, iteration_number) %>%
    summarise(sum_total_MillionTonnesCO2 = sum(total_MillionTonnesCO2perMappedPixel, na.rm=T))
  
  salinity_stability_summed_summaries <- salinity_stability_summed_iterations %>% 
    group_by(analysis_description) %>%
    summarise(median_sum_total_MillionTonnesCO2 = median(sum_total_MillionTonnesCO2),
              lower_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.025),
              upper_sum_total_MillionTonnesCO2 = quantile(sum_total_MillionTonnesCO2, 0.975))
  combined_sum_summaries <- rbind(total_nggi_summed_summaries, salinity_stability_summed_summaries)
  write_csv(combined_sum_summaries, "data/outputTables/MonteCarloResults1/combined_sum_summaries.csv")
  
  abline_df <- data.frame(x = c(0, 0, 0, 0), 
                          salinity = c("Estuarine", "Estuarine", "Palustrine", "Palustrine"),
                          stability = c("Stable and Gains", "Losses", "Stable and Gains", "Losses"))
  
  breakdown_MillionTonnesCO2_histograms <- ggplot(data = salinity_stability_summed_iterations, aes(x = sum_total_MillionTonnesCO2)) +
    facet_wrap(salinity~stability) +
    geom_histogram(fill = "grey", color ="black") +
    geom_vline(data=abline_df, color = "darkred", aes(xintercept = x)) +
    theme_bw() +
    xlab(expression(paste("Million Tonnes CO"[2],"e (+ storage and - emission)", sep="")))
  
  grid.arrange(breakdown_MillionTonnesCO2_histograms, sum_total_MillionTonnesCO2_histograms, nrow = 2, heights = c(6, 4))
}

# Re-run The Inventory 1 input-variable at a time for the sensitivity analysis
{ 
  # for all of the parameter (input) tables generate the median, and upper-lower (95% credible intervals)
  getColumnCIs <- function(input_df = palustrineMappedPixels.savedIterations) {
    for (i in 1:ncol(input_df)) {
      CredibleIntervals <- quantile(input_df[,i], c(0.025, 0.5, 0.975)) 
      if (i == 1) { output_df <- data.frame(CredibleIntervals) }
      else { output_df <- cbind(output_df, CredibleIntervals) }
    }
    colnames(output_df) <- colnames(input_df)
    return(output_df)
  }
  
  palustrineMappedPixels.CIs <- getColumnCIs(palustrineMappedPixels.savedIterations)
  ccap2010perPixelScalers.CIs <- getColumnCIs(ccap2010perPixelScalers.savedIterations)
  cncPerPixelScalers.CIs <- getColumnCIs(cncPerPixelScalers.savedIterations)
  storageAndEmissions.CIs <- getColumnCIs(storageAndEmissions.savedIterations) 
  
  parameterNameStore <- c() # empty vector to store parameter names
  parameterTypeStore <- c() # empty vector to store parameter types
  parameterEffectStore <- c() # empty vector to store 'effect' of parameter on overall total
  
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
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                            palustrineMappedPixels = palustrineMappedMinsTable, 
                            ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                            cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                            storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedMaxsTable, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
  parameterNameStore <- c(parameterNameStore, paste(palustrineMappedMaxsTable$abbrev[i], ".mappedArea", sep=""))
  parameterTypeStore <- c(parameterTypeStore, "coastal.lands")
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
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Mins, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Maxs, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(ccap2010perPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
    
    
  }
  
  # move on to CNC Accuracy
  for (i in 1:ncol(cncPerPixelScalers.CIs)) {

    cncPerPixelScalers.Mins <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Mins[1, i] <- cncPerPixelScalers.CIs[1,i]
    
    cncPerPixelScalers.Maxs <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Maxs[1, i] <- cncPerPixelScalers.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Mins,
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Maxs,
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(cncPerPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
  }
  
  # move on to burial / emissions factors
  for (i in 1:(ncol(storageAndEmissions.CIs)-2)) {
    
    storageAndEmissions.Mins <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Mins[1, i] <- storageAndEmissions.CIs[1,i]
    
    storageAndEmissions.Maxs <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Maxs[1, i] <- storageAndEmissions.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Mins)[[1]]
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Maxs)[[1]]
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(storageAndEmissions.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
  }
  
  # Test a Couple Key Assumptions.
  # First compare calculating Palustrine Wetland Mapped Area Based on Coastal Lands
  {
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,])[[1]]
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.1.NWI, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Maxs)[[1]]
    
    parameterNameStore <- c(parameterNameStore, "areaOfInterest")
    parameterTypeStore <- c(parameterTypeStore, "assumption")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
  }
  
  # Second compare calculating calculating methane based on Sustained Global Warming/Cooling Potential vs GWP
  {
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,], 
                               gwp=T)[[1]]
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,], 
                               gwp=F)[[1]]
    parameterNameStore <- c(parameterNameStore, "gwpVsSgwp")
    parameterTypeStore <- c(parameterTypeStore, "assumption")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate - minEstimate))
  }
  sensitivityAnalysisDF <- data.frame(parameter = parameterNameStore, type = parameterTypeStore, effectTonnesCO2 = parameterEffectStore)
  sensitivityAnalysisDF <- sensitivityAnalysisDF[order(-sensitivityAnalysisDF$effectTonnesCO2), ]
  
  sensitivityAnalysisDF["effectMillionTonnesCO2"] <- round(sensitivityAnalysisDF$effectTonnesCO2 * 1E-6, 2)
  View(sensitivityAnalysisDF)
  write.table(sensitivityAnalysisDF, "data/outputTables/SensitivityAnalysisResults.csv", sep=",", row.names = F)
  
}
