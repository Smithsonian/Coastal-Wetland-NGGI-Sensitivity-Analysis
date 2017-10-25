# this will simulate the NGGI Based on Available Data
# load necessary packages 
{
  library(foreign) # to read .dbf files
  library(MASS) # to create multivariate normal distribution
}

# load files
{
  # all CCAP data from 2006 to 2010 for getting estuarine subcategory pixel counts
  ccap_fullTab <- read.dbf(paste(getwd(), "/data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf", sep=""))
  
  ccap_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/CCAP2010AccuracyAssessment.csv", sep=""), row.names = 1))
  ccap_tab <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/ccapPixelCounts.csv", sep=""))
  ccap_area <- ccap_tab$pixels
  
  cnc_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/CCAP06to10ChangeNoChangeAccuracyAssesment.csv", sep=""), row.names = 1))
  cnc_tab <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/cncPixelCounts.csv", sep=""))
  cnc_area <- cnc_tab$pixels
  
  palustrineFilePath <- paste(getwd(), "/data/WetlandArea/Palustrine/PalustrinePixelCounts/_AllCONUS/tables", sep="")
  
  # Soils table
  soils <- read.table(paste(getwd(), "/data/HolmquistSoilSynthesis/Cal_Val_Points_171016.txt", sep=""), header=T)
  
  # create a table with all C mass values
  cmMatrix <- as.matrix(data.frame(CM000_010 = soils$vCM000_010, 
                                   CM010_020 = soils$vCM010_020,
                                   CM020_030 = soils$vCM020_030,
                                   CM030_040 = soils$vCM030_040,
                                   CM040_050 = soils$vCM040_050,
                                   CM050_060 = soils$vCM050_060,
                                   CM060_070 = soils$vCM060_070,
                                   CM070_080 = soils$vCM070_080,
                                   CM080_090 = soils$vCM080_090,
                                   CM090_100 = soils$vCM090_100,
                                   CM100_110 = soils$vCM100_110, 
                                   CM110_120 = soils$vCM110_120,
                                   CM120_130 = soils$vCM120_130,
                                   CM130_140 = soils$vCM130_140,
                                   CM140_150 = soils$vCM140_150,
                                   CM150_160 = soils$vCM150_160,
                                   CM160_170 = soils$vCM160_170,
                                   CM170_180 = soils$vCM170_180,
                                   CM180_190 = soils$vCM180_190,
                                   CM190_200 = soils$vCM190_200))
  
  # define Carbon Accumulation Rate Assumptions
  car <- read.csv(paste(getwd(), "/data/MengReview/PbandcsData_170926.csv", sep=""))
  
  kristinBiomass <- read.csv(paste(getwd(), "/data/Biomass/BiomassSamples.csv", sep=""))
  kristinBiomass <- subset(kristinBiomass, biomass_gm > 0)
  
  mengBiomass <- read.csv(paste(getwd(), "/data/MengReview/mangroveAndMarshesABG.csv", sep=""))
  mangroveBiomass <- subset(mengBiomass , Ecosystem == "mangrove")
  
  methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
  names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
  
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
}

# Prep the Soil Carbon Data and convert to gCO2 eq. per m2 per year
{
  # Variables are in kgC per m2
  # convert to gCO2 per m2
  cmMatrix <- cmMatrix * gramsPerKg * carbonToCO2
  
  # get summary statistics for later
  cm.means <- colMeans(cmMatrix, na.rm=T)
  cm.n <- colSums(!is.na(cmMatrix))
  cm.cov <- cov(cmMatrix, use="complete.obs") # get covariance matrix
  
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

# Prep methane data and convert to gCO2 eq per m2 per year
{
  # using Scott Neubauer's CO2 Equivalents for SGWP and SGCP at 100 years
  generateMethaneSGPs <- function(ch4) {
    if (ch4 < 0) { return(ch4 * 203)
    } else { return(ch4 * 45)}
  }
  
  # Units are in gCH4 per m2 per year
  ch4_co2_eq <- mapply(generateMethaneSGPs, methane$CH4.flux) # Calculate CO2 equivalents
  methane["ch4_co2_eq"] <- ch4_co2_eq # add to the dataframe so they can be sorted
  
  # Estuarine Emissions Factors are Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  est.ch4 <- subset(methane, Salinity.ppt >= 5)
  methane.est.n <- length(est.ch4$ch4_co2_eq)
  methane.est.mean <- mean(est.ch4$ch4_co2_eq)
  methane.est.sd <- sd(est.ch4$ch4_co2_eq)
  
  # Palustrine Emissions Factors are Log Normally Distributed
  # Units are in gCO2 equivalent per m2 per year
  pal.ch4 <- subset(methane, Salinity.ppt <5)
  methane.pal.n <- length(pal.ch4$ch4_co2_eq)
  methane.pal.log.mean <- mean(log(pal.ch4$ch4_co2_eq))
  methane.pal.log.sd <- sd(log(pal.ch4$ch4_co2_eq))
  
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

# alternate functions for generating palustrine areas using binomial normal approximation method
{
  calculateBinomialNormalApproximation <- function(inputDf="PEM_PEM", tabDir = paste(getwd(), "/data/WetlandArea/Palustrine/PalustrinePixelCounts/_AllCONUS/tables", sep="")) {
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
  generateLogNormalMeans <- function(x.n, x.log.mean, x.log.sd) { return(mean(rlnorm(x.n, x.log.mean, x.log.sd))) }
  
  generateNormalMeans <- function(x.n, x.mean, x.sd) { return(mean(rnorm(x.n, x.mean, x.sd))) }
  
  # to generate Soil Carbon profiles 
  generateSoilCarbonMassProfiles <- function(soilC.n=cm.n, soilC.mean=cm.means, soilC.cov=cm.cov) {
    # simulate a matrix based on multivariate normal
    simCMs <- as.matrix(mvrnorm(max(soilC.n), soilC.mean, soilC.cov))
    simCMs[simCMs < 0] <- 0 # bound values at 0
    
    # make it so that there's variable coverages mimiking empirical data coverage
    for (i in 1:ncol(simCMs)) {
      if (soilC.n[i] != nrow(simCMs)) {
        simCMs[(soilC.n[i] + 1):nrow(simCMs),i] <- NA 
      }
    }
    return(simCMs)
  }
  
  generateDepthIntervalsLost <- function(depth.min = 50, depth.max=150) { return(round(runif(1, depth.min, depth.max)/10)) } 
  
  generateSoilEmissionsFactor <- function(cMassTable, depthIntervalLost) {
    return(sum(colMeans(cMassTable[,1:depthIntervalLost], na.rm = T)))
  }
}

# create some initial (.1) data.frames with just means for all mapped area, area scale factors, and emissions/storage factors
{
  # load estuarine mapped pixels
  estuarineMappedPixels.1 = estCcapClassDf
  
  # get the average number of palustrine pixels
  palustrineMappedPixels.1 <- palCcapClassDf
  palustrineMappedPixels.1["mappedPixelCount"] <- as.integer(palustrineNormalApproximations$mu)
  
  ccap2010perPixelScalers.1 <- as.data.frame(t(as.matrix(areaCorrections(ccap_aa, ccap_area)$perPixelScaler)))
  names(ccap2010perPixelScalers.1) <- row.names(ccap_aa)
  
  cncPerPixelScalers.1 <- as.data.frame(t(as.matrix(areaCorrections(cnc_aa, cnc_area)$perPixelScaler)))
  names(cncPerPixelScalers.1) <- row.names(cnc_aa)
  
  soil.stocks.1 <- as.data.frame(t(as.matrix(cm.means)))
                                 
  storageAndEmissions.1 = data.frame(soil.burial = exp(pb.log.mean),
                                     depth.invervals.lost = 10, fraction.loss = 0.625,
                                     biomass.em = exp(biomass.em.log.mean), biomass.ss = exp(biomass.ss.log.mean), biomass.fo = exp(biomass.fo.log.mean),
                                     methane.est = methane.est.mean, methane.pal = exp(methane.pal.log.mean)
                                     )
}

# Function for Calculating the Inventory
{
  # Inputs are mapped pixels, area scalers, and storage/emissions factors
  coastalNGGI <- function(estuarineMappedPixels = estuarineMappedPixels.1, 
                          palustrineMappedPixels = palustrineMappedPixels.1, 
                          ccap2010perPixelScalers = ccap2010perPixelScalers.1, 
                          cncPerPixelScalers = cncPerPixelScalers.1,
                          soil.stocks = soil.stocks.1,
                          storageAndEmissions = storageAndEmissions.1,
                          soilEmissionsFactor=NA) {
    
    wetlandMappedPixels <- rbind(estuarineMappedPixels, palustrineMappedPixels)
    
    # empty vectors for storing each class's variables to export in a table later
    derived.quantity.names <- c()
    class.change.in.tonnes.CO2 <- c()
    
    # check to see if we have a soil emissions factor in input data
    if (! is.na(soilEmissionsFactor)) {
      soilEmissionsFactor <- soilEmissionsFactor
    } else { # if we don't generate one from the input tables
      soilEmissionsFactor <- generateSoilEmissionsFactor(soil.stocks, storageAndEmissions$depth.invervals.lost[1])
    }
    
    # for every class in estuarineMappedPixels and palustrineMappedPixels
    for (i in 1:nrow(wetlandMappedPixels)) {
      class.t1 <-  wetlandMappedPixels$class_2006[i]
      class.t2 <-  wetlandMappedPixels$class_2010[i]
      abbrev.t2 <- toString(wetlandMappedPixels$abbrev_2010[i])
      
      # calculate scaled area
      {
        # Multiply number of mapped pixels by the estimated to mapped ratio for the 2010 class 
        n.pixels = wetlandMappedPixels$mappedPixelCount[i] * ccap2010perPixelScalers[, abbrev.t2]
        
        # Multiply number of mapped pixels by the estimated to mapped ratio for the 2006-2010 change class 
        if (class.t1 == class.t2) { # if there's no change scale by the 'no change' estimated to mapped ratio 
          n.pixels = n.pixels * cncPerPixelScalers$No.Change[1]
        } else { # if there's a change scale number of pixels by the 'change' estimated to mapped ratio
          n.pixels = n.pixels * cncPerPixelScalers$Change[1]
        }
      }
      
      # calculate soil change
      {
        
        # is it a wetland to wetland transition (wetland at time 1 and wetland at time 2)
        if ((class.t1 %in% c(estuarineWetlands, palustrineWetlands)) & (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
          soil.change =  storageAndEmissions$soil.burial[1] * 4 # if it's a wetland remaining wetland then it buries carbon for 4 years
        } else {
          # is it a wetland restoration (non wetland at time 1 to wetland at time 2)
          if ((! (class.t1 %in% c(estuarineWetlands, palustrineWetlands))) & (class.t2 %in% c(estuarineWetlands, palustrineWetlands))) {
            soil.change =  storageAndEmissions$soil.burial[1] * 2 # if a class goes from not wetland to wetland we assume it starts buring carbon half way through the time step
          } else { # is it a wetland loss
            if (class.t2 %in% soilLossEvents) { # is it a soil loss event, defined in the 'assumptions section' near the beginning of the code? 
              soil.change = -(soilEmissionsFactor * storageAndEmissions$fraction.loss) # if this is a soil loss even, then the whole column is lost at once
            } else {
              soil.change  = storageAndEmissions$soil.burial[1] * 2 # if it is a wetland loss without soil loss, then we assume it stops buring carbon halfway through the timestep 
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
      
      # calculate methane emissions
      {  
        # What were the emissions at the start?
        if (class.t1 %in% estuarineWetlands) {
          methane.t1 <- storageAndEmissions$methane.est
        } else if (class.t1 %in% palustrineWetlands) {
          methane.t1 <- storageAndEmissions$methane.pal
        } else {
          methane.t1 <- 0
        }
        
        if (class.t2 %in% estuarineWetlands) {
          methane.t2 <- storageAndEmissions$methane.est
        } else if (class.t2 %in% palustrineWetlands) {
          methane.t2 <- storageAndEmissions$methane.pal
        } else {
          methane.t2 <- 0
        }
        
        # What were the emissions at the end?
        methane.change = -(2*methane.t1 + 2*methane.t2)
      }
      
      total.change.per.pixel <- (soil.change + biomass.change + methane.change) # add the three changes together
      total.change.in.grams.CO2 <- (total.change.per.pixel * n.pixels * 900) # multiply by n of pixels
      total.change.in.tonnes.CO2 <-total.change.in.grams.CO2 * 1E-6 # may just need a different fundemental unit of storage
      
      # store values
      derived.quantity.names <- c(derived.quantity.names, paste(wetlandMappedPixels$abbrev[i], ".change.in.tonnes.CO2", sep=""))
      class.change.in.tonnes.CO2 <- c(class.change.in.tonnes.CO2, total.change.in.tonnes.CO2)
    }
    # Summarize accross the entire inventory
    outputDF <- as.data.frame(t(as.matrix(c(sum(class.change.in.tonnes.CO2), class.change.in.tonnes.CO2))))
    colnames(outputDF) <- c("total.tonnes.CO2", derived.quantity.names)
    return(outputDF)
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
    
    depth.invervals.lost.randomDraw <- generateDepthIntervalsLost()
    
    soil.profiles.randomDraw <- generateSoilCarbonMassProfiles()
    soil.stocks.randomDraw <- as.data.frame(t(as.matrix(colMeans(soil.profiles.randomDraw, na.rm=T))))
    soilEmissionsFactor.randomDraw <- generateSoilEmissionsFactor(soil.profiles.randomDraw, depth.invervals.lost.randomDraw)
    
    fraction.loss.randomDraw <- runif(1, 0.5, 0.75)
    
    # random draws for biomass emissions/storage factors
    biomass.em.randomDraw <- generateLogNormalMeans(biomass.em.n, biomass.em.log.mean, biomass.em.log.sd)
    biomass.ss.randomDraw <- generateLogNormalMeans(biomass.ss.n, biomass.ss.log.mean, biomass.ss.log.sd)
    biomass.fo.randomDraw <- generateLogNormalMeans(biomass.fo.n, biomass.fo.log.mean, biomass.fo.log.sd)
    
    # random draws for methane emissions factors
    methane.est.randomDraw <- generateNormalMeans(methane.est.n, methane.est.mean, methane.est.sd)
    methane.pal.randomDraw <- generateLogNormalMeans(methane.pal.n, methane.pal.log.mean, methane.pal.log.sd)
    
    storageAndEmissions.randomDraw = data.frame(soil.burial = soil.burial.randomDraw,
                                       depth.invervals.lost = depth.invervals.lost.randomDraw, fraction.loss = fraction.loss.randomDraw,
                                       biomass.em = biomass.em.randomDraw, biomass.ss = biomass.ss.randomDraw, biomass.fo = biomass.fo.randomDraw,
                                       methane.est = methane.est.randomDraw, methane.pal = methane.pal.randomDraw)
    
    
    # Run NGGI Function
    coastalNGGI.randomDraw <- coastalNGGI(estuarineMappedPixels=estuarineMappedPixels.fixedVariables,
                                          palustrineMappedPixels=palustrineMappedPixels.randomDraw,
                                          ccap2010perPixelScalers=ccap2010perPixelScalers.randomDraw,
                                          cncPerPixelScalers=cncPerPixelScalers.randomDraw,
                                          soil.stocks=soil.stocks.randomDraw,
                                          storageAndEmissions=storageAndEmissions.randomDraw,
                                          soilEmissionsFactor=soilEmissionsFactor.randomDraw
                                          )
    
    # if it's the first iteration establish the output data.tables
    if (i == 1) {
      palustrineMappedPixels.savedIterations <- as.data.frame(t(as.matrix(palustrineMappedPixels.randomDraw$mappedPixelCount)))
      colnames(palustrineMappedPixels.savedIterations) <- palustrineMappedPixels.randomDraw$abbrev
      ccap2010perPixelScalers.savedIterations <- ccap2010perPixelScalers.randomDraw
      cncPerPixelScalers.savedIterations <- cncPerPixelScalers.randomDraw
      soil.stocks.savedIterations <- soil.stocks.randomDraw
      storageAndEmissions.savedIterations <- storageAndEmissions.randomDraw
      soilEmissionsFactor.savedIterations <- c(soilEmissionsFactor.randomDraw)
      
      coastalNGGI.savedIterations <- coastalNGGI.randomDraw
    } else { # if it's after one add the inputs and outputs as a row in the saved data frames
      palustrineMappedPixels.savedIterations <- rbind(palustrineMappedPixels.savedIterations, palustrineMappedPixels.randomDraw$mappedPixelCount)
      ccap2010perPixelScalers.savedIterations <- rbind(ccap2010perPixelScalers.savedIterations, ccap2010perPixelScalers.randomDraw)
      cncPerPixelScalers.savedIterations <- rbind(cncPerPixelScalers.savedIterations, cncPerPixelScalers.randomDraw)
      soil.stocks.savedIterations <- rbind(soil.stocks.savedIterations, soil.stocks.randomDraw)
      storageAndEmissions.savedIterations <- rbind(storageAndEmissions.savedIterations, storageAndEmissions.randomDraw)
      soilEmissionsFactor.savedIterations <- c(soilEmissionsFactor.savedIterations, soilEmissionsFactor.randomDraw)
      
      coastalNGGI.savedIterations <- rbind(coastalNGGI.savedIterations, coastalNGGI.randomDraw)
    }
    if (i == n.iterations) {print("Done!")}
  }
}

par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(3,3,3,0))
hist(coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6, main="", xlab="", breaks=40, col="grey", xlim=(quantile( (coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6), c(0, 0.975))))
mtext(expression(paste("Million Tonnes CO"[2], " (- emission / + storage)")), side=1, line=1.5, outer=T)
mtext("frequency", side=2, line=1.5, outer=T)
mtext("Uncertainty Analysis", side=3, line=1.5, outer=T)
mtext("(1,000 Simulations)", side=3, line=0.5, outer=T)
abline(v=0, lty=2, lwd=2, col="darkred")

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
  soil.stocks.CIs <- getColumnCIs(soil.stocks.savedIterations)
  storageAndEmissions.CIs <- getColumnCIs(storageAndEmissions.savedIterations) 
  soilEmissionsFactor.CIs <- quantile(soilEmissionsFactor.savedIterations, c(0.025, 0.5, 0.975))
  
  
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
      
      palustrineMappedMaxsTable <- palCcapClassDf
      palustrineMappedMaxsTable["mappedPixelCount"] <- c(t(palustrineMappedMaxs[1,]))
    }
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                            palustrineMappedPixels = palustrineMappedMinsTable, 
                            ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                            cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                            soil.stocks = soil.stocks.CIs[2,],
                            storageAndEmissions = storageAndEmissions.CIs[2,],
                            soilEmissionsFactor=NA)
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedMaxsTable, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
  parameterNameStore <- c(parameterNameStore, paste(palustrineMappedMaxsTable$abbrev[i], ".mappedArea", sep=""))
  parameterTypeStore <- c(parameterTypeStore, "coastal.lands")
  parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # the rest of the analyses will use this input table for palustrine pixel count
  palustrineMappedPixels.CIs.Med.Table <- palCcapClassDf
  palustrineMappedPixels.CIs.Med.Table["mappedPixelCount"] <- c(t(palustrineMappedPixels.CIs[2,]))
  
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
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Maxs, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(ccap2010perPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
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
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Maxs,
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(cncPerPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
  }
  
  # move on to soil C stocks
  for (i in 1:ncol(soil.stocks.CIs)) {
    
    soil.stocks.Mins <- soil.stocks.CIs[2,]
    soil.stocks.Mins[1, i] <- soil.stocks.CIs[1,i]
    
    soil.stocks.Maxs <- soil.stocks.CIs[2,]
    soil.stocks.Maxs[1, i] <- soil.stocks.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Mins,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Maxs,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(soil.stocks.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # move on to burial / emissions factors
  for (i in 1:ncol(storageAndEmissions.CIs)) {
    
    storageAndEmissions.Mins <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Mins[1, i] <- storageAndEmissions.CIs[1,i]
    
    storageAndEmissions.Maxs <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Maxs[1, i] <- storageAndEmissions.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Mins,
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.CIs.Med.Table, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Maxs,
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(storageAndEmissions.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  sensitivityAnalysisDF <- data.frame(parameter = parameterNameStore, type = parameterTypeStore, effectTonnesCO2 = parameterEffectStore)
  sensitivityAnalysisDF <- sensitivityAnalysisDF[order(-sensitivityAnalysisDF$effectTonnesCO2), ]
  
}

sensitivityAnalysisDF_topTwenty <- sensitivityAnalysisDF[1:20,]
barplot(sensitivityAnalysisDF_topTwenty$effectTonnesCO2 * 1E-6, 
        horiz = T, ylim=rev(c(1,24)), 
        names.arg=sensitivityAnalysisDF_topTwenty$parameter,
        xlab="",
        main="Top 20 Uncertain Parameters (Estuarine)",
        las=1)
mtext("Sensitivity of Total NGGI to Parameter", 1, line=2, outer=F)
mtext(expression(paste("(Million Tonnes CO"[2], ")")), 1, line=3, outer=F)

# Run an alternate version of the sensitivity analysis where all palustrine mapped areas are 0
# Re-run The Inventory 1 input-variable at a time for the sensitivity analysis

palustrineMappedPixels.DummyVars <- palCcapClassDf
palustrineMappedPixels.DummyVars["mappedPixelCount"] <- rep(0, nrow(palustrineMappedPixels.DummyVars))
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
  
  ccap2010perPixelScalers.CIs <- getColumnCIs(ccap2010perPixelScalers.savedIterations)
  cncPerPixelScalers.CIs <- getColumnCIs(cncPerPixelScalers.savedIterations)
  soil.stocks.CIs <- getColumnCIs(soil.stocks.savedIterations)
  storageAndEmissions.CIs <- getColumnCIs(storageAndEmissions.savedIterations) 
  soilEmissionsFactor.CIs <- quantile(soilEmissionsFactor.savedIterations, c(0.025, 0.5, 0.975))
  
  parameterNameStore <- c() # empty vector to store parameter names
  parameterTypeStore <- c() # empty vector to store parameter types
  parameterEffectStore <- c() # empty vector to store 'effect' of parameter on overall total
  
  # iterate through each parameter table
  # start with palustrineMappedPixels
  for (i in 1:ncol(palustrineMappedPixels.DummyVars)) {
    
    # prep the input data tables
    {
      ## Inputs are mapped pixels, area scalers, and storage/emissions factors
      #palustrineMappedMedians <- palustrineMappedPixels.DummyVars[2,]
      # analyse min 
      #palustrineMappedMins <- palustrineMappedMedians
      #palustrineMappedMaxs <- palustrineMappedMedians
      
      #palustrineMappedMins[1,i] <- palustrineMappedPixels.CIs[1,i]
      #palustrineMappedMaxs[1,i] <- palustrineMappedPixels.CIs[3,i]
      
      #palustrineMappedMinsTable <- palCcapClassDf
      #palustrineMappedMinsTable["mappedPixelCount"] <- c(t(palustrineMappedMins[1,]))
      
      #palustrineMappedMaxsTable <- palCcapClassDf
      #palustrineMappedMaxsTable["mappedPixelCount"] <- c(t(palustrineMappedMaxs[1,]))
    }
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(palustrineMappedMaxsTable$abbrev[i], ".mappedArea", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "coastal.lands")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # the rest of the analyses will use this input table for palustrine pixel count
  #palustrineMappedPixels.CIs.Med.Table <- palCcapClassDf
  #palustrineMappedPixels.CIs.Med.Table["mappedPixelCount"] <- c(t(palustrineMappedPixels.CIs[2,]))
  
  # then move on to CCAP 2010 Class Accuracy
  for (i in 1:ncol(ccap2010perPixelScalers.CIs)) {
    ccap2010perPixelScalers.Mins <- ccap2010perPixelScalers.CIs[2,]
    ccap2010perPixelScalers.Mins[1, i] <-  ccap2010perPixelScalers.CIs[1,i]
    
    ccap2010perPixelScalers.Maxs <- ccap2010perPixelScalers.CIs[2,]
    ccap2010perPixelScalers.Maxs[1, i] <-  ccap2010perPixelScalers.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Mins, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Maxs, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(ccap2010perPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
  }
  
  # move on to CNC Accuracy
  for (i in 1:ncol(cncPerPixelScalers.CIs)) {
    
    cncPerPixelScalers.Mins <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Mins[1, i] <- cncPerPixelScalers.CIs[1,i]
    
    cncPerPixelScalers.Maxs <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Maxs[1, i] <- cncPerPixelScalers.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Mins,
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Maxs,
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(cncPerPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
  }
  
  # move on to soil C stocks
  for (i in 1:ncol(soil.stocks.CIs)) {
    
    soil.stocks.Mins <- soil.stocks.CIs[2,]
    soil.stocks.Mins[1, i] <- soil.stocks.CIs[1,i]
    
    soil.stocks.Maxs <- soil.stocks.CIs[2,]
    soil.stocks.Maxs[1, i] <- soil.stocks.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Mins,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Maxs,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(soil.stocks.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # move on to burial / emissions factors
  for (i in 1:ncol(storageAndEmissions.CIs)) {
    
    storageAndEmissions.Mins <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Mins[1, i] <- storageAndEmissions.CIs[1,i]
    
    storageAndEmissions.Maxs <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Maxs[1, i] <- storageAndEmissions.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Mins,
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Maxs,
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(storageAndEmissions.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  sensitivityAnalysisDF <- data.frame(parameter = parameterNameStore, type = parameterTypeStore, effectTonnesCO2 = parameterEffectStore)
  sensitivityAnalysisDF <- sensitivityAnalysisDF[order(-sensitivityAnalysisDF$effectTonnesCO2), ]
  
}

# Make plots to summarize the sensitivity analysis
sensitivityAnalysisDF_topTen <- sensitivityAnalysisDF[1:10,]
par(mar=c(4,9,3,1), oma=c(0,0,0,0))
barplot(sensitivityAnalysisDF_topTen$effectTonnesCO2 * 1E-6, 
        horiz = T, ylim=rev(c(1,12)), 
        names.arg=sensitivityAnalysisDF_topTen$parameter,
        xlab="",
        main="Top 10 Uncertain Parameters (Estuarine)",
        las=1)
mtext("Sensitivity of Total NGGI to Parameter", 1, line=2, outer=F)
mtext(expression(paste("(Million Tonnes CO"[2], ")")), 1, line=3, outer=F)

# Run 1000 Alt Version of the Uncertainty Analysis with only Estuarine Wetlands
{
  # Define the number of iterations
  n.iterations = 1000
  
  # need a place to save the output files
  
  pb <- txtProgressBar(0, n.iterations) # establish progress bar
  
  # load estuarine mapped pixels
  estuarineMappedPixels.fixedVariables <- estCcapClassDf # these are fixed variables so we use the same one every time
  palustrineMappedPixels.randomDraw <- palustrineMappedPixels.DummyVars
  
  for (i in 1:n.iterations){
    setTxtProgressBar(pb, i) # forward progress bar
    
    # Create Input Tables by randomly drawing from the data's probability distributions  
    # random draws for area mapping
    #palustrineMappedPixels.randomDraw <- generatePalustrineAreaTableBNA()
    ccap2010perPixelScalers.randomDraw <- simulatePerPixelScalers(ccap_aa, ccap_area)
    cncPerPixelScalers.randomDraw <- simulatePerPixelScalers(cnc_aa, cnc_area)
    
    # random draws for soil carbon emissions/storage factors
    soil.burial.randomDraw <- generateLogNormalMeans(pb.n, pb.log.mean, pb.log.sd)
    
    depth.invervals.lost.randomDraw <- generateDepthIntervalsLost()
    
    soil.profiles.randomDraw <- generateSoilCarbonMassProfiles()
    soil.stocks.randomDraw <- as.data.frame(t(as.matrix(colMeans(soil.profiles.randomDraw, na.rm=T))))
    soilEmissionsFactor.randomDraw <- generateSoilEmissionsFactor(soil.profiles.randomDraw, depth.invervals.lost.randomDraw)
    
    fraction.loss.randomDraw <- runif(1, 0.5, 0.75)
    
    # random draws for biomass emissions/storage factors
    biomass.em.randomDraw <- generateLogNormalMeans(biomass.em.n, biomass.em.log.mean, biomass.em.log.sd)
    biomass.ss.randomDraw <- generateLogNormalMeans(biomass.ss.n, biomass.ss.log.mean, biomass.ss.log.sd)
    biomass.fo.randomDraw <- generateLogNormalMeans(biomass.fo.n, biomass.fo.log.mean, biomass.fo.log.sd)
    
    # random draws for methane emissions factors
    methane.est.randomDraw <- generateNormalMeans(methane.est.n, methane.est.mean, methane.est.sd)
    methane.pal.randomDraw <- generateLogNormalMeans(methane.pal.n, methane.pal.log.mean, methane.pal.log.sd)
    
    storageAndEmissions.randomDraw = data.frame(soil.burial = soil.burial.randomDraw,
                                                depth.invervals.lost = depth.invervals.lost.randomDraw, fraction.loss = fraction.loss.randomDraw,
                                                biomass.em = biomass.em.randomDraw, biomass.ss = biomass.ss.randomDraw, biomass.fo = biomass.fo.randomDraw,
                                                methane.est = methane.est.randomDraw, methane.pal = methane.pal.randomDraw)
    
    
    # Run NGGI Function
    coastalNGGI.randomDraw <- coastalNGGI(estuarineMappedPixels=estuarineMappedPixels.fixedVariables,
                                          palustrineMappedPixels=palustrineMappedPixels.randomDraw,
                                          ccap2010perPixelScalers=ccap2010perPixelScalers.randomDraw,
                                          cncPerPixelScalers=cncPerPixelScalers.randomDraw,
                                          soil.stocks=soil.stocks.randomDraw,
                                          storageAndEmissions=storageAndEmissions.randomDraw,
                                          soilEmissionsFactor=soilEmissionsFactor.randomDraw
    )
    
    # if it's the first iteration establish the output data.tables
    if (i == 1) {
      palustrineMappedPixels.savedIterations <- as.data.frame(t(as.matrix(palustrineMappedPixels.randomDraw$mappedPixelCount)))
      colnames(palustrineMappedPixels.savedIterations) <- palustrineMappedPixels.randomDraw$abbrev
      ccap2010perPixelScalers.savedIterations <- ccap2010perPixelScalers.randomDraw
      cncPerPixelScalers.savedIterations <- cncPerPixelScalers.randomDraw
      soil.stocks.savedIterations <- soil.stocks.randomDraw
      storageAndEmissions.savedIterations <- storageAndEmissions.randomDraw
      soilEmissionsFactor.savedIterations <- c(soilEmissionsFactor.randomDraw)
      
      coastalNGGI.savedIterations <- coastalNGGI.randomDraw
    } else { # if it's after one add the inputs and outputs as a row in the saved data frames
      palustrineMappedPixels.savedIterations <- rbind(palustrineMappedPixels.savedIterations, palustrineMappedPixels.randomDraw$mappedPixelCount)
      ccap2010perPixelScalers.savedIterations <- rbind(ccap2010perPixelScalers.savedIterations, ccap2010perPixelScalers.randomDraw)
      cncPerPixelScalers.savedIterations <- rbind(cncPerPixelScalers.savedIterations, cncPerPixelScalers.randomDraw)
      soil.stocks.savedIterations <- rbind(soil.stocks.savedIterations, soil.stocks.randomDraw)
      storageAndEmissions.savedIterations <- rbind(storageAndEmissions.savedIterations, storageAndEmissions.randomDraw)
      soilEmissionsFactor.savedIterations <- c(soilEmissionsFactor.savedIterations, soilEmissionsFactor.randomDraw)
      
      coastalNGGI.savedIterations <- rbind(coastalNGGI.savedIterations, coastalNGGI.randomDraw)
    }
    if (i == n.iterations) {print("Done!")}
  }
}
par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(3,3,3,0))
hist(coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6, main="", xlab="", breaks=40, col="grey", xlim=(quantile( (coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6), c(0.001, 0.975))))
mtext(expression(paste("Million Tonnes CO"[2], " (- emission / + storage)")), side=1, line=1.5, outer=T)
mtext("frequency", side=2, line=1.5, outer=T)
mtext("Uncertainty Analysis (Estuarine)", side=3, line=1.5, outer=T)
mtext("(1,000 Simulations)", side=3, line=0.5, outer=T)
abline(v=0, lty=2, lwd=2, col="darkred")

# Run an alternate version of the estuarine inventory with cs-based CAR rather than pb-based
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
  
  ccap2010perPixelScalers.CIs <- getColumnCIs(ccap2010perPixelScalers.savedIterations)
  cncPerPixelScalers.CIs <- getColumnCIs(cncPerPixelScalers.savedIterations)
  soil.stocks.CIs <- getColumnCIs(soil.stocks.savedIterations)
  storageAndEmissions.CIs <- getColumnCIs(storageAndEmissions.savedIterations) 
  soilEmissionsFactor.CIs <- quantile(soilEmissionsFactor.savedIterations, c(0.025, 0.5, 0.975))
  
  parameterNameStore <- c() # empty vector to store parameter names
  parameterTypeStore <- c() # empty vector to store parameter types
  parameterEffectStore <- c() # empty vector to store 'effect' of parameter on overall total
  
  # iterate through each parameter table
  # start with palustrineMappedPixels
  for (i in 1:ncol(palustrineMappedPixels.DummyVars)) {
    
    # prep the input data tables
    {
      ## Inputs are mapped pixels, area scalers, and storage/emissions factors
      #palustrineMappedMedians <- palustrineMappedPixels.DummyVars[2,]
      # analyse min 
      #palustrineMappedMins <- palustrineMappedMedians
      #palustrineMappedMaxs <- palustrineMappedMedians
      
      #palustrineMappedMins[1,i] <- palustrineMappedPixels.CIs[1,i]
      #palustrineMappedMaxs[1,i] <- palustrineMappedPixels.CIs[3,i]
      
      #palustrineMappedMinsTable <- palCcapClassDf
      #palustrineMappedMinsTable["mappedPixelCount"] <- c(t(palustrineMappedMins[1,]))
      
      #palustrineMappedMaxsTable <- palCcapClassDf
      #palustrineMappedMaxsTable["mappedPixelCount"] <- c(t(palustrineMappedMaxs[1,]))
    }
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(palustrineMappedMaxsTable$abbrev[i], ".mappedArea", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "coastal.lands")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # the rest of the analyses will use this input table for palustrine pixel count
  #palustrineMappedPixels.CIs.Med.Table <- palCcapClassDf
  #palustrineMappedPixels.CIs.Med.Table["mappedPixelCount"] <- c(t(palustrineMappedPixels.CIs[2,]))
  
  # then move on to CCAP 2010 Class Accuracy
  for (i in 1:ncol(ccap2010perPixelScalers.CIs)) {
    ccap2010perPixelScalers.Mins <- ccap2010perPixelScalers.CIs[2,]
    ccap2010perPixelScalers.Mins[1, i] <-  ccap2010perPixelScalers.CIs[1,i]
    
    ccap2010perPixelScalers.Maxs <- ccap2010perPixelScalers.CIs[2,]
    ccap2010perPixelScalers.Maxs[1, i] <-  ccap2010perPixelScalers.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Mins, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.Maxs, 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(ccap2010perPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
  }
  
  # move on to CNC Accuracy
  for (i in 1:ncol(cncPerPixelScalers.CIs)) {
    
    cncPerPixelScalers.Mins <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Mins[1, i] <- cncPerPixelScalers.CIs[1,i]
    
    cncPerPixelScalers.Maxs <- cncPerPixelScalers.CIs[2,]
    cncPerPixelScalers.Maxs[1, i] <- cncPerPixelScalers.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Mins,
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.Maxs,
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(cncPerPixelScalers.CIs)[i], ".Accuracy", sep=""))
    parameterTypeStore <- c(parameterTypeStore, "CCAP")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
    
    
  }
  
  # move on to soil C stocks
  for (i in 1:ncol(soil.stocks.CIs)) {
    
    soil.stocks.Mins <- soil.stocks.CIs[2,]
    soil.stocks.Mins[1, i] <- soil.stocks.CIs[1,i]
    
    soil.stocks.Maxs <- soil.stocks.CIs[2,]
    soil.stocks.Maxs[1, i] <- soil.stocks.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Mins,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.Maxs,
                               storageAndEmissions = storageAndEmissions.CIs[2,],
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(soil.stocks.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  # move on to burial / emissions factors
  for (i in 1:ncol(storageAndEmissions.CIs)) {
    
    storageAndEmissions.Mins <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Mins[1, i] <- storageAndEmissions.CIs[1,i]
    
    storageAndEmissions.Maxs <- storageAndEmissions.CIs[2,]
    storageAndEmissions.Maxs[1, i] <- storageAndEmissions.CIs[3,i]
    
    minEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Mins,
                               soilEmissionsFactor=NA)
    
    maxEstimate <- coastalNGGI(estuarineMappedPixels = estuarineMappedPixels.1, 
                               palustrineMappedPixels = palustrineMappedPixels.DummyVars, 
                               ccap2010perPixelScalers = ccap2010perPixelScalers.CIs[2,], 
                               cncPerPixelScalers = cncPerPixelScalers.CIs[2,],
                               soil.stocks = soil.stocks.CIs[2,],
                               storageAndEmissions = storageAndEmissions.Maxs,
                               soilEmissionsFactor=NA)
    
    parameterNameStore <- c(parameterNameStore, paste(colnames(storageAndEmissions.CIs)[i], sep=""))
    parameterTypeStore <- c(parameterTypeStore, "burialAndEmissionFactors")
    parameterEffectStore <- c(parameterEffectStore, abs(maxEstimate$total.tonnes.CO2 - minEstimate$total.tonnes.CO2))
  }
  
  sensitivityAnalysisDF <- data.frame(parameter = parameterNameStore, type = parameterTypeStore, effectTonnesCO2 = parameterEffectStore)
  sensitivityAnalysisDF <- sensitivityAnalysisDF[order(-sensitivityAnalysisDF$effectTonnesCO2), ]
  
}

par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(3,3,3,0))
hist(coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6, main="", xlab="", breaks=40, col="grey")
mtext(expression(paste("Million Tonnes CO"[2], " (- emission / + storage)")), side=1, line=1.5, outer=T)
mtext("frequency", side=2, line=1.5, outer=T)
mtext(expression(paste("Uncertainty Analysis [Estuarine; "^137, "Cs-based CAR]")), side=3, line=1.5, outer=T)
mtext("(1,000 Simulations)", side=3, line=0.5, outer=T)
abline(v=0, lty=2, lwd=2, col="darkred")

