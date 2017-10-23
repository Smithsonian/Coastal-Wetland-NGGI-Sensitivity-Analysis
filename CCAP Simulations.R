# prob distributions for CCAP AA

classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
               'Cultivated', 'Pasture/Hay', 
               'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
               'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
               'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
               'Unconsolidated Shore', 'Bare Land', 'Water', 
               'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed', 
               'Snow/Ice')


ccapCol <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/ccapCols.csv", sep=""))

ccapColVect <- c()
for (i in 1:nrow(ccapCol)) {
  ccapColVect <- c(ccapColVect, rgb(ccapCol$red[i], ccapCol$green[i], ccapCol$blue[i], max=255))
}

# Generating Tables to get unbiased esimators of error w/ CI's from CCAP data
test_aa <- as.matrix(read.csv(paste(getwd(), "/data/TestDataAA.csv", sep=""), row.names = 1))
test_area <-read.csv(paste(getwd(), "/data/TestDataArea.csv", sep=""))
test_area <- test_area$Area

ccap_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/CCAP2010AccuracyAssessment.csv", sep=""), row.names = 1))
ccap_area <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/ccapPixelCounts.csv", sep=""))
ccap_names <- ccap_area$abbrevs
ccap_area <- ccap_area$pixels

cnc_aa <- as.matrix(read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/CCAP06to10ChangeNoChangeAccuracyAssesment.csv", sep=""), row.names = 1))
cnc_area <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/cncPixelCounts.csv", sep=""))
cnc_area <- cnc_area$pixels

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

areaCorrections(ccap_aa, ccap_area)
simmulateCCAP <- simulateAA(ccap_aa)

for (i in 1:1000) {
  simmulateCCAP <- simulateAA(ccap_aa)
  simulatedAreas <- areaCorrections(simmulateCCAP, ccap_area)$estimatedArea
  if (i == 1){
    store_df <- data.frame(t(as.data.frame(simulatedAreas)), row.names = NULL)
    colnames(store_df) <- colnames(ccap_aa)
  } else {
    store_df <- rbind(store_df, simulatedAreas)
  }
}

par(oma = c(3, 3, 3, 0), mfrow=c(5,5), mar=c(2,2,1,1))
for (i in 1:length(classOrder)) {
  hist(c(store_df[,i]), col=ccapColVect[i], main=classOrder[i])
}
mtext("pixels (n)", side=1, outer=T, line=1.5)
mtext("frequency", side=2, outer=T, line=1.5)
mtext("1,000 Iterations of Accuracy Assesment (multinomial distributions)", side=3, outer=2, line=1.5)
