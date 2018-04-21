library(MASS)
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
# get C mass means
cmMeans <- colMeans(cmMatrix, na.rm=T)
cmCounts <- colSums(!is.na(cmMatrix))

# get covariance matrix
covCM <- cov(cmMatrix, use="complete.obs")
sqrt(sum(covCM))
sqrt(diag(covCM))

# simulate a matrix based on multivariate normal
simCMs <- as.matrix(mvrnorm(max(cmCounts), cmMeans, covCM))
simCMs[simCMs < 0] <- 0 # bound values at 0

# make it so that there's variable coverages mimiking empirical data coverage
for (i in 1:ncol(simCMs)) {
  if (cmCounts[i] != nrow(simCMs)) {
    simCMs[(cmCounts[i] + 1):nrow(simCMs),i] <- NA 
  }
}

# generate a random number of depth intervals to include
# will need to update this one once I calculate the 150 cm ones
randomDepthAffected <- round(runif(1,5,15))

soilEmissionsFactor <- sum(colMeans(simCMs[,1:randomDepthAffected], na.rm = T)) * 1000