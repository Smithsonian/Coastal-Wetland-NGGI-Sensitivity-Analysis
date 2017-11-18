# make some figures and a table for supplemental information for soil carbon density

library(ggplot2)
library(raster)

# Soils table
soils <- read.table(paste(getwd(), "/data/HolmquistSoilSynthesis/Cal_Val_Points_171016.txt", sep=""), header=T)

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

# convert to matrix etc.
{

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


# Variables are in kgC per m2
# convert to gCO2 per m2
cmMatrix <- cmMatrix * gramsPerKg * carbonToCO2

# get summary statistics for later
cm.means <- colMeans(cmMatrix, na.rm=T)
cm.n <- colSums(!is.na(cmMatrix))
cm.cov <- cov(cmMatrix, use="complete.obs") # get covariance matrix

cm.cor <- cor(cmMatrix, use="complete.obs") # get correlation matrix

}

cm.df.x <- c()
cm.df.y <- c()
cm.df.varCov <- c()

for (x in c(1:15)) {
  for (y in c(1:15)) {
    cm.df.x <- c(cm.df.x, x-.5)
    cm.df.y <- c(cm.df.y, y-.5)
    if (y<=x) {
      cm.df.varCov <- c(cm.df.varCov, cm.cov[x,y]) 
    } else {
      cm.df.varCov <- c(cm.df.varCov, NA)
    }
  } 
}


cm.df.x <- c()
cm.df.y <- c()
cm.df.cor <- c()

for (x in c(1:15)) {
  for (y in c(1:15)) {
    cm.df.x <- c(cm.df.x, x-.5)
    cm.df.y <- c(cm.df.y, y-.5)
    if (y<=x) {
      cm.df.cor <- c(cm.df.cor, cm.cor[x,y]) 
    } else {
      cm.df.cor <- c(cm.df.cor, NA)
    }
  } 
}

cm.cov.df <- data.frame(x= cm.df.x, y=cm.df.y, z = cm.df.varCov)

ggplot(data=cm.cov.df, aes(x,y,fill=z)) +
  geom_tile() +
  scale_fill_continuous(name="Variance-Covariance", low = "lightblue", high = "black") +
  scale_y_reverse()



cm.cor.df <- data.frame(x = cm.df.x, y = cm.df.y, z = cm.df.cor)

ggplot(data=cm.cor.df, aes(x,y,fill=z)) +
  geom_tile() +
  scale_fill_continuous(name="Correlation", low = "lightyellow", high = "darkblue") +
  scale_y_reverse()
