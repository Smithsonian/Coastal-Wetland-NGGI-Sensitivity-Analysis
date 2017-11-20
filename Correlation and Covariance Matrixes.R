# make some figures and a table for supplemental information for soil carbon density

library(ggplot2)
library(raster)
library(wesanderson)
library(gridBase)

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
cmMatrix[cmMatrix<0] <- 0

depths_vect <-c()
cm_vect <- c()

for (x in 1:ncol(cmMatrix)) {
  temp_cm <- cmMatrix[,x][!is.na(cmMatrix[,x])]
  cm_vect <- c(cm_vect, temp_cm)
  depths_vect <-c(depths_vect, rep(x*10, length(temp_cm)))
}

cm_oneCol <- data.frame(depth = depths_vect, soilCO2 = cm_vect)

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
    #if (y<=x) {
      cm.df.varCov <- c(cm.df.varCov, cm.cov[x,y]) 
    #} else {
    #  cm.df.varCov <- c(cm.df.varCov, NA)
    #}
  } 
}


cm.df.x <- c()
cm.df.y <- c()
cm.df.cor <- c()

for (x in c(1:15)) {
  for (y in c(1:15)) {
    cm.df.x <- c(cm.df.x, x-.5)
    cm.df.y <- c(cm.df.y, y-.5)
    #if (y<=x) {
      cm.df.cor <- c(cm.df.cor, cm.cor[x,y]) 
    #} else {
    #  cm.df.cor <- c(cm.df.cor, NA)
    #}
  } 
}

cm.cov.df <- data.frame(x= cm.df.x, y=cm.df.y, z = cm.df.varCov)


bp<-boxplot(soilCO2 ~ depth, data = cm_oneCol, col="lightblue", xlab="", axisnames = F, horizontal=T, axes = F, main=expression("soil depth increments"), space=0, xlim=c(15,1), xpd=F, outline=F, notch=T)
axis(1)
axis(2, labels=rev(c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100", "110", "120", "130", "140", "150")), at=rev(1:15))
#box()
mtext(expression("g Organic CO"[2]), side=1, line=2)
mtext("Depth (cm)", side=2, outer = T)

plot(x = cm.n[1:15], y = 1:15, ylim=rev(c(1,15)), type="l", lwd=2, xlab="n", ylab="depth increment (10 cm)")

ggplot(data=cm.cov.df, aes(x,y,fill=sqrt(z))) +
  geom_tile() +
  scale_fill_gradientn(name=expression(sqrt("Variance-Covariance")), colours = rainbow(5)) + 
  scale_y_reverse() + 
  ylab("Depth Interval (10 cm)") +
  xlab("Depth Interval (10 cm)") +
  theme_minimal() 

cm.cor.df <- data.frame(x = cm.df.x, y = cm.df.y, z = cm.df.cor)

ggplot(data=cm.cor.df, aes(x,y,fill=z)) +
  geom_tile() +
  scale_fill_gradientn(name="Correlation", colours = wes_palette("Zissou")) +
  scale_y_reverse()




# say I would like to have my regular R graphic in top-right quadrant
par(mfrow = c(2,2), mar=c(0,0,0,0), oma=c(0,0,0,0))

# leave top-left quadrant empty!
plot.new()

# plot regular R graphic in top-right quadrant
plot(seq(1:10), seq(1:10), pch = 20)


# https://stackoverflow.com/questions/14124373/combine-base-and-ggplot-graphics-in-r-figure-window
## the last one is the current plot
plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
require(ggplot2)
acz <- acf(y, plot=F)
acd <- data.frame(lag=acz$lag, acf=acz$acf)
p <- ggplot(acd, aes(lag, acf)) + geom_area(fill="grey") +
  geom_hline(yintercept=c(0.05, -0.05), linetype="dashed") +
  theme_bw()+labs(title= "Autocorrelation\n")+
  ## some setting in the title to get something near to the other plots
  theme(plot.title = element_text(size = rel(1.4),face ='bold'))
print(p,vp = vp1)        ## suggested by @bpatiste
