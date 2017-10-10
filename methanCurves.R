methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
head(methane)
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
#methane <- subset(methane, CH4.flux > 0)

logTransform <- function(y) { 
  defined.min <- min(y)
  return(log(1+y-defined.min)) 
}

revLogTransform <- function(log.y, defined.min) { return(exp(log.y) + defined.min - 1) }

ch4min <- min(methane$CH4.flux)
logCH4 <- logTransform(methane$CH4.flux)
methane["CH4.ltransformed"] <- logCH4
hist(methane$CH4.ltransformed)

pal <- subset(methane, Salinity.ppt <5)
est <- subset(methane, Salinity.ppt >= 5)

hist(est$CH4.ltransformed)
est_lmean <- mean(est$CH4.ltransformed)
est_lsd <- sd(est$CH4.ltransformed)
est_lse <- sd(est$CH4.ltransformed) / sqrt(length(est$CH4.ltransformed))

(revLogTransform(est_lmean, ch4min))
(revLogTransform(est_lmean+pal_lse, ch4min))
(revLogTransform(est_lmean-pal_lse, ch4min))


hist(pal$CH4.ltransformed)
pal_lmean <- mean(pal$CH4.ltransformed)
pal_lsd <- sd(pal$CH4.ltransformed)
pal_lse <- sd(pal$CH4.ltransformed) / sqrt(length(pal$CH4.ltransformed))

(revLogTransform(pal_lmean, ch4min))
(revLogTransform(pal_lmean+pal_lse, ch4min))
(revLogTransform(pal_lmean-pal_lse, ch4min))

hist(methane$CH4.flux, probability = T, ylim=c(0,0.1), col="grey", xlab=expression(paste("Methane Emissions (gCH" [4], " m"^-2, " yr"^-1,")")), main="Knox's Methane Data (Est and Pal)")
y_target <- seq(-50,250.0001, by=1)
log.y <- ltransformed()
target_em <- dlnorm(y_target, est_lmean, est_lsd)
points(y_target, target_em, type="l", col="red", lwd=2)

target_pal <- dlnorm(y_target, pal_lmean, pal_lsd)
points(y_target, target_pal, type="l", col="blue", lwd=2)

par(mfrow=c(1,1))
hist(methane$CH4.flux, probability = T, col="grey", xlab=expression(paste("Methane Emissions (gCH" [4], " m"^-2, " yr"^-1,")")), main="Knox's Methane Data", breaks=seq(-10,300, by=5), ylim=c(0,0.45))
y_target <- seq(-10,300, by=0.1)
y_target_ltranform <-logTransform(y_target)

target_em <- dnorm(y_target_ltranform, est_lmean, est_lsd)
points(y_target, target_em, type="l", col="red", lwd=2)

#hist(pal$CH4.flux, probability = T, col="grey", xlab=expression(paste("Methane Emissions (gCH" [4], " m"^-2, " yr"^-1,")")), main="Knox's Methane Data (Pal)", nclass=25, ylim=c(0,0.38))
target_pal <- dnorm(y_target_ltranform, pal_lmean, pal_lsd)
points(y_target, target_pal, type="l", col="blue", lwd=2)

sim_Est <- c()
sim_Pal <- c()
for (i in 1:10000) {
  randEst <- revLogTransform(rnorm(nrow(est), est_lmean, est_lsd), ch4min)
  sim_Est <- c(sim_Est, mean(randEst))
  
  randPal <- revLogTransform(rnorm(nrow(pal), pal_lmean, pal_lsd), ch4min)
  sim_Pal <- c(sim_Pal, mean(randPal))
}

par(mfrow=c(1,2), oma=c(3,3,0,0), mar=c(1,1,1,1))
hist(sim_Est, col="grey", main="Estuarine (>= 5 ppt)", xlab="", xlim=c(min(sim_Est, sim_Pal), max(sim_Est, sim_Pal)))
hist(sim_Pal, col="grey", main="Palustrine (< 5 ppt)", xlab="", ylab="", xlim=c(min(sim_Est, sim_Pal), max(sim_Est, sim_Pal)))
mtext(text=expression(paste("Simulated Mean Emissions w/ 10,000 iter (gCH" [4], " m"^-2, " yr"^-1,")")), side=1, line=1.5, outer = T)
mtext(text="frequency", side=2, line=1.5, outer = T)