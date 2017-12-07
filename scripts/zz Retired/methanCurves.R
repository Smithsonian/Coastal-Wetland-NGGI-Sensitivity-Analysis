methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
head(methane)
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")
#methane <- subset(methane, CH4.flux > 0)

# using Scott Neubauer's CO2 Eq. for SGWP and SGCP
generateMethaneSGPs <- function(ch4) {
  if (ch4 < 0) { return(ch4 * 203)
  } else { return(ch4 * 45)}
}

ch4_co2_eq <- mapply(generateMethaneSGPs, methane$CH4.flux)

#logTransform <- function(y, defined.min=min(y)) { return(log(1+y-defined.min)) }

#revLogTransform <- function(log.y, defined.min) { return(exp(log.y) + defined.min - 1) }

#ch4_co2_eq.min <- min(ch4_co2_eq)
methane["ch4_co2_eq"] <- ch4_co2_eq
est <- subset(methane, Salinity.ppt >= 5)
est_mean <- mean(est$ch4_co2_eq)
est_sd <- sd(est$ch4_co2_eq)

pal <- subset(methane, Salinity.ppt <5)
log.ch4_co2_eq <- log(pal$ch4_co2_eq)
pal["log.ch4_co2_eq"] <- log.ch4_co2_eq

pal_lmean <- mean(pal$log.ch4_co2_eq)
pal_lsd <- sd(pal$log.ch4_co2_eq)

(exp(pal_lmean))
(exp(pal_lmean+pal_lse))
(exp(pal_lmean-pal_lse))


par(oma=c(3,3,3,0), mar=c(1.5,1.5,1.5,1.5))

m <- matrix(c(1,2), nrow=1)
layout(mat=m, widths = c(1,2))
layout.show(n=2)

hist(est$ch4_co2_eq, probability = T, col="grey", xlab="", main="Estuarine (>=5ppt)", breaks= 10)
y_target <- seq(min(est$ch4_co2_eq), max(est$ch4_co2_eq), by=1)
target_em <- dnorm(y_target, est_mean, est_sd)
points(y_target, target_em, type="l", col=rgb(179,13,179, max=255), lwd=2)

y_target <- seq(0, max(pal$ch4_co2_eq), by=1)
log.y <- log(y_target)
target_pal <- dlnorm(y_target, pal_lmean, pal_lsd)
hist(pal$ch4_co2_eq, probability = T, col="grey", xlab="", main="Palustrine (<5ppt)", ylim=c(0, max(target_pal)), breaks = 10)
points(y_target, target_pal, type="l", col=rgb(243,13,243, max=255), lwd=2)

mtext(expression(paste("Methane Emissions (gCH" [4], " m"^-2, " yr"^-1, "-CO" [2], " eq.",")")), side=1, line=1.5, outer = T)
mtext("Knox's Methane Data", side=3, line=1.5, outer = T)
mtext("Neubauer's Conversion Factors", side=3, line=0.5, outer = T)

mtext("Density", side=2, line=1.5, outer = T)

sim_Est <- c()
sim_Pal <- c()
for (i in 1:10000) {
  randEst <- rnorm(nrow(est), est_mean, est_sd)
  sim_Est <- c(sim_Est, mean(randEst))
  
  randPal <- rlnorm(nrow(pal), pal_lmean, pal_lsd)
  sim_Pal <- c(sim_Pal, mean(randPal))
}
