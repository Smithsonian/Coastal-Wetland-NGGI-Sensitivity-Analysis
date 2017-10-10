biomass <- read.csv(paste(getwd(), "/data/Biomass/BiomassSamples.csv", sep=""))
biomass <- subset(biomass, biomass_gm > 0)
biomass <- biomass[order(biomass$biomass_gm),]

tail(biomass)

iva <- subset(biomass, (sp1 == "Iva frutescens") & (pc_sp1 >= 0.50))
em <- subset(biomass, (! ( (sp1 == "Iva frutescens") & (pc_sp1 >= 0.50))))

hist(em$biomass_gm, probability = T)
hist(log(em$biomass_gm), probability = T)

em_lmean <- (mean(log(em$biomass_gm)))
em_lsd <- (sd(log(em$biomass_gm)))
em_lse <- (sd(log(em$biomass_gm)) / sqrt(length(em$biomass_gm)))

hist(iva$biomass_gm)
hist(log(iva$biomass_gm), probability = T)

ss_lmean <- (mean(log(iva$biomass_gm)))
ss_lsd <- (sd(log(iva$biomass_gm)))
ss_lse <- (sd(log(iva$biomass_gm)) / sqrt(length(iva$biomass_gm)))

hist(biomass$biomass_gm, probability = T, ylim=c(0,0.002), col="grey", xlab=expression(paste("Biomass (g m"^-2, ")")), main="Kristen's Biomass Data (EM and SS)")
y_target <- seq(1,5000, by=1)
target_em <- dlnorm(y_target, em_lmean, em_lsd)
points(y_target, target_em, type="l", col="purple", lwd=2)

target_ss <- dlnorm(y_target, ss_lmean, ss_lsd)
points(y_target, target_ss, type="l", col="pink", lwd=2)

sim_EM <- c()
sim_SS <- c()
for (i in 1:10000) {
  randEM <- rlnorm(nrow(em), em_lmean, em_lsd)
  sim_EM <- c(sim_EM, mean(randEM))
  
  randSS <- rlnorm(nrow(iva), ss_lmean, ss_lsd)
  sim_SS <- c(sim_SS, mean(randSS))
}

par(mfrow=c(1,2), oma=c(3,3,0,0), mar=c(1,1,1,1))
hist(sim_EM, col="grey", main="Emergent Veg. (n=2345)", xlab="", xlim=c(min(sim_EM, sim_SS), max(sim_EM, sim_SS)))
hist(sim_SS, col="grey", main="Scrub/Shrub (n=8)", xlab="", ylab="", xlim=c(min(sim_EM, sim_SS), max(sim_EM, sim_SS)))
mtext(text=expression(paste("Simulated Mean Biomass w/ 10,000 iter (gC m"^-2,")")), side=1, line=1.5, outer = T)
mtext(text="frequency", side=2, line=1.5, outer = T)

(quantile(sim_EM, c(0.025, 0.5, 0.975)))
(quantile(sim_SS, c(0.025, 0.5, 0.975)))

