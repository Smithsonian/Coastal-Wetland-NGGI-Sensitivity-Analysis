### Let's make a graph for CH4 emissions to show the different splits we could make

methane <- read.csv(paste(getwd(), "/data/Methane/Methane Synthesis Knox.csv", sep=""))
names(methane) <- c("Site.Name", "Location", "Region", "Saliniity.class", "year", "Method", "Salinity.ppt", "CH4.flux", "Reference")

pch_vect <- c()
for (i in 1:nrow(methane)) {
  if (methane$Method[i]=="SC") {
    pch_vect <- c(pch_vect, 22) 
  } else {
    pch_vect <- c(pch_vect, 23)
  }
}
dev.off()
pdf("figs/Methane v Salinity Ideals v Reality.pdf", 4, 4)
par(mar=c(4,4,1,1), family = "ArialMT")
plot(methane$Salinity.ppt, methane$CH4.flux, pch=pch_vect, bg="darkgrey", xlab="", ylab="", cex=1.75)

abline(v=5, lwd=2, col="darkred")
abline(v=15, lty=2, lwd=2, col="darkred")
legend("topright", c("Static Chamb.", "Eddy Cov.", "C-CAP", "IPCC"), bty="n", lty=c(NA,NA, 1,2), pch=c(22,23,NA,NA), col=c("black", "black", "darkred", "darkred"), lwd=c(NA,NA, 2,2))
mtext(side=1, line=2.5, "Salinity (ppt)")
mtext(expression(paste("Methane Emissions (gCH"[4]," m"^-2, " yr"^-1,")")), side=2, line=2.5)
dev.off()