# Fished values from probibalistic MHHWS Map

navd88 <- 2.370
navd88_rmse <- 0.205
navd88_me <- 0.173
corrected_navd88 <- navd88 - navd88_me

mhhw_navd88 <- 1.937
mhhw_navd88_datumError <- 0.00977
mhhw_navd88_krigError <- 0.121

mhhws_mhhw <- 0.2
mhhws_navd <- mhhw_navd88 + mhhws_mhhw
mhhws_mhhw_datumError <- 0.0275
mhhws_mhhw_krigError <- 0.00979

#target_y <- seq(1.5,3, by=0.01)

propegated_uncertainty <- sqrt(navd88_rmse^2 + mhhw_navd88_datumError^2 + mhhws_mhhw_krigError^2 + mhhws_mhhw_datumError^2 + mhhws_mhhw_krigError^2)

totalRange <- seq(corrected_navd88 - 4 * propegated_uncertainty, corrected_navd88 + 4 * propegated_uncertainty, by=0.01)
navd.y.1 <- seq(corrected_navd88 - 4 * navd88_rmse, corrected_navd88 + 4 * navd88_rmse, by=0.01)
navd88_density <- dnorm(navd.y.1, corrected_navd88, navd88_rmse)

par(mar=c(0,0,5,0), oma=c(4,4,1,0))
m <- matrix(c(1:6), nrow=1)
layout(mat=m, widths = c(2,1,1,1,1,2))
layout.show(n=6)

plot(navd88_density, navd.y.1, type="l", axes=F, ylim=range(totalRange), lwd=2, main=expression(paste("LiDAR\nError")))
#points(max(navd88_density),navd88, pch="-", cex=3)
#points(max(navd88_density),corrected_navd88, pch="-", cex=3)
#segments(max(navd88_density), navd88, max(navd88_density),corrected_navd88, lwd=1)
#text(max(navd88_density-.5), navd88 + 0.20, "LiDAR Bias")

axis(2)
axis(1)
abline(h=navd88, col="black", lty=1)
abline(h=corrected_navd88, col="black", lty=2)

mhhw_y.1 <- seq(mhhw_navd88 - mhhw_navd88_datumError * 4, mhhw_navd88 + mhhw_navd88_datumError * 4, by=0.01)
mhhw_density.1 <- dnorm(mhhw_y.1, mhhw_navd88, mhhw_navd88_datumError)
plot(mhhw_density.1, mhhw_y.1, type="l", axes=F, lwd=2,  ylim=range(totalRange), main=expression(paste("Datum")))
abline(h=mhhw_navd88, col="black")
axis(3)

mhhw_y.2 <- seq(mhhw_navd88 - mhhw_navd88_krigError * 4, mhhw_navd88 + mhhw_navd88_krigError * 4, by=0.01)
mhhw_density.2 <- dnorm(mhhw_y.2, mhhw_navd88, mhhw_navd88_krigError)
plot(mhhw_density.2, mhhw_y.2, type="l", axes=F, lwd=2, ylim=range(totalRange), main=expression(paste("Gauge\ndistance")))
axis(1)
abline(h=mhhw_navd88, col="black")
#segments(max(mhhw_density.2), mhhw_navd88, max(mhhw_density.2), mhhw_navd88 + mhhws_mhhw)

mhhws_y.1 <- seq(mhhws_navd - mhhws_mhhw_datumError * 4, mhhws_navd + mhhws_mhhw_datumError * 4, by=0.01)
mhhws_density.1 <- dnorm(mhhws_y.1, mhhws_navd, mhhws_mhhw_datumError)
plot(mhhws_density.1, mhhws_y.1, type="l", axes=F, lwd=2, ylim=range(totalRange), main=expression(paste("Datum")))
abline(h=mhhws_navd, col="black")
axis(3)

mhhws_y.2 <- seq(mhhws_navd - mhhws_mhhw_krigError * 4, mhhws_navd + mhhws_mhhw_krigError * 4, by=0.01)
mhhws_density.2 <- dnorm(mhhws_y.2, mhhws_navd, mhhws_mhhw_krigError)
plot(mhhws_density.2, mhhws_y.2, type="l", axes=F, lwd=2, ylim=range(totalRange), main=expression(paste("Gauge\ndistance")))
axis(1)
abline(h=mhhws_navd, col="black")

propegated.density <- dnorm(totalRange, corrected_navd88, propegated_uncertainty)
plot(propegated.density, totalRange, type="l", axes=F, lwd=2, main=expression(paste("Propegated\nUncertainty")))
axis(3)
mtext("NAVD88", side=3, line=-.5, outer=T, at=0.12)
mtext("MHHW", side=3, line=-.5, outer=T, at=0.38)
mtext("MHHWS", side=3, line=-.5, outer=T, at=0.62)
mtext("Total", side=3, line=-.5, outer=T, at=0.88)
mtext("Probability Density", side=1, line=2.5, outer=T)
mtext("Elevation (m)", side=2, line=2.5, outer=T)

poly_y1 <- totalRange[totalRange<mhhws_navd]
poly_x1 <- propegated.density[totalRange<mhhws_navd]

poly_y2 <- c(poly_y1, rev(poly_y1))
poly_x2 <- c(poly_x1, rep(0, length(poly_x1)))
polygon(poly_x2, poly_y2, col="grey")


# 
par(mar=c(1,1,0,1), oma=c(3,3,0,0))
m <- matrix(c(1:1), nrow=1)
layout(mat=m)
layout.show(n=1)

plot(navd88_density, navd.y.1, type="l", axes=F, ylim=range(totalRange), lwd=2)
#points(max(navd88_density),navd88, pch="-", cex=3)
#points(max(navd88_density),corrected_navd88, pch="-", cex=3)
#segments(max(navd88_density), navd88, max(navd88_density),corrected_navd88, lwd=1)
#text(max(navd88_density-.5), navd88 + 0.20, "LiDAR Bias")

axis(2)
axis(1)
abline(h=navd88, col="black", lty=1)
abline(h=corrected_navd88, col="black", lty=2)
mtext("Probability Density", side=1, line=1.5, outer=T)
mtext("Elevation (m)", side=2, line=1.5, outer=T)


