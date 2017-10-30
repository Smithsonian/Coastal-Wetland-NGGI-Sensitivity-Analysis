# Fished values from probibalistic MHHWS Map

navd88 <- 2.370
navd88_rmse <- 0.205
navd88_me <- 0.173
corrected_navd88 <- navd88 - navd88_me

mhhw_navd88 <- 1.937
mhhw_navd88_datumError <- 0.00977
mhhw_navd88_krigError <- 0.121

mhhws_mhhw <- 0.2
mhhws_mhhw_datumError <- 0.0275
mhhws_mhhw_krigError <- 0.00979

#target_y <- seq(1.5,3, by=0.01)

propegated_uncertainty <- sqrt(navd88_rmse^2 + mhhw_navd88_datumError^2 + mhhws_mhhw_krigError^2 + mhhws_mhhw_datumError^2 + mhhws_mhhw_krigError^2)

totalRange <- seq(corrected_navd88 - 3 * propegated_uncertainty, corrected_navd88 + 3* propegated_uncertainty, by=0.01)
navd88_density <- dnorm(totalRange, corrected_navd88, navd88_rmse)

par(mar=c(1,1,1,0), oma=c(3,3,3,0))
m <- matrix(c(1:6), nrow=1)
layout(mat=m, widths = c(2,1,1,1,1,2))
layout.show(n=6)

plot(navd88_density, totalRange, type="l", axes=F, ylim=range(totalRange))
points(max(navd88_density),navd88, pch="-", cex=3)
points(max(navd88_density),corrected_navd88, pch="-", cex=3)
segments(max(navd88_density), navd88, max(navd88_density),corrected_navd88, lwd=1)

text(max(navd88_density-.5), navd88 + 0.20, "LiDAR Bias")

axis(2)
axis(1)
abline(h=navd88, lty=2)
abline(h=navd88-navd88_me, lty=2)

mhhw_density.1 <- dnorm(target_y, mhhw_navd88, mhhw_navd88_datumError)
plot(mhhw_density.1, target_y, type="l", axes=F)
#abline(h=mhhw_navd88, lty=2)

mhhw_density.2 <- dnorm(target_y, mhhw_navd88, mhhw_navd88_krigError)
plot(mhhw_density.2, target_y, type="l", axes=F)

#abline(h=mhhw_navd88, lty=2)
#segments(max(mhhw_density.2), mhhw_navd88, max(mhhw_density.2), mhhw_navd88 + mhhws_mhhw)

mhhws_density.1 <- dnorm(target_y, mhhw_navd88 + mhhws_mhhw, mhhws_mhhw_datumError)
plot(mhhws_density.1, target_y, type="l", axes=F)

mhhws_density.2 <- dnorm(target_y, mhhw_navd88 + mhhws_mhhw, mhhws_mhhw_krigError)
plot(mhhws_density.2, target_y, type="l", axes=F)

propegated.density <- dnorm(target_y, navd88 - navd88_me, propegated_uncertainty)
plot(propegated.density, target_y, type="l", axes=F)

mtext("Measurement", side=3, line=1.5, outer=T, at=0.1)
mtext("MHHW", side=3, line=1.5, outer=T, at=0.33)
mtext("MHHWS", side=3, line=1.5, outer=T, at=0.56)
mtext("Propegated Uncertainty", side=3, line=1.5, outer=T, at=0.85)
