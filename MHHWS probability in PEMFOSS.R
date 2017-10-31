library(foreign)
mhhws_pal <- read.dbf(paste(getwd(), "/data/WetlandArea/Palustrine/pMHHWSforCCAPpal.dbf", sep=""))

# using poisson Binomial Distrubution Normal Approximation Method (Hong, 2013)
value <- mhhws_pal$Value / 10000 # tranform values
count <- mhhws_pal$Count

mu_vect <- c()
for (i in 1:length(value)) {
  mu_temp <- value[i] * count[i]
  mu_vect <- c(mu_vect, mu_temp)
}
mu <- sum(mu_vect)

var_vect <- c()
for (i in 1:length(value)) {
  var_temp <- count[i] * (value[i] * (1-value[i]))
  var_vect <- c(var_vect, var_temp)
}
sigma = sqrt(sum(var_vect))

# pixels to m2 to ha to million ha
mu_ha <- mu * 900 / 10000
sigma_ha <- sigma * 900 / 10000

(mu_mHa <- mu_ha / 1000000)
(sigma_mHa <- sigma_ha / 1000000)

# plot distribution of total pixels
target_x <-seq(mu - (4*sigma), mu + (4*sigma), by = 1)
target_x.mha <- target_x * 900 / 10000 / 1E6
target_y <- c()
for (j in 1:length(target_x)) { target_y <- c(target_y, dnorm(target_x[j], mu, sigma)) }

par(mar=c(1,1,1,1), oma=c(3,3,3,1))
plot(target_x.mha, target_y, type="l", xlab="Pixels (n)", ylab="probability density", main=expression(paste("Palustrine Wetlands < MHHWS")), lwd=2, axes=F)
axis(1)
axis(2)
#box()
mtext("Area (million ha)", side=1, line=1.5, outer=T)
mtext("Density", side=2, line=1.5, outer=T)
mtext(expression(paste(mu, "=", sum(paste(n[i], phi[i]), paste("i=", 0), 1), "   ", sigma^2, "=",  sum(paste(n[i], phi[i], "(1-", phi[i],")"), paste("i=", 0), 1))), side=3, line=1.5)
