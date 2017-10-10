library(foreign)
mhhws_pal <- read.dbf(paste(getwd(), "/data/pMHHWSforCCAPpal.dbf", sep=""))

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
target_x <-seq(mu - (5*sigma), mu + (5*sigma), by = 100)
target_y <- c()
for (j in 1:length(target_x)) { target_y <- c(target_y, dnorm(target_x[j], mu, sigma)) }

plot(target_x, target_y, type="l", xlab="Pixels (n)", ylab="probability density", main="Palustrine Wetlands < MHHWS (2006-2010)")

