# Messing Around w/ Meng's CAR Data
# 26 September 2017
car <- read.csv(paste(getwd(), "/data/MengReview/PbandcsData_170926.csv", sep=""))
pb <- car$delSOC1[! is.na(car$delSOC1)]
cs <- car$delSOC2[! is.na(car$delSOC2) ]
cs <- cs[cs <= 1500]

par(mfrow=c(1,2))

hist(cs, breaks = 15, col="red", density=10, xlab=expression(paste("CAR (gC ", m^-2, " ", yr^-1, ")")), probability = T, main="Original Data", ylim=c(0,0.008))
hist(pb, breaks = 15, col = "blue", density=10, angle = -45, add=T, probability = T)

target_x <- seq(1, max(c(cs, pb)), by=5)
target_y1 <- dlnorm(target_x, mean(log(cs)), sd(log(cs)))
target_y2 <- dlnorm(target_x, mean(log(pb)), sd(log(pb)))

points(target_x, target_y1, col="darkred", type="l", lwd=2)
points(target_x, target_y2, col="darkblue", type="l", lwd=2)
legend("topright", c(expression(paste({}^137, "Cs")),
                   expression(paste({}^210, "Pb")))
      , text.col=c("darkred", "darkblue"), bty="n")


hist(log(cs), breaks = seq(0,8, by=0.25), col="red", density=10, xlab=expression(paste("log(CAR [gC ", m^-2, " ", yr^-1, "])")), probability = T, main="Log-Normal", ylim=c(0,0.7))
hist(log(pb), breaks = seq(0,8, by=0.25), density = 10, col = "blue", angle = -45, add=T, probability = T)

alpha1 <- log(median(cs))
beta1 <- sqrt(log( (var(cs) ^ 2 + mean(cs) ^ 2) / mean(cs) ^ 2))

alpha2 <- log(median(pb))
beta2 <- sqrt(log( (var(pb) ^ 2 + mean(pb) ^ 2) / mean(pb) ^ 2))

target_x <- seq(1, max(c(cs, pb)), by=5)
target_y1 <- dnorm(log(target_x), log(median(cs)), sd(log(cs)))
target_y2 <- dnorm(log(target_x), log(median(pb)), sd(log(pb)))

points(log(target_x), target_y1, col="darkred", type="l", lwd=2)
points(log(target_x), target_y2, col="darkblue", type="l", lwd=2)
legend("topleft", c(expression(paste({}^137, "Cs")),
                    expression(paste({}^210, "Pb")))
                     , text.col=c("darkred", "darkblue"), bty="n")

pb_se <- sd(log(pb)) / sqrt(length(pb))
(exp(mean(log(pb))))
(exp(mean(log(pb) + pb_se)))
(exp(mean(log(pb) - pb_se)))

# compounded uncertainty for area 10 +/- 1 pixel
target_y3 <- dnorm(seq(0,20, by=0.1), 10, 1)
plot(seq(0,20, by=0.1), target_y3, type="l", lwd=2)
compounded_mean = log(10) + mean(log(pb))
compounded_sd = sqrt((1 / 10) + (sd(log(pb))^2 / mean(log(pb))))

(exp(compounded_mean))
(exp(compounded_mean-compounded_sd))
(exp(compounded_mean+compounded_sd))
