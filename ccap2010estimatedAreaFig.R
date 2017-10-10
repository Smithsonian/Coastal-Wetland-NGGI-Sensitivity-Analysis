# making color charts for CCAP estimated area
ccap <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/2010Classes/CCAP2010DataOutput.csv", sep=""))
cnc <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/CNC/cnc2006to2010DataOutput.csv", sep=""))

ccapCol <- read.csv(paste(getwd(), "/data/WetlandArea/CCAP/ccapCols.csv", sep=""))

ccapColVect <- c()
for (i in 1:nrow(ccapCol)) {
  ccapColVect <- c(ccapColVect, rgb(ccapCol$red[i], ccapCol$green[i], ccapCol$blue[i], max=255))
}

AreaUpperSE <- ccap$estimatedArea + ccap$estimatedAreaSE
AreaLowerSE <- ccap$estimatedArea - ccap$estimatedAreaSE
AreaUpperCI <- ccap$estimatedArea + ccap$estimatedAreaCI
AreaLowerCI <- ccap$estimatedArea - ccap$estimatedAreaCI

ccap["AreaUpperSE"] <- AreaUpperSE
ccap["AreaLowerSE"] <- AreaLowerSE
ccap["AreaUpperCI"] <- AreaUpperCI
ccap["AreaLowerCI"] <- AreaLowerCI
ccap["ccapColVect"] <- ccapColVect

#ccap <- ccap[order(ccap$estimatedArea),]

#plot(ccap$originalArea, ccap$estimatedArea, pch=19, col=ccap$ccapColVect, xlab="Mapped Area (pixels)", ylab="Estimated Area (pixels)")
m <- matrix(c(1,2), nrow=1)

par(oma=c(3,3,0,0), mar=c(1,1,1,1))
layout(mat=m, widths = c(4,1))
layout.show(n=2)

target_y <- seq(1, nrow(ccap), by=1)
plot(target_y, ccap$perPixelScaler, type="h", col=ccap$ccapColVect, xlab="Classifications", ylab="Estimated:Mapped Area", ylim=range(ccap$perPixelScaler + ccap$scalerSE, ccap$perPixelScaler - ccap$scalerSE), lwd=4,axes=F, main="2010")
segments(target_y, ccap$perPixelScaler + ccap$scalerSE, target_y, ccap$perPixelScaler - ccap$scalerSE, col="black")
abline(h=1, lty=2)
axis(2)
axis(1, at=target_y, labels=ccap$X, las=2)

plot(c(2,3), cnc$perPixelScaler, type="h", col=c("black", "red"), xlab="", ylab="", ylim=range(ccap$perPixelScaler + ccap$scalerSE, ccap$perPixelScaler - ccap$scalerSE), lwd=4,axes=F, main="2006-2010", xlim=c(0,4))
abline(h=1, lty=2)
axis(2)
axis(1, at=c(2,3), labels=c("Stable", "Change"), las=2)
segments(c(2,3), cnc$perPixelScaler + cnc$scalerSE, c(2,3), cnc$perPixelScaler - cnc$scalerSE, col=c("red", "black"))

mtext("Estimated Area:Mapped Area", side=2, outer=T, line=1.5)
