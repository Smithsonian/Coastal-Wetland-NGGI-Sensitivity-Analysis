# All the scripts needed for uncertainty and sensitivity figures

# load assumptions
{
  classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
                 'Cultivated', 'Pasture/Hay', 
                 'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
                 'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
                 'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
                 'Unconsolidated Shore', 'Bare Land', 'Water', 
                 'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
                 'Snow/Ice')
  
  palustrineWetlands <- c('Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland')
  estuarineWetlands <- c('Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland')
  
  abbrevs <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BAR', 'OW', 'PAB', 'EAB', 'SNOW')
  
  soilLossEvents <- c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
                      'Cultivated', 'Pasture/Hay',
                      'Unconsolidated Shore', 'Water',
                      'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed')
  
  forestVeg <- c('Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 
                 'Palustrine Forested Wetland',
                 'Estuarine Forested Wetland'
  )
  
  scrubShrubVeg <- c('Scrub/Shrub',
                     'Palustrine Scrub/Shrub Wetland',
                     'Estuarine Scrub/Shrub Wetland'
  )
  
  emergentVeg <- c('Cultivated', 'Pasture/Hay', 
                   'Grassland',
                   'Palustrine Emergent Wetland',
                   'Estuarine Emergent Wetland'
  )
  
  nonVeg <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space',
             'Unconsolidated Shore', 'Bare Land', 'Water', 
             'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
             'Tundra',
             'Snow/Ice'
  )
  
  
  est.loss.cats <-c()
  est.stable.gain.cats <-c()
  pal.loss.cats <-c()
  pal.stable.gain.cats <-c()
  
  for (i in 1:length(classOrder)) {
    for (j in 1:length(classOrder)) {
      if ( (classOrder[i] %in% c(estuarineWetlands, palustrineWetlands)) | (classOrder[j] %in% c(estuarineWetlands, palustrineWetlands)))
        if (classOrder[j] %in% estuarineWetlands) {
          est.stable.gain.cats <-c(est.stable.gain.cats, paste(abbrevs[i], "_", abbrevs[j], ".change.in.tonnes.CO2", sep=""))
        } else if  (classOrder[j] %in% palustrineWetlands) {
          pal.stable.gain.cats <-c(pal.stable.gain.cats, paste(abbrevs[i], "_", abbrevs[j], ".change.in.tonnes.CO2", sep=""))
        } else if (classOrder[i] %in% estuarineWetlands) {
          est.loss.cats <-c(est.loss.cats, paste(abbrevs[i], "_", abbrevs[j], ".change.in.tonnes.CO2", sep=""))
        } else if (classOrder[i] %in% palustrineWetlands) {
          pal.loss.cats <-c(pal.loss.cats, paste(abbrevs[i], "_", abbrevs[j], ".change.in.tonnes.CO2", sep=""))
        }
    }
  }
  
}

# uncertainty figure for SGWP/SGCP
{
  coastalNGGI.savedIterations <- read.csv("data/outputTables/MonteCarloResults1/coastalNGGI.savedIterations.csv")
  dev.off()
  pdf("figs/Uncertainty Analysis Pal Est Loss Stable Total SGWP.pdf", 4, 5)
  par(mfrow=c(1,1), mar=c(2,1,2,1), oma=c(3,3,0,0), family = "ArialMT")
  m <- matrix(c(1,3,5,2,4,5), ncol = 2)
  layout(mat=m)
  #layout.show(n=5)
  
  est.loss.iter <- rowSums(coastalNGGI.savedIterations[est.loss.cats]) / 1E6
  est.stable.gain.iter <- rowSums(coastalNGGI.savedIterations[est.stable.gain.cats]) / 1E6
  pal.loss.iter <- rowSums(coastalNGGI.savedIterations[pal.loss.cats]) / 1E6
  pal.stable.gain.iter <- rowSums(coastalNGGI.savedIterations[pal.stable.gain.cats]) / 1E6
  
  hist.x.range <- range(c(est.loss.iter, est.stable.gain.iter, pal.loss.iter, pal.stable.gain.iter))
  target_breaks <- seq(hist.x.range[1] - 2.5, hist.x.range[2] + 2.5, by = 5)
  hist(est.stable.gain.iter, xlim=hist.x.range, main="Estuarine Stable/Gains", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(est.loss.iter, xlim=hist.x.range, main="Estuarine Losses", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(pal.stable.gain.iter, xlim=hist.x.range, main="Palustrine Stable/Gains", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(pal.loss.iter, xlim=hist.x.range, main="Palustrine Losses", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  
  hist.x.range.2 <- range(50, coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6)
  target_breaks <- seq(hist.x.range.2[1] - 2.5, hist.x.range.2[2] + 2.5, by = 5)
  
  hist(coastalNGGI.savedIterations$total.tonnes.CO2 / 1E6, main="Total Gains/Losses", xlab="", breaks=target_breaks, col="grey")
  
  mtext(expression(paste("Million Tonnes CO"[2], " (- emission / + storage)")), side=1, line=1.5, outer=T)
  mtext("frequency", side=2, line=1.5, outer=T)
  #mtext(paste("Uncertainty Analysis (", toString(n.iterations), " iterations)", sep=""), side=3, line=1.5, outer=T)
  #mtext(expression(paste("All Coastal Wetlands - "^210, "Pb Carbon Burial")), side=3, line=0.5, outer=T)
  abline(v=0, lty=2, lwd=2, col="darkred")
  
  dev.off()
}

# Sensitivity analysis fig
{
  sensitivityAnalysisDF <- read.csv("data/outputTables/SensitivityAnalysisResults.csv")
  dev.off()
  pdf("figs/Sensitivity Analysis Top 10.pdf", 5, 4)
  # Make plots to summarize the sensitivity analysis
  sensitivityAnalysisDF_topTen <- sensitivityAnalysisDF[1:10,]
  par(mar=c(4,9,1,1), oma=c(0,0,0,0), family="ArialMT")
  barplot(sensitivityAnalysisDF_topTen$effectTonnesCO2 * 1E-6, 
          horiz = T, ylim=rev(c(1,12)), 
          names.arg=sensitivityAnalysisDF_topTen$parameter,
          xlab="",
          main="",
          las=1)
  mtext("Sensitivity of Total NGGI to Parameter", 1, line=2, outer=F)
  mtext(expression(paste("(Million Tonnes CO"[2], ")")), 1, line=3, outer=F)
  dev.off()
  
  pdf("figs/Sensitivity Analysis Top 20.pdf", 5, 6)
  par(mar=c(4,9,1,1), oma=c(0,0,0,0), family="ArialMT")
  sensitivityAnalysisDF_topTwenty <- sensitivityAnalysisDF[1:20,]
  barplot(sensitivityAnalysisDF_topTwenty$effectTonnesCO2 * 1E-6, 
          horiz = T, ylim=rev(c(1,24)), 
          names.arg=sensitivityAnalysisDF_topTwenty$parameter,
          xlab="",
          main="",
          las=1)
  mtext("Sensitivity of Total NGGI to Parameter", 1, line=2, outer=F)
  mtext(expression(paste("(Million Tonnes CO"[2], ")")), 1, line=3, outer=F)
  dev.off()
}

# Uncertainty fig. for GWP
{
  coastalNGGI.savedIterations.2 <- read.csv("data/outputTables/MonteCarloResults2/coastalNGGI.savedIterations.csv")
  dev.off()
  pdf("figs/Uncertainty Analysis Pal Est Loss Stable Total GWP.pdf", 4, 5)
  par(mfrow=c(1,1), mar=c(2,1,2,1), oma=c(3,3,0,0), family = "ArialMT")
  m <- matrix(c(1,3,5,2,4,5), ncol = 2)
  layout(mat=m)
  #layout.show(n=5)
  
  est.loss.iter.2 <- rowSums(coastalNGGI.savedIterations.2[est.loss.cats]) / 1E6
  est.stable.gain.iter.2 <- rowSums(coastalNGGI.savedIterations.2[est.stable.gain.cats]) / 1E6
  pal.loss.iter.2 <- rowSums(coastalNGGI.savedIterations.2[pal.loss.cats]) / 1E6
  pal.stable.gain.iter.2 <- rowSums(coastalNGGI.savedIterations.2[pal.stable.gain.cats]) / 1E6
  
  #hist.x.range.2 <- range(c(est.loss.iter.2, est.stable.gain.iter.2, pal.loss.iter.2, pal.stable.gain.iter.2))
  
  target_breaks <- seq(hist.x.range[1] - 3, hist.x.range[2] + 3, by = 5)
  hist(est.stable.gain.iter.2, xlim=hist.x.range, main="Estuarine Stable/Gains", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(est.loss.iter.2, xlim=hist.x.range, main="Estuarine Losses", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(pal.stable.gain.iter.2, xlim=hist.x.range, main="Palustrine Stable/Gains", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  hist(pal.loss.iter.2, xlim=hist.x.range, main="Palustrine Losses", col="grey", breaks = target_breaks)
  abline(v=0, lty=2, lwd=2, col="darkred")
  
  #hist.x.range.2 <- range(50, coastalNGGI.savedIterations.2$total.tonnes.CO2 / 1E6)
  target_breaks <- seq(hist.x.range.2[1] - 2.5, hist.x.range.2[2] + 2.5, by = 5)
  
  hist(coastalNGGI.savedIterations.2$total.tonnes.CO2 / 1E6, main="Total Gains/Losses", xlab="", breaks=target_breaks, col="grey")
  
  mtext(expression(paste("Million Tonnes CO"[2], " (- emission / + storage)")), side=1, line=1.5, outer=T)
  mtext("frequency", side=2, line=1.5, outer=T)
  #mtext(paste("Uncertainty Analysis (", toString(n.iterations), " iterations)", sep=""), side=3, line=1.5, outer=T)
  #mtext(expression(paste("All Coastal Wetlands - "^210, "Pb Carbon Burial")), side=3, line=0.5, outer=T)
  abline(v=0, lty=2, lwd=2, col="darkred")
  dev.off()
}