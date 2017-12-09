# creating a bar chart for ccap classes and change events

# needed libraries
{ 
  library(foreign)
  library(ggplot2)
}

# load needed data files
{
  ccapCol <- read.csv("data/WetlandArea/CCAP/ccapCols.csv")
  ccap2010 <- read.dbf("data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf")
  palustrineMappedPixels <- read.csv("data/outputTables/MonteCarloResults1/palustrineMappedPixels.savedIterations.csv")
  ccap2010perPixelScalers <- read.csv("data/outputTables/MonteCarloResults1/ccap2010perPixelScalers.savedIterations.csv")
  cncPerPixelScalers <- read.csv("data/outputTables/MonteCarloResults1/cncPerPixelScalers.savedIterations.csv")
}

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
  
  abbrevs.2 <- c('HID', 'MID', 'LID', 'OSD',
               'CULT', 'PAST',
               'GRS', 'DEC', 'EVR', 'MIX', 'SS',
               'PFW', 'PSS', 'PEM', 'EFW', 'ESS', 'EEM',
               'UCS', 'BARE', 'OW', 'PAB', 'EAB', 'SNOW')
}

# first get all the estuarine pixels
# 2006 class, 2010 class, # pixles 0.025, # pixels 0.5, # pixels 0.975, R, G, B, Abbrev
class2006 <- c()
class2010 <- c()
abbrev.vect<-c()
cnc_col <- c()
i = 1
for (m in 1:length(classOrder)) {
  for (n in 1:length(classOrder)) {
    if ((classOrder[m] %in% c(palustrineWetlands, estuarineWetlands)) | (classOrder[n] %in% c(palustrineWetlands, estuarineWetlands))) {
      
      class2006 <- c(class2006, classOrder[m])
      class2010 <- c(class2010, classOrder[n])
      this.abbrev <- paste(abbrevs[m], "_", abbrevs[n], sep="")
      abbrev.vect<-c(abbrev.vect, this.abbrev)
      
      scaler.2010.iterations <- ccap2010perPixelScalers[abbrevs[n]]
      if (classOrder[m] == classOrder[n]) {
        scaler.cnc <- cncPerPixelScalers$No.Change
        cnc_col <- c(cnc_col, "darkgrey")
      } else {
        scaler.cnc <- cncPerPixelScalers$Change
        cnc_col <- c(cnc_col, "red")
      }
      
      # if it's estuarine pull from original CCAP table
      if ((classOrder[m] %in% c(estuarineWetlands)) | (classOrder[n] %in% c(estuarineWetlands))) {
        # scale by 2010 class
        mapped.pixel.count <- subset(ccap2010, (X2006_Clas == classOrder[m]) & (X2010_Clas == classOrder[n]))$Count[1] 
        mapped.pixel.count[is.na(mapped.pixel.count)] <- 0
      } else {
        if (this.abbrev %in% names(palustrineMappedPixels)) {
          mapped.pixel.count <- palustrineMappedPixels[this.abbrev] # if it's palustrine pull from probibalistic map
        } else {
          mapped.pixel.count <- 0
        }
      }
      # scale by 2010 class and change or no change
      scaled.pixel.count <- mapped.pixel.count * scaler.2010.iterations * scaler.cnc
      scaled.pixel.count.quantiles <- quantile(scaled.pixel.count[,1], c(0.025,0.5,0.975)) # summarize quantiles
      rgb2010 <- subset(ccapCol, class == abbrevs.2[n]) # get rgb for 2010 class
      if (i == 1) {
        q.df <- t(scaled.pixel.count.quantiles)
        rgb.df <- rgb2010
      } else {
        q.df <- rbind(q.df, t(scaled.pixel.count.quantiles))
        rgb.df <- rbind(rgb.df, rgb2010)
      }
      i = i + 1
    }
  }
}

col.vect <- c()
for (i in 1:nrow(rgb.df)) { col.vect <- c(col.vect, rgb(r=rgb.df$red[i], g=rgb.df$green[i], b=rgb.df$blue[i], maxColorValue = 255))}

bar.df <- as.data.frame(q.df) * 900 / 10000 / 1E6
names(bar.df) <- c("lower.ci", "median", "upper.ci")

bar.df["abbrevs"] <- gsub("_", "-", abbrev.vect)
bar.df['colors'] <- col.vect
bar.df['ncn.colors'] <- cnc_col

#bar.df['labs'] <- paste(bar.df$abbrevs, " (", round(bar.df$median * 900 / 10000 / 1E6, 2), ")", sep="")

bar.df <- bar.df[order(- bar.df['median']), ]


bar.sub.df.1 <- bar.df[1:13,]
the.rest.1 <- bar.df[14:nrow(bar.df),]
add.this.row.1 <- c(sum(the.rest.1$lower.ci), sum(the.rest.1$median), sum(the.rest.1$upper.ci), "Other", "grey", "white")
bar.sub.df.1b <- rbind(bar.sub.df.1, add.this.row.1)

dev.off()
pdf("figs/CCAP Area Breakdowns Bar.pdf", height=4.25, width=5)
par(mar=c(6,3.5,1,1), family="ArialMT")
barplot(as.numeric(bar.sub.df.1b$median), col=bar.sub.df.1b$ncn.colors, ylim=c(0, max(as.numeric(bar.sub.df.1b$upper.ci))), names.arg=bar.sub.df.1b$abbrevs, las=2)
df.bar <- barplot(as.numeric(bar.sub.df.1b$median), plot=F)
mtext("Estimated Area (Million Hectares)", side=2, line=2.5)
mtext("C-CAP and Coastal Lands (2006-2010)", side=3, line=0, font=2)
segments(df.bar, as.numeric(bar.sub.df.1b$lower.ci), df.bar, as.numeric(bar.sub.df.1b$upper.ci), lwd=2)
dev.off()

# how much OW to EM compared to EM to OW?
((as.numeric(bar.sub.df.1b[9,]$median[1]) + as.numeric(bar.sub.df.1b[10,]$median[1])) / 
    (as.numeric(bar.sub.df.1b[7,]$median[1]) + as.numeric(bar.sub.df.1b[8,]$median[1]))) * 100


# Histograms for key wetland classes estimated:mapped ratio
# EEM ESS EFO PEM 
# PFO PSS OW UCS
# EAB EAB # Change # No.Change 
dev.off()
pdf("figs/CCAP Estimated to Mapped Ratio Key Classes Monte Carlo.pdf", height=5, width=7)
m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11), byrow = T, nrow=3)
layout(mat = m)
par(mar=c(1.25,1,2.5,1), oma=c(3,2.75,0,0), family="ArialMT")
#layout.show(n=11)

x.range.hists <- range(ccap2010perPixelScalers$EEM, ccap2010perPixelScalers$ESS, ccap2010perPixelScalers$EFW,
                       ccap2010perPixelScalers$PEM, ccap2010perPixelScalers$PSS, ccap2010perPixelScalers$PFW,
                       ccap2010perPixelScalers$OW, cncPerPixelScalers$No.Change, cncPerPixelScalers$Change)
target.breaks <- seq(0.4, 2.5, by=0.025)

ccapCol.sub <- subset(ccapCol, class == 'EEM')
hist(ccap2010perPixelScalers$EEM, main="Estuarine Emergent", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'ESS')
hist(ccap2010perPixelScalers$ESS, main="Estuarine Scrub/Shrub", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'EFW')
hist(ccap2010perPixelScalers$EFW, main="Estuarine Forested", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PEM')
hist(ccap2010perPixelScalers$PEM, main="Palustrine Emergent", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PSS')
hist(ccap2010perPixelScalers$PEM, main="Palustrine Scrub/Shrub", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'PFW')
hist(ccap2010perPixelScalers$PFW, main="Palustrine Forested", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'OW')
hist(ccap2010perPixelScalers$OW, main="Open Water", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'UCS')
hist(ccap2010perPixelScalers$UCS, main="Unconsolidated Shore", xlim=x.range.hists, col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

ccapCol.sub <- subset(ccapCol, class == 'EAB')
hist(ccap2010perPixelScalers$EAB, main="Estuarine Aquatic Bed", xlim=range(x.range.hists, ccap2010perPixelScalers$EAB), col=rgb(ccapCol.sub$red[1], ccapCol.sub$green[1], ccapCol.sub$blue[1],  maxColorValue = 255), breaks = target.breaks)

hist(cncPerPixelScalers$No.Change, main="No Change", xlim=range(x.range.hists), col="black", breaks = target.breaks)
hist(cncPerPixelScalers$Change, main="Change", xlim=range(x.range.hists), col="red", breaks = target.breaks)
mtext("Estimated to Mapped Area Ratio", side=1, line=1.5, outer = T)
mtext("frequency", side=2, line=1.25, outer = T)
dev.off()

print(quantile(cncPerPixelScalers$Change, c(0.025,0.5,0.975)))

#
