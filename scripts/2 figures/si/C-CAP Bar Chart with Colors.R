# creating a bar chart for ccap classes and change events

# needed libraries
{ 
  library(foreign)
  library(tidyverse)
  library(dplyr)
}

# load needed data files
{
  ccapCol <- read_csv("data/WetlandArea/CCAP/ccapCols.csv")
  ccap2010 <- read.dbf("data/WetlandArea/CCAP/AllStates2006to2010wGreatLakes_170720B.dbf")
  palustrineMappedPixels <- read.csv("data/outputTables/MonteCarloResults1/palustrineMappedPixels.savedIterations.csv")
  # input table from raster dataset of palustrine under NWI assumption
  palustrineNwi <- read.dbf("data/WetlandArea/Palustrine/nwi/CCAP2006to2010_wTab_PalMaskedByNwi.dbf")
  ccap2010perPixelScalers <- read.csv("data/outputTables/MonteCarloResults1/ccap2010perPixelScalers.savedIterations.csv")
  cncPerPixelScalers <- read.csv("data/outputTables/MonteCarloResults1/cncPerPixelScalers.savedIterations.csv")
}

# set up assumptions
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
accounting_strategy <- c()

i = 1
for (m in 1:length(classOrder)) {
  for (n in 1:length(classOrder)) {
    if ((classOrder[m] %in% c(palustrineWetlands, estuarineWetlands)) | (classOrder[n] %in% c(palustrineWetlands, estuarineWetlands))) {
      
      this.abbrev <- paste(abbrevs[m], "_", abbrevs[n], sep="")
      scaler.2010.iterations <- ccap2010perPixelScalers[abbrevs[n]]
      
      if (classOrder[m] == classOrder[n]) {
        scaler.cnc <- cncPerPixelScalers$No.Change
        cnc_col <- c(cnc_col, rep("no change", 2))
      } else {
        scaler.cnc <- cncPerPixelScalers$Change
      }
      
      # if it's estuarine pull from original CCAP table
      if ((classOrder[m] %in% c(estuarineWetlands)) | (classOrder[n] %in% c(estuarineWetlands))) {
        # scale by 2010 class
        mapped.pixel.count <- subset(ccap2010, (X2006_Clas == classOrder[m]) & (X2010_Clas == classOrder[n]))$Count[1] 
        mapped.pixel.count[is.na(mapped.pixel.count)] <- 0 # set to 0 if it's a no data value
        
        scaled.pixel.count <- mapped.pixel.count * scaler.2010.iterations * scaler.cnc
        
        scaled.pixel.count.quantiles <- as.data.frame(t(quantile(scaled.pixel.count[,1], c(0.025,0.5,0.975)))) # summarize quantiles
        accounting_strategy <- c(accounting_strategy, "Estuarine")
        abbrev.vect<-c(abbrev.vect, rep(this.abbrev, 1))
        class2006 <- c(class2006, rep(classOrder[m], 1))
        class2010 <- c(class2010, rep(classOrder[n], 1))
        
      } else {
        if (this.abbrev %in% names(palustrineMappedPixels)) {
          mapped.pixel.count.coastal.lands <- palustrineMappedPixels[this.abbrev] # if it's palustrine pull from probibalistic map
          mapped.pixel.count.coastal.lands[is.na(mapped.pixel.count.coastal.lands)] <- 0
          # scale by 2010 class and change or no change for coastal lands
          scaled.pixel.count.coastal.lands <- mapped.pixel.count.coastal.lands * scaler.2010.iterations * scaler.cnc
          scaled.pixel.count.coastal.lands.quantiles <- as.data.frame(t(quantile(scaled.pixel.count.coastal.lands[,1], c(0.025,0.5,0.975)))) # summarize quantiles
          
          mapped.pixel.count.NWI <- subset(palustrineNwi, (X2006_Clas == classOrder[m]) & (X2010_Clas == classOrder[n]))$Count[1] 
          mapped.pixel.count.NWI[is.na(mapped.pixel.count.NWI)] <- 0 # set to 0 if it's a no data value
          # scale by 2010 class and change or no change for NWI
          scaled.pixel.count.NWI <- mapped.pixel.count.NWI * scaler.2010.iterations * scaler.cnc
          scaled.pixel.count.NWI.quantiles <- as.data.frame(t(quantile(scaled.pixel.count.NWI[,1], c(0.025,0.5,0.975)))) # summarize quantiles
          
          scaled.pixel.count.quantiles <- rbind(scaled.pixel.count.coastal.lands.quantiles, scaled.pixel.count.NWI.quantiles)
          accounting_strategy <- c(accounting_strategy, c("Palustrine: Coastal Lands", "Palustrine: NWI"))
          abbrev.vect<-c(abbrev.vect, rep(this.abbrev, 2))
          class2006 <- c(class2006, rep(classOrder[m], 2))
          class2010 <- c(class2010, rep(classOrder[n], 2))
          
        } else {
          # do nothing
        }
      }

      if (i == 1) {
        q.df <- scaled.pixel.count.quantiles
      } else {
        q.df <- rbind(q.df, scaled.pixel.count.quantiles)
      }
      i = i + 1
    }
  }
}

#col.vect <- c()
#for (i in 1:nrow(rgb.df)) { col.vect <- c(col.vect, rgb(r=rgb.df$red[i], g=rgb.df$green[i], b=rgb.df$blue[i], maxColorValue = 255))}

bar.df <- as_tibble(as.data.frame(q.df) * 900 / 10000 / 1E6)
names(bar.df) <- c("lower.ci", "median.ci", "upper.ci")

bar.df["abbrevs"] <- gsub("_", " to ", abbrev.vect)
bar.df['analysis_type'] <- accounting_strategy

#bar.df['labs'] <- paste(bar.df$abbrevs, " (", round(bar.df$median * 900 / 10000 / 1E6, 2), ")", sep="")

sort.df <- bar.df[bar.df$analysis_type == "Palustrine: Coastal Lands" | bar.df$analysis_type == "Estuarine",]
sort.df <- sort.df[order(- sort.df['median.ci']), ]

categories_included_in_chart <- sort.df$abbrevs[1:10]
the_rest_categories <- sort.df$abbrevs[11:nrow(sort.df)]

excluded_from_chart_1 <- bar.df %>%
  filter(abbrevs %in% the_rest_categories,
         analysis_type != "Estuarine") %>%
  group_by(analysis_type) %>%
  summarise(lower.ci = sum(lower.ci),
            median.ci = sum(median.ci),
            upper.ci = sum(upper.ci),
            abbrevs = "other palustrine"
  )

excluded_from_chart_2 <- bar.df %>%
  filter(abbrevs %in% the_rest_categories,
         analysis_type == "Estuarine") %>%
  group_by(analysis_type) %>%
  summarise(lower.ci = sum(lower.ci),
            median.ci = sum(median.ci),
            upper.ci = sum(upper.ci),
            abbrevs = "other estuarine"
  )

included_in_chart <- bar.df %>%
  filter(abbrevs %in% categories_included_in_chart)

included_in_chart <- rbind(included_in_chart, excluded_from_chart_1, excluded_from_chart_2)

generate_decending_order <- included_in_chart %>%
  group_by(abbrevs) %>%
  summarise(max_median.ci = max(median.ci))

generate_decending_order <- generate_decending_order[order(-generate_decending_order$max_median.ci), ]
generate_decending_order['order'] <- 1:nrow(generate_decending_order)

included_in_chart <- included_in_chart %>%
  left_join(generate_decending_order)

included_in_chart <- included_in_chart[order(-included_in_chart$order),]

included_in_chart$abbrevs <- factor(included_in_chart$abbrevs , levels = generate_decending_order$abbrevs [order(generate_decending_order$order)])

ggplot(included_in_chart, aes(x=abbrevs, y = median.ci, fill = analysis_type)) +
  geom_bar(stat="identity", position = "dodge", color="black") +
  geom_linerange(aes(ymin=upper.ci, ymax=lower.ci), position = position_dodge(width = 0.9)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Coastal Wetland Area (Million Hectares)") +
  labs(title = "2006 to 2011") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title=element_blank()) +
  scale_fill_manual(values = c("#999999", "#F0E442", "#CC79A7")) +
  theme(legend.position = c(0.6, 0.6))
  
# How much NWI vs Coastal Lands
included_in_chart_coastal_lands <- subset(included_in_chart, analysis_type == "Estuarine" | analysis_type == "Palustrine: Coastal Lands")
(sum(included_in_chart_coastal_lands$median.ci))

included_in_chart_nwi <- subset(included_in_chart, analysis_type == "Estuarine" | analysis_type == "Palustrine: NWI")
(sum(included_in_chart_nwi$median.ci))

# how much OW to EM compared to EM to OW?
((as.numeric(bar.sub.df.1b[9,]$median[1]) + as.numeric(bar.sub.df.1b[10,]$median[1])) / 
    (as.numeric(bar.sub.df.1b[7,]$median[1]) + as.numeric(bar.sub.df.1b[8,]$median[1]))) * 100


# Histograms for key wetland classes estimated:mapped ratio
# EEM ESS EFO PEM 
# PFO PSS OW UCS
# EAB EAB # Change # No.Change 

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

print(quantile(cncPerPixelScalers$Change, c(0.025,0.5,0.975)))
