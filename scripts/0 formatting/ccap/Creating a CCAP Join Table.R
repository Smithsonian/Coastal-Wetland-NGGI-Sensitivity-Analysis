# wrote a code to generate a new attribute join table for CCAP
classOrder <-c('High Intensity Developed', 'Medium Intensity Developed', 'Low Intensity Developed', 'Developed Open Space', 
               'Cultivated', 'Pasture/Hay', 
               'Grassland', 'Deciduous Forest', 'Evergreen Forest', 'Mixed Forest', 'Scrub/Shrub', 
               'Palustrine Forested Wetland', 'Palustrine Scrub/Shrub Wetland', 'Palustrine Emergent Wetland', 
               'Estuarine Forested Wetland', 'Estuarine Scrub/Shrub Wetland', 'Estuarine Emergent Wetland', 
               'Unconsolidated Shore', 'Bare Land', 'Water', 
               'Palustrine Aquatic Bed', 'Estuarine Aquatic Bed',
               'Tundra',
               'Snow/Ice')

classOrder <- c('Unclassified', classOrder)

value_number <- c()
X2006_Clas <- c()
X2010_Clas <- c()
Class_Name <- c()

value_n = 0
for (i in 1:length(classOrder)) {
  for (j in 1:length(classOrder)) {
    X2006_Clas <- c(X2006_Clas, classOrder[i])
    X2010_Clas <- c(X2010_Clas, classOrder[j])
    
    Class_Name <- c(Class_Name, paste(classOrder[i], " to ", classOrder[j], sep=""))
   
    value_n = value_n + 1
    value_number <- c(value_number, value_n)
  }
}

outTable <- data.frame(Value = value_number, X2006_Clas = X2006_Clas, X2010_Clas = X2010_Clas, Class_Name = Class_Name)

write.table(outTable, paste(getwd(), "/data/ccapJoinTable.csv", sep=""), sep=",", row.names=F)