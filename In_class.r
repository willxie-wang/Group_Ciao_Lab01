ameslist <- read.table("https://msudataanalytics.github.io/SSC442/assets/ames.csv",
                       header = TRUE,
                       sep = "," ) 
ameslist
unique(ameslist$GarageType)
GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType )
ameslist <- cbind(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

