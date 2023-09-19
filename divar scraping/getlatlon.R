# this is a script for getting lat-long of a list of places. because the geocode()
#function sometimes gets NAs, I intended to write this script.
library(ggmap)
library(dplyr)


#districts <- read.csv("./districtList.csv")
districts <- as.character(districtsList)

tehDistricts <- paste0("?????????? ",districts)

a <- geocode(tehDistricts)
latlon <- data.frame(districts = tehDistricts, lat = a$lat, lon = a$lon)
latlon$districts <- as.character(latlon$districts)

naCounter <- sum(is.na(latlon$lat))
loopCounter <- 0

while(naCounter > 0){
  
  data1 <- latlon[complete.cases(latlon), ]
  data2 <- latlon[!complete.cases(latlon), ]
  
  a <- geocode(data2$districts)
  data2 <- mutate(data2, lat = a$lat, lon = a$lon)
  
  latlon <- rbind(data1, data2)
  
  naCounter <- sum(is.na(latlon$lat))
  loopCounter <- loopCounter + 1
  print(naCounter)
  print(loopCounter)
  
}


dataSumm$districts <- as.character(dataSumm$districts)
x <- dataSumm$districts
xx <- paste0("?????????? ", x)
dataSumm$districts <- xx
dataSummLL <- merge(dataSumm, latlon, by = "districts")

write.csv(dataSummLL, "./dataSummLL282.csv")

