#this is a temporary main script which will turn into a functioned program

library(stringr)
library(dplyr)
library(rvest)
library(ggmap)
library(googleway)

#setwd("D:/Data Science/Dr. jafari project - gmaps/divar DB/divar-regions/")

setwd("/media/m-joudy/01D29D6C66F47010/Data Science/Dr. jafari project - gmaps/divar DB/divar-regions")

filenames <- list.files(pattern="*.txt", full.names=TRUE)

#____________________________________________________________________-

readHTML.toDF <- function(x) {
  
  #setwd("D:/Data Science/Dr. jafari project - gmaps/divar DB/divar-regions/")
  
  #divarHtml <- read_html("./test2.txt")
  divarHtml <- read_html(x)
  
  districtNode <- html_nodes(divarHtml, '.description label')
  district <- html_text(districtNode)
  
  priceNode <- html_nodes(divarHtml, '.price')
  priceRaw <- html_text(priceNode)
  
  descriptionNode <- html_nodes(divarHtml, 'h2')
  descrption <- html_text(descriptionNode)
  
  #this line should be revisited!!!! 
  #length of description = 841, two others = 840 !
  descrption <- descrption[-1]
  
  #try to call this function from another file.
  
  #there 2 types of unicodes for persian or arabic digits!
  #persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
  persian <- "۰۱۲۳۴۵۶۷۸۹\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669"
  english <- "01234567890123456789"
  
  persian.tonumber <- function(x) as.numeric(chartr(persian,english,x))
  
  #priceTemp <- gsub(pattern = "[^??-??0-9\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]", replacement = "", x = priceRaw)
  priceTemp <- gsub(pattern = "قیمت کل:|٫|تومان", replacement = "", x = priceRaw)
  price <- persian.tonumber(priceTemp)
  
  descriptionTemp <- gsub(pattern = "[^??-??0-9۰-۹\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]", replacement = " ", x = descrption)
  #descriptionTemp <- gsub(pattern = "[^??-??0-9۰-۹]", replacement = " ", x = descrption)
  temp <- strsplit(descriptionTemp, split = "\\s+")
  ncol <- max(sapply(temp,length))
  tempDF <- as.data.frame(lapply(1:ncol,function(i)sapply(temp,"[",i)))
  
  # library(splitstackshape)
  # concat.split(dat, "x", " ")
  # https://stackoverflow.com/questions/12946883/strsplit-by-row-and-distribute-results-by-column-in-data-frame?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  
  tempDfNum <- apply(tempDF, MARGIN = 2, persian.tonumber)
  tempDfNum[is.na(tempDfNum)] <- 0
  size <- apply(tempDfNum, 1, max)
  #size <- pmax(tempDfNum[,1], tempDfNum[,2], tempDfNum[,3], tempDfNum[,4], tempDfNum[,5], na.rm = T)
  size[size < 20] <- NA
  #size <- do.call(pmax, tempDfNum)
  
  dataTemp <- data.frame(prices = price, sizes = size, districts = district, rawPrice = priceRaw, descrption = descrption)
  dataTemp$fileName <- x
  print(district)
  print(x)
  data <<- rbind(data, dataTemp)
  
  return(data)
  
  #supInfoTemp <- data.frame(rawPrice = priceRaw, descrption = descrption)
  #supInfo <- rbind(supInfoTemp, supInfo)
  
}

#______________________________________________________________________

data <- data.frame()
DF <- lapply(filenames, FUN = readHTML.toDF)

#omiting wrong prices, later should have better solution
data_e7 <- filter(data, is.na(data$prices) | prices > 1e7)

data <- data_e7 %>% mutate(pricePerSqM = prices/sizes)
naCount <- function(x) sum(is.na(x))

y <- data %>% group_by(districts) %>%
  summarize(numberOfNAs = naCount(pricePerSqM), 
            meanOfPpM = mean(pricePerSqM, na.rm = T), meanPr = mean(prices, na.rm = T))

#!!!!!!!!!!!!!!!! it is working by both "districts" and "districst"
distC <- as.data.frame(table(data$districts))

colnames(distC) <- c("districts", "Freq")
dataSumm <- merge(y, distC, by = "districts")
dataSumm <- mutate(dataSumm, nItems = Freq - numberOfNAs)

districtsList <- dataSumm$districts
write.csv(districtsList, "./districtList.csv")

#______________________________________________________________________

districts <- as.character(districtsList)

tehDistricts <- paste0("تهران ",districts)

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
xx <- paste0("تهران ", x)
dataSumm$districts <- xx

dataSummLL <- merge(dataSumm, latlon, by = "districts")

#__________________________________________________________

b <- dataSummLL
# I changed name of decile column to class
b <- mutate(b, class = ntile(b$meanOfPpM, 10))
b$districts <- as.character(b$districts)
b <- mutate(b, nItems = Freq - numberOfNAs)
# q <- "tehran, "
# test <- paste0(q, a$district)
# b <- geocode(test)
# 
# a <- mutate(a, lon = b$lon, lat = b$lat)
# x <- a[!is.na(a$lon), ]

b$lat[59] <- 35.747187
b$lon[59] <- 51.302465

#because of omtting some outliers in order to have better view. after adding 
#zoom feature I will remove it.
c <- filter(b, lat > 35.45 & lat < 35.85 & lon > 51 & lon < 51.8)

teh_center <- as.numeric(geocode("tehran"))

#map = ggmap(get_googlemap(center=teh_center, zoom=12), extent="normal")

map <- get_map(location = c(lon = 51.38897, lat = 35.6892), zoom = 10,
               source = "google")

foo <-ggmap(map) +
  scale_x_discrete(limits = c(51, 51.8), expand = c(0, 0)) +
  scale_y_discrete(limits = c(35.45, 35.85), expand = c(0, 0))

#in order to have discrete legend
c$decile <- factor(c$decile)

fmap <- foo + 
geom_point(data=c, aes(x=lon, y=lat, col=decile), alpha=.8, size = 4)

# #######
# c$meanOfPpM <- (c$meanOfPpM)/1000000
# d <- c %>% group_by(decile)%>% summarize(groupMean = mean(meanOfPpM, na.rm = T))
# c$groupMean[c$decile[i] == d$decile[i]] <- d$decile[i]  
# #^^^^^^^^^^^^^^ Error in NextMethod("[") : object 'i' not found

  
  
