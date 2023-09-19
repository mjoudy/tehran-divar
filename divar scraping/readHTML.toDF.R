# library(stringr)
# library(dplyr)
# library(rvest)


#Sys.setlocale(locale = "persian")

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
persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
english <- "01234567890123456789"

persian.tonumber <- function(x) as.numeric(chartr(persian,english,x))

priceTemp <- gsub(pattern = "[^??-??0-9\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]", replacement = "", x = priceRaw)
price <- persian.tonumber(priceTemp)

descriptionTemp <- gsub(pattern = "[^??-??0-9\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]", replacement = " ", x = descrption)
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








