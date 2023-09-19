library(stringr)
library(dplyr)
library(rvest)

setwd("D:/Data Science/Dr. jafari project - gmaps/divar DB/divar-regions/")

filenames <- list.files(pattern="*.txt", full.names=TRUE)

data <- data.frame()
DF <- lapply(filenames, FUN = readHTML.toDF)

data <- data %>% mutate(pricePerSqM = prices/sizes)
naCount <- function(x) sum(is.na(x))

y <- data %>% group_by(districts) %>%
  summarize(numberOfNAs = naCount(pricePerSqM), 
            meanOfPpM = mean(pricePerSqM, na.rm = T), meanPr = mean(prices, na.rm = T))

#!!!!!!!!!!!!!!!! it is working by both "districts" and "districst"
distC <- as.data.frame(table(data$districts))

colnames(distC) <- c("districts", "Freq")
dataSumm <- merge(y, distC, by = "districts")

districtsList <- dataSumm$districts
write.csv(districtsList, "./districtList.csv")