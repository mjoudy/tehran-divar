library(stringr)
library(ggmap)
library(dplyr)

#Sys.setlocale(locale = "persian")

setwd("D:/Data Science/Dr. jafari project - gmaps/divar DB/")
data <- read.csv("./divar6500.csv", encoding = "UTF-8")

data$X.U.FEFF.Price <- as.character(data$X.U.FEFF.Price)
prices <- gsub(pattern = "[^۰-۹]",replacement = "",x = data$X.U.FEFF.Price)

persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
english <- "01234567890123456789"

persian.tonumber <- function(x) as.numeric(chartr(persian,english,x))

prices <- persian.tonumber(prices)

# 
# regexForSize <- "\\bمتر\\s*(\\d+)"
# 
# str_match(data$size, regexForSize)
data$district <- as.character(data$district)
data$size <- as.character(data$size)

temp <- strsplit(data$size, split = "متر")
df <- data.frame(matrix(unlist(temp), nrow=dim(data)[1], byrow=T),stringsAsFactors=FALSE)
X1 <- gsub(pattern = "[^۰-۹0-9\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]",replacement = "",x = df$X1)
X2 <- gsub(pattern = "[^۰-۹0-9\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669]",replacement = "",x = df$X2)
X1 <- persian.tonumber(X1)
X2 <- persian.tonumber(X2)

data.2 <- data.frame(price = prices, size = pmax(X1, X2, na.rm = T),
                     district = data$district, info.1 = df$X1, info.2 = df$X2)


# these codes are to reduce regex programming for extracting price 
# which were complicated through some other numbers. logically, price number 
# would be greater than others like size, floor, rooms, age street and ...
#docker run -p 5023:5023 -p 8050:8050 -p 8051:8051 scrapinghub/splash

data.2$size[data.2$size < 20] <- NA

data.2 <- data.2 %>% mutate(pricePerSqM = price/size)

naCount <- function(x) sum(is.na(x))
y <- data.2 %>% group_by(district) %>%
  summarize(numberOfNAs = naCount(pricePerSqM), 
            meanOfPpM = mean(pricePerSqM, na.rm = T), meanPr = mean(price, na.rm = T))

distC <- as.data.frame(table(data.2$district))
colnames(distC) <- c("district", "Freq")
dataSumm <- merge(y, distC, by = "district")

#######mapping
b <- dataSummLL
b <- mutate(b, decile = ntile(b$meanOfPpM, 10))
b$districts <- as.character(b$districts)
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
  scale_x_continuous(limits = c(51, 51.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(35.45, 35.85), expand = c(0, 0))

geom_point(aes(x=lon, y=lat), data=c, col=c$decile, alpha=0.4, size = 3) +

