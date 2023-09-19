#* @get/ ll.toClass
ll.toClass <- function(x, y, r = .1) {
  
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
  
  df <- read.csv("./llClass.csv")
  radius <- r
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  df2 <- mutate(df, distance = sqrt((x - lat)^2 + (y - lng)^2))
  df2 <- filter(df2, distance < radius)
  df2 <- mutate(df2, temp = distance*class)
  class <- round(sum(df2$temp)/sum(df2$distance))
  return(class)

}