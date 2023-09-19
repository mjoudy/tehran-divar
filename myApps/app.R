#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)
library(rvest)
library(ggmap)
library(leaflet)

#___________________________________________________________________

b <- read.csv("./dataSummLL282.csv")
b <- mutate(b, decile = ntile(b$meanOfPpM, 10))
b$districts <- as.character(b$districts)
b <- mutate(b, nItems = Freq - numberOfNAs)
b$lat[59] <- 35.747187
b$lon[59] <- 51.302465

c <- b
#because of leaflet
colnames(c)[which(names(c) == "lon")] <- "lng"

pal <- colorNumeric(
  palette = colorRampPalette(rainbow(10))(length(c$decile)), 
  domain = c$decile)

#in order to have numerics for filtering decile in ploting process
c$deciles <- as.numeric(c$decile)
#in order to have discrete legend
c$decile <- factor(c$decile)
c$FmeanOfPpM <- round((c$meanOfPpM)/1e+06, digits = 2)

mpMax <- max(c$FmeanOfPpM, na.rm = T)
mpMin <- min(c$FmeanOfPpM, na.rm = T)

c <- mutate(c, poplable = paste(c$FmeanOfPpM,"-", c$decile))
#c$poplable <- as.factor(c$poplable)


#____________________________________________________________________________

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tehran House Prices"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("nItems",
                     "show districts which have items more than:",
                     min = 1,
                     max = 500,
                     value = 1), 
         
         sliderInput("class", "Show classes between:", 1, 10, value = c(1,10)),
         
         sliderInput("meanPrice", "show districts with average prices between(million Tomans):",
                     mpMin, mpMax, value = c(mpMin, mpMax))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderLeaflet({
      # generate bins based on input$bins from ui.R
      x1 <- input$class[1]
      x2 <- input$class[2]
      c <- filter(c, deciles >= x1 & deciles <= x2)
      
      y <- input$nItems
      c <- filter(c, nItems >= y)
      
      z1 <- input$meanPrice[1]
      z2 <- input$meanPrice[2]
      c <- filter(c, FmeanOfPpM >= z1 & FmeanOfPpM <= z2)
      
      tMap <- c %>% leaflet() %>% addTiles() %>% 
        addCircleMarkers(color = ~ pal(deciles), label = ~poplable) %>% 
        addLegend("bottomright", pal = pal, title = "Classes:",
                  values = ~deciles, opacity = 1)
      
      tMap
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

