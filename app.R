
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

#-------------------------------------------------

data <-read.csv("./points_data.csv")
point_list<- as.list(as.character(levels(data$name)))
names(point_list) <- as.character(levels(data$name))
data$name <- as.character(data$name)
data <- data %>% mutate(lat = lats, lng = lngs)
data <- data %>% mutate(-lats, -lngs)
col <- c("yellow", "blue", "red")
data$col <- col[data$time]


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tehran Points"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(
          "select",
          label = h3("Select a Point:"),
          choices = point_list
        ),
        
        checkboxInput("morning", "Morning:", value = T),
        checkboxInput("noon", "Noon:"), 
        checkboxInput("afternoon", "Afternoon:")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map"),
        plotOutput("speedDist")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     
     # if (input$morning == T){
     #   visData <- filter(data, name == input$select, time == "morning")
     #   tmap <- visData %>% leaflet() %>% addTiles() %>%
     #     addCircleMarkers(color = "blue")
     # }
     # 
     # if (input$noon == T){
     #   visData <- filter(data, name == input$select, time == "noon")
     #   tmap <- visData %>% leaflet() %>% addTiles() %>%
     #     addCircleMarkers(color = "yellow")
     # }
     # 
     # if (input$afternoon == T){
     #   visData <- filter(data, name == input$select, time == "afternoon")
     #   tmap <- visData %>% leaflet() %>% addTiles() %>%
     #     addCircleMarkers(color = "red")
     # }
     
     visData <- filter(data, name == input$select)
     tmap <- visData %>% leaflet() %>% addTiles() %>%
       addCircleMarkers(color = visData$col)
     
     tmap
   })
   
   output$speedDist <- renderPlot({
     visData <- filter(data, name == input$select)
     #qplot(speed, data = visData, fill = visData$time)
     ggplot(visData, aes(x = speed, color = time, fill = time)) + 
       geom_histogram(alpha = 0.5, position="identity") + 
       scale_color_manual(values=c("red", "blue", "yellow")) +
       scale_fill_manual(values=c("red", "blue", "yellow"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

