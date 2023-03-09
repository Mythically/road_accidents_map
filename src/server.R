library(shiny)
library(leaflet)

shinyServer(function(input, output) {

  # load the data
  accidents <- read.csv("path/to/accidents.csv")

  # create the map
  output$map <- renderLeaflet({
    leaflet(accidents) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude)
  })

})