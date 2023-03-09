library(shiny)
library(leaflet)
library(dplyr)

accidents <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")
accidents <- accidents %>%
  na.omit() # remove rows with NA values

ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {

  filtered_accidents <- reactive({
    # get the bounds of the currently visible portion of the map
    bounds <- input$map_bounds
    # filter the accidents data to only include rows within the bounds
    accidents %>%
      filter(
        between(latitude, bounds$south, bounds$north),
        between(longitude, bounds$west, bounds$east)
      )
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = -1.4, lat = 52.9, zoom = 10) %>%
      addMarkers(
        data = filtered_accidents(),
        lat = ~latitude,
        lng = ~longitude,
        popup = ~as.character(Date),
        # set conditions for displaying markers
        options = markerOptions(
          clickable = TRUE,
          opacity = 1,
          fillOpacity = 1
        )
      )
  })

  observeEvent(input$map_zoom, {
    zoom <- input$map_zoom
    if (zoom < 15) {
      leafletProxy("map") %>%
        clearMarkers()
    } else {
      leafletProxy("map") %>%
        addMarkers(
          data = filtered_accidents(),
          lat = ~latitude,
          lng = ~longitude,
          popup = ~as.character(Date),
          options = markerOptions(
            clickable = TRUE,
            opacity = 1,
            fillOpacity = 1
          )
        )
    }
  })
}

shinyApp(ui, server)
