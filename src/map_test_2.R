library(leaflet)
library(dplyr)
library(shiny)

accidents_data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")
accidents_data <- accidents_data %>% filter(accident_severity == 3 | accident_severity == 2)
accidents_data <- accidents_data %>% filter(!is.na(latitude) & !is.na(longitude), accident_severity == 3 | accident_severity == 2)
accidents_data$longitude <- as.numeric(accidents_data$longitude)
accidents_data$latitude <- as.numeric(accidents_data$latitude)
accidents_data <- na.omit(accidents_data[c("longitude", "latitude")])

ui <- fluidPage(
  leafletOutput("map"),
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.2426, lat = 53.4808, zoom = 10)
  })

  observe( priority = 1,{
    req(input$map_zoom)
    print(input$map_zoom)
    if (input$map_zoom >= 15) {
      bounds <- input$map_bounds
      print(bounds)
      accidents_data_filtered <- accidents_data %>%
        filter(longitude >= bounds$west,
               longitude <= bounds$east,
               latitude >= bounds$south,
               latitude <= bounds$north)
      accidents_data_filtered <- accidents_data_filtered %>% slice(1:10) # Select the first 10 rows
      print(nrow(accidents_data_filtered))
      leafletProxy("map", session) %>%
        addMarkers(data = accidents_data_filtered,
                   lng = accidents_data_filtered$longitude,
                   lat = accidents_data_filtered$latitude,
                   popup = paste(
                     "Accident Severity: ", accidents_data_filtered$accident_severity,
                     "Accident Date: ", accidents_data_filtered$date,
                     "Accident Time: ", accidents_data_filtered$time,
                     "Number of Vehicles: ", accidents_data_filtered$number_of_vehicles,
                     sep = "<br/>"
                   ),
                   clusterOptions = markerClusterOptions(
                     maxClusterRadius = 5,
                     disableClusteringAtZoom = 80
                   )
        )
    } else {
      leafletProxy("map", session) %>% clearMarkers()
    }
  })

}

shinyApp(ui, server)
