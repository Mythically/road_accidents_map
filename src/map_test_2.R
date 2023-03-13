library(leaflet)
library(dplyr)
library(shiny)
library(leaflet.extras)
accidents_data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")
accidents_data <- accidents_data %>% filter(accident_severity == 1 | accident_severity == 2)
accidents_data <- accidents_data %>% filter(!is.na(latitude) &
                                              !is.na(longitude) &
                                              (accident_severity == 1 | accident_severity == 2))
accidents_data <- na.omit(accidents_data[c("longitude", "latitude", "date", "accident_severity", "time", "number_of_vehicles")])
accidents_data$longitude <- as.numeric(accidents_data$longitude)
accidents_data$latitude <- as.numeric(accidents_data$latitude)
accidents_data <- na.omit(accidents_data)

ui <- fluidPage(
  leafletOutput("map", height = "100vh", width = "100vw")
)

server <- function(input, output, session) {
  data <- reactive({
    x <- accidents_data
  })

  output$map <- renderLeaflet({
    a_d <- data()

    # http://rstudio.github.io/leaflet/markers.html
    getColor <- function(accident) {
      sapply(accident$accident_severity, function(severity) {
        if (severity == 1) {
          return("red")
        } else if (severity == 2) {
          return("orange")
        }
      })
    }

    icons <- awesomeIcons(
      icon = "ios-close",
      iconColor = "black",
      library = "ion",
      markerColor = getColor(a_d)
    )
    m <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(data = a_d,
                 lng = a_d$longitude, lat = a_d$latitude,
                 icon = icons(),
                 popup = paste(
                   "Accident Severity: ", a_d$accident_severity,
                   "Accident Date: ", a_d$date,
                   "Accident Time: ", a_d$time,
                   "Number of Vehicles: ", a_d$number_of_vehicles,
                   sep = "<br/>"
                 ),
                 clusterOptions = markerClusterOptions(
                   maxClusterRadius = 80,
                   disableClusteringAtZoom = 15),

      ) %>%
      addSearchOSM(
        options = searchOptions(
          url = "https://nominatim.openstreetmap.org/search",
          zoom = 16,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      )
  })
}

shinyApp(ui, server)
