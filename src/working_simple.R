library(leaflet)
library(dplyr)
accidents_data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")
accidents_data <- accidents_data %>% filter(!is.na(latitude) & !is.na(longitude), accident_severity == 3 | accident_severity == 2)
accidents_data$longitude <- as.numeric(accidents_data$longitude)
accidents_data$latitude <- as.numeric(accidents_data$latitude)
accidents_data <- na.omit(accidents_data[c("longitude", "latitude", "date", "accident_severity", "time","number_of_vehicles")])


min_zoom <- 10
map <- leaflet() %>% addTiles() %>% setView(lng = 2.2426, lat = 53.4808, zoom = 8)
map <- map %>%
  addMarkers(data = accidents_data,
             lng = accidents_data$longitude, lat = accidents_data$latitude,
             popup = paste(
               "Accident Severity: ", accidents_data$accident_severity,
               "Accident Date: ", accidents_data$date,
               "Accident Time: ", accidents_data$time,
               "Number of Vehicles: ", accidents_data$number_of_vehicles,
               sep = "<br/>"
             ),-
             clusterOptions = markerClusterOptions(
               maxClusterRadius = 10,
               disableClusteringAtZoom = 15
             )
  )

map
