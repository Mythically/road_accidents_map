library(leaflet)
library(dplyr)
accidents_data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")
accidents_data <- accidents_data %>% filter(accident_severity == 3 | accident_severity == 2)
accidents_data <- accidents_data %>% filter(!is.na(latitude) & !is.na(longitude), accident_severity == 3 | accident_severity == 2)

min_zoom <- 10
map <- leaflet() %>% addTiles() %>% setView(lng = 53.4808, lat = 2.2426, zoom = 5)
map <- leaflet() %>% addTiles() %>% addMarkers(lng = ~longitude, lat = ~latitude, popup = paste(
    "Accident Severity: ", accidents_data$accident_severity,
    "Accident Date: ", accidents_data$date,
    "Accident Time: ", accidents_data$time,
    "Number of Vehicles: ", accidents_data$number_of_vehicles,
), clusterOptions = markerClusterOptions(maxClusterRadius = 20, disableClusteringAtZoom = min_zoom))
map