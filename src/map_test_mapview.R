library(shiny)
library(dplyr)
library(sp)
library(mapview)

# load data
accidents_data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")

# create spatial object
accidents_data$longitude <- as.numeric(accidents_data$longitude)
accidents_data$latitude <- as.numeric(accidents_data$latitude)
accidents_data <- na.omit(accidents_data[c("longitude", "latitude")])
coordinates(accidents_data) <- cbind(accidents_data$longitude, accidents_data$latitude)
proj4string(accidents_data) <- CRS("+proj=longlat +datum=WGS84")

# filter data to first 10 rows
accidents_data_filtered <- accidents_data[1:10, ]

# define ui
ui <- fluidPage(
  titlePanel("Accidents Map"),
  sidebarLayout(
    sidebarPanel(
      helpText("Display accidents map"),
      selectInput(
        "marker",
        "Display Markers:",
        choices = c("None", "All", "Fatalities", "Severe injuries", "Light injuries"),
        selected = "All"
      )
    ),
    mainPanel(
      mapviewOutput("map")
    )
  )
)

# define server
server <- function(input, output) {

  # create a reactive object for filtered accidents data
  accidents_data_filtered_markers <- reactive({
    filtered_data <- accidents_data_filtered

    # filter data based on input
    if (input$marker == "None") {
      filtered_data <- filtered_data[0,]
    } else if (input$marker != "All") {
      filtered_data <- filtered_data %>%
        filter(Severity == input$marker)
    }

    # create spatial object
    coordinates(filtered_data) <- cbind(filtered_data$longitude, filtered_data$latitude)
    proj4string(filtered_data) <- CRS("+proj=longlat +datum=WGS84")

    return(filtered_data)
  })

  # display map
  output$map <- renderMapview({
    mapview(accidents_data_filtered_markers(), map.types = "OpenStreetMap.Mapnik")
  })
}


# run app
shinyApp(ui = ui, server = server)
