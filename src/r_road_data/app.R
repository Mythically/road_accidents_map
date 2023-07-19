library(leaflet)
library(dplyr)
library(shiny)
library(leaflet.extras)

ui <- fluidPage(
  includeCSS("./www/stylesheet.css"),
  splitLayout(
    leafletOutput("map", height = "100vh"),
    div(
      id = "right-panel",
      style = "width: 48vw;",
      selectInput("year", "Select a year:", choices = 2021:2019, selected = 2021),
      checkboxInput("filter_dataset", "Include non-pedestiran/bicycle casualties", value = FALSE),
      br(),
      h4("Accident Statistics"),
      div(
        id = "top_locations",
        p("Top 3 most common accident locations:"),
        actionLink("location1", "Loading..."),
        br(),
        actionLink("location2", "Loading..."),
        br(),
        actionLink("location3", "Loading...")
      ),
      br(),
      p("Total number of accidents:"),
      verbatimTextOutput("total_accidents"),
      p("Average number of casualties per accident:"),
      verbatimTextOutput("avg_casualties"),
      p("Average age of drivers involved in accidents:"),
      verbatimTextOutput("avg_driver_age"),
      p("Proportion of accidents that involve a male driver:"),
      verbatimTextOutput("prop_male_driver"),
      p("Proportion of accidents based on light conditions"),
      uiOutput("accidents_by_light_conditions"),
      p("Proportion of accidents based on weather conditions"),
      uiOutput("accidents_by_weather"),
      p("Proportion of accidents based on day of the week"),
      uiOutput("accidents_by_day"),
      p("Proportion of accidents based on road type"),
      uiOutput("accidents_by_road_surface"),
      p("Proportion of accidents based on speed limit"),
      uiOutput("accidents_by_speed_limit"),
      p("Proportion of accidents based on junction details"),
      uiOutput("accidents_by_junction_details"),
      p("Proportion of accidents based on pedestrian location"),
      uiOutput("accidents_by_ped_crossing"),
    )
  )
)




server <- function(input, output, session) {

  data <- reactive({
    x <- readRDS("./resources/merge_data_table_both_index_filtered_unique.rds")
    x <- x %>% filter(x$accident_year.x == input$year)
    if (!input$filter_dataset) {
      x <- x %>% filter(x$casualty_class == 3 | x$vehicle_type == 1)
    }
    x
  })


  visible_markers <- reactive({
    input$map_marker_click
    eventReactive(input$map_bounds, {
      bounds <- input$map_bounds
      if (!is.null(bounds)) {
        x <- data() %>% filter(longitude >= bounds$west &
                                         longitude <= bounds$east &
                                         latitude >= bounds$south &
                                         latitude <= bounds$north)
      } else {
        x <- data()
      }
      x
    })
  })

  reactive_top_locations <- function(a_d) {
    a_d <- visible_markers()
    top_locations <- a_d() %>%
      group_by(latitude, longitude, .groups = "drop_last") %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(3)
  }

  observe({
    top_locations <- reactive_top_locations()

    updateActionButton(session, "location1",
                       label = paste0("1. Lat: ", top_locations$latitude[1],
                                      ", Long: ", top_locations$longitude[1],
                                      "(", top_locations$count[1], " accidents)"))
    updateActionButton(session, "location2",
                       label = paste0("2. Lat: ", top_locations$latitude[2],
                                      ", Long: ", top_locations$longitude[2],
                                      " (", top_locations$count[2], " accidents)"))
    updateActionButton(session, "location3",
                       label = paste0("3. Lat: ", top_locations$latitude[3],
                                      ", Long: ", top_locations$longitude[3],
                                      " (", top_locations$count[3], " accidents)"))
  })



  observeEvent(input$location1, autoDestroy = TRUE, {
    top_locations <- reactive_top_locations()
    leafletProxy("map") %>%
      setView(lng = top_locations$longitude[1],
              lat = top_locations$latitude[1], zoom = 16)
  })

  observeEvent(input$location2, autoDestroy = TRUE, {
    top_locations <- reactive_top_locations()
    leafletProxy("map") %>%
      setView(lng = top_locations$longitude[2],
              lat = top_locations$latitude[2], zoom = 16)
  })

  observeEvent(input$location3, autoDestroy = TRUE, {
    top_locations <- reactive_top_locations()
    leafletProxy("map") %>%
      setView(lng = top_locations$longitude[3],
              lat = top_locations$latitude[3], zoom = 16)
  })

  output$total_accidents <- renderPrint({
    a_d <- visible_markers()
    nrow(a_d())
  })

  output$avg_casualties <- renderPrint({
    a_d <- visible_markers()
    round(mean(a_d()$number_of_casualties, na.rm = TRUE), 2)
  })

  output$avg_driver_age <- renderPrint({
    a_d <- visible_markers()
    round(mean(a_d()$age_of_driver, na.rm = TRUE), 2)
  })

  output$prop_male_driver <- renderPrint({
    a_d <- visible_markers()
    prop_male <- round(sum(a_d()$sex_of_driver == "1", na.rm = TRUE) / nrow(a_d()) * 100, 2)
    paste(prop_male, "%")
  })

  output$accidents_by_day <- renderPrint({
    a_d <- visible_markers()
    results <- a_d() %>%
      group_by(day_of_week) %>%
      summarise(count = n()) %>%
      arrange(day_of_week)

    HTML(paste0(results$day_of_week, ": ", results$count, collapse = "<br>"))
  })

  output$accidents_by_weather <- renderPrint({
    a_d <- visible_markers()
    results <- a_d() %>%
    group_by(weather_conditions) %>%
    summarise(count = n()) %>%
    arrange(weather_conditions)

    HTML(paste0(results$weather_conditions, ": ", results$count, collapse = "<br>"))
    })

  output$accidents_by_light_conditions <- renderPrint({
    a_d <- visible_markers()
    results <- group_by(a_d(), light_conditions) %>%
      summarise(count = n()) %>%
      arrange(light_conditions)

    HTML(paste0(results$light_conditions, ": ", results$count, collapse = "<br>"))
  })


  output$accidents_by_road_surface <- renderPrint({
    a_d <- visible_markers()
    results <- group_by(a_d(), road_surface_conditions) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

    HTML(paste0(results$road_surface_conditions, ": ", results$count, collapse = "<br>"))
    })

  output$accidents_by_speed_limit <- renderPrint({
    a_d <- visible_markers()
    results <- group_by(a_d(), speed_limit) %>%
      summarise(count = n()) %>%
      mutate(proportion = count / sum(count) * 100)

    HTML(paste0(results$speed_limit, ": ", round(results$proportion, 2), "%", collapse = "<br>"))
  })

  output$accidents_by_junction_details <- renderPrint({
    a_d <- visible_markers()
    results <- group_by(a_d(), junction_detail) %>%
      summarise(count = n()) %>%
      mutate(proportion = count / sum(count) * 100)

    HTML(paste0(results$junction_detail, ": ", round(results$proportion, 2), "%", collapse = "<br>"))
  })

  output$accidents_by_ped_crossing <- renderPrint({
    a_d <- visible_markers()
    results <- group_by(a_d(), pedestrian_crossing_physical_facilities) %>%
      summarise(count = n()) %>%
      mutate(proportion = count / sum(count) * 100)

    HTML(paste0(results$pedestrian_crossing_physical_facilities, ": ", round(results$proportion, 2), "%", collapse = "<br>"))
  })



  output$map <- renderLeaflet({
    a_d <- data()
    m <- leaflet() %>%
      addTiles() %>%
      addFullscreenControl(position = "topleft") %>%
      addSearchOSM(
        options = searchOptions(
          url = "https://nominatim.openstreetmap.org/search",
          zoom = 16,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE,

        )
      ) %>%
      addHeatmap(
        group = "Heatmap",
        data = a_d[, c("longitude", "latitude")],
        radius = 17,
        blur = 25,
        cellSize = 25,
      ) %>%
      addAwesomeMarkers(
        group = "Markers",
        data = a_d,
        lng = ~longitude, lat = ~latitude,
        icon = icons(),
        popup = paste(
          "Accident Severity: ", a_d$accident_severity,
          "Accident Date: ", a_d$date,
          "Accident Time: ", a_d$time,
          sep = "<br/>"
        ),
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 80,
          disableClusteringAtZoom = 17
        )
      ) %>%
      addProviderTiles(providers$Stamen.Toner,
                       group = "Black and white") %>%
      addLayersControl(
        baseGroups = c("Black and white", "Colour"),
        overlayGroups = c("Heatmap", "Markers"),
        options = layersControlOptions(collapsed = FALSE)
      )

  })
}

shinyApp(ui, server)