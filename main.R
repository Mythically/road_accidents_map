library(shiny)
library(ggplot2)
library(dplyr)
 colnames(read.csv("resources/dft-road-casualty-statistics-accident-2021.csv"))


data <- read.csv("resources/dft-road-casualty-statistics-accident-2021.csv")

ui <- fluidPage(
  titlePanel("UK Road Accidents Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("attribute", "Select an attribute to display:",
                  choices = c("Severity", "Location", "Time and date",
                              "Type of Accident", "Contributory factors", "Age and gender",
                              "Vehicle type"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  data_severe <- data %>% filter(accident_severity == 3 | accident_severity == 2)

  selected_data <- reactive({
    switch(input$attribute,
           "Severity" = data_severe %>% group_by(accident_severity) %>% summarize(count = n())
    )
  })

  output$plot <- renderPlot({
    ggplot(data = selected_data(), aes(x = reorder(!!sym(names(selected_data())[1]), count), y = count)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      xlab(input$attribute) +
      ylab("Count") +
      ggtitle(paste("Number of severe UK road accidents by", input$attribute))
  })

}

shinyApp(ui = ui, server = server)
