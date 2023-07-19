![Application Screenshot](https://github.com/Mythically/r-code/blob/master/img/app_screenshot.png?raw=true)
# 🚦 Public Road Accidents Map

This repository contains a Shiny application that displays a toggleable heatmap and markers overlay 📍 for public road accidents data from the UK government. The focus of the data is on pedestrian 🚶‍♂️ and cyclist 🚴‍♂️ casualties, with the option to include vehicle-only accidents alongside pedestrian-related ones.
### ✨ Features

 - Heatmap: Visualizes accident distribution using a heatmap layer 🌡️.
 - Markers: Displays individual markers for each accident, allowing users to view details 📍.
 - Filtering: Select a specific year and include/exclude non-pedestrian/bicycle casualties.
 - Accident Statistics 📊: Provides various accident-related statistics.
 - AutoML Feature Importance: Identifies important features using AutoML.
 - Interactive Map Controls 🌍: Toggle between map styles and overlay layers.

### 💻 Technologies Used

The application is built using the following libraries and technologies:

  * leaflet 🍃: A powerful JavaScript library for creating interactive maps.
  * dplyr 🔄: A data manipulation library in R, used for filtering and summarizing the accident data.
  * shiny 🌟: A web application framework for R, used to develop the interactive Shiny application.
  * leaflet.extras 🌟: An extension package for the leaflet library, providing additional functionality for markers and controls.
  * h2o 🌟: An open-source AutoML platform, used for feature importance analysis and model evaluation.

### 📋 Usage

To use the application, it is currently hosted free on shinyapps:

<https://mariyannakev.shinyapps.io/r_road_data/>

To run the application locally, follow these steps:

  Make sure you have R and the required packages (***leaflet***, ***dplyr***, ***shiny***, ***leaflet.extras***, ***h2o***) installed.
  
    packages <- c("leaflet", "leaflet.extras", "dplyr", "shiny", "h2o")
    install.packages(packages)
  No need to install h2o unless you want to use autoML.
  Clone this repository to your local machine.
  Set the working directory in your R environment to the cloned repository folder.
  Run the application script using an R development environment or the command shiny::runApp() in your R console or press the "play" ▶ button in RStudio or your favourite editor configured for R language.


### 📁 Data Source

The public road accidents data used in this application is sourced from the [UK government](https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data). The specific dataset used in the application is stored in the file merge_data_table_both_index_filtered_unique.rds. The data underwent a cleanup process before being used in the application. The cleanup script included the following steps:

The data cleanup script includes:

 * Reading and selecting relevant columns.
 * Converting data types and removing missing values.
 * Filtering severe accidents and casualties.
 * Merging datasets.
 * Cleaning categorical columns.
 * Saving the final merged dataset.

Feel free to explore and customize the application according to your needs and requirements!

### 🔒 License

This project uses road safety data obtained from the UK government website under the terms of the [Open Government License (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
This project is licensed under the MIT License.
