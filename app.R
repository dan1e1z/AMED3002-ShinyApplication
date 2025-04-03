# Kidney Disease Analysis Shiny App
# Main application file that loads libraries and launches the app

# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(plotly)
library(rsconnect)

source("ui.R")
source("server.R")
  
# Run the application
shinyApp(ui = ui, server = server)