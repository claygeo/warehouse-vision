# Main script content
cat('
# WarehouseVision: Main Script

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# Source all modules
source("R/data_generation.R")
source("R/visualization_module.R")
source("R/analytics_module.R")
source("R/app.R")

# Load or generate data
warehouse_data <- load_warehouse_data()

# Run the Shiny application
shinyApp(ui, server)
')

# Create a simple version for importing into RStudio 
writeLines(
  '# WarehouseVision: Digital Twin Management System
# Quick Import File for RStudio

# This file can be sourced in RStudio to quickly set up the project environment
# and load all modules from their expected locations.

# Install required packages if not already installed
required_packages <- c(
  "shiny", "shinydashboard", "tidyverse", "plotly", "DT", 
  "lubridate", "viridis", "viridisLite"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load libraries
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}

# Source all modules
source("R/data_generation.R")
source("R/visualization_module.R")
source("R/analytics_module.R")

# Source the app.R file
source("R/app.R")

# Function to run the application
run_warehouse_vision <- function() {
  # Load or generate data
  warehouse_data <- load_warehouse_data()
  
  # Run the Shiny application
  shinyApp(ui, server)
}

# Print a helpful message
cat("\nWarehouseVision environment loaded successfully!\n")
cat("To run the application, execute: run_warehouse_vision()\n")
',
  "run_warehouse_vision.R"
)