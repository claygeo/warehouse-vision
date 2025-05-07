# Warehouse Vision
A Shiny R web application for managing a U-shaped warehouse with multi-component shelves. It provides interactive visualizations of warehouse layouts, shelf details, and component inventories, along with analytics for movement patterns and storage optimization. Built as a standalone demo with generated sample data, it supports warehouse operations analysis and component search.

## Features

- Interactive dashboard with metrics for component counts, inventory levels, and high-velocity items
- U-shaped warehouse layout visualization with filters for zone, category, velocity, utilization, and tier
- Detailed shelf unit views showing multi-tier layouts and component distributions
- Advanced component search by ID, type, or category with location highlighting
- Analytics for inventory by type, movement patterns, and multi-component storage
- Sample data generation for a 120x80 ft warehouse with wall and island shelves

## Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended for running Shiny apps)
- R packages: shiny, shinydashboard, tidyverse, plotly, DT, lubridate, viridis, viridisLite

## Setup

1. Clone the repository: git clone https://github.com/your-username/warehouse-vision.git
2. Navigate to the project directory: cd warehouse-vision
3. Install R packages: install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "lubridate", "viridis", "viridisLite"))
4. Run the app in RStudio (open standalone_warehouse_app.R and click "Run App") or in R:library(shiny) runApp("standalone_warehouse_app.R")

## Notes

- The app generates sample data internally; no external CSV files are required.
- Designed as a standalone demo.
- For the modular version, use main.R or run_warehouse_vision.R with the R/ folder modules.

