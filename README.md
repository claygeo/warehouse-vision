# Warehouse Vision
A Shiny R web application for managing a U-shaped warehouse with multi-component shelves. It provides interactive visualizations of warehouse layouts, shelf details, and component inventories, along with analytics for movement patterns and storage optimization. Built as a standalone demo with generated sample data, it supports warehouse operations analysis and component search.

## Table of Contents 
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Setup](#setup)
- [Visuals](#visuals)
- [Notes](#notes)

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

1. Clone the repository: git clone https://github.com/claygeo/warehouse-vision.git
2. Navigate to the project directory: cd warehouse-vision
3. Install R packages: install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "lubridate", "viridis", "viridisLite"))
4. Run the app in RStudio (open standalone_warehouse_app.R and click "Run App") or in R:library(shiny) runApp("standalone_warehouse_app.R")

## Visuals

OT Overview:
![image](https://github.com/user-attachments/assets/d4a47a18-3bf3-43d3-9a05-8bf5e516b2c3)
OT Trend: 
![image](https://github.com/user-attachments/assets/038a6642-6d98-4c2d-ba86-b78abb01123a)
UPLH Review
![image](https://github.com/user-attachments/assets/73c0c868-0f06-4250-a8a3-ad394fd0deea)
Equipment Analysis
![image](https://github.com/user-attachments/assets/464db378-1123-4af0-8084-db5886d1736e)

## Notes

- The app generates sample data internally; no external CSV files are required.
- Designed as a standalone demo.
- For the modular version, use main.R or run_warehouse_vision.R with the R/ folder modules.

