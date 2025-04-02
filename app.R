# WarehouseVision: Enhanced App with Multi-Component Shelf Support
# Main Shiny Application

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "WarehouseVision",
    dropdownMenu(
      type = "notifications", 
      notificationItem(
        text = "System initialized",
        icon = icon("check"),
        status = "success"
      )
    )
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Warehouse Layout", tabName = "layout", icon = icon("map")),
      menuItem("Shelf Details", tabName = "shelf_details", icon = icon("th-large")),
      menuItem("Component Search", tabName = "search", icon = icon("search")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Optimization", tabName = "optimization", icon = icon("lightbulb"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_components_box", width = 3),
          valueBoxOutput("total_inventory_box", width = 3),
          valueBoxOutput("high_velocity_box", width = 3),
          valueBoxOutput("low_stock_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Warehouse Overview",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("dashboard_layout", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Components by Tier",
            solidHeader = TRUE,
            status = "primary",
            width = 6,
            plotlyOutput("tier_distribution", height = "300px")
          ),
          box(
            title = "Component Velocity",
            solidHeader = TRUE,
            status = "info",
            width = 6,
            plotlyOutput("velocity_pie", height = "300px")
          )
        )
      ),
      
      # Warehouse Layout Tab
      tabItem(
        tabName = "layout",
        fluidRow(
          box(
            title = "Layout Visualization Settings",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            selectInput(
              "color_by",
              "Color By:",
              choices = c("Zone" = "zone", 
                         "Category" = "category", 
                         "Velocity" = "velocity",
                         "Utilization %" = "utilization",
                         "Component Count" = "component_count"),
              selected = "zone"
            ),
            selectInput(
              "show_tier",
              "Show Tier:",
              choices = c("All", "Bottom", "Middle", "Top"),
              selected = "All"
            ),
            checkboxInput("show_labels", "Show Bin Labels", value = FALSE),
            actionButton("reset_view", "Reset View", icon = icon("undo")),
            br(), br(),
            helpText("Click on any shelf unit in the layout to see its detailed view in the Shelf Details tab.")
          ),
          box(
            title = "Warehouse Layout",
            solidHeader = TRUE,
            status = "primary",
            width = 9,
            plotlyOutput("warehouse_layout", height = "600px")
          )
        )
      ),
      
      # Shelf Details Tab
      tabItem(
        tabName = "shelf_details",
        fluidRow(
          box(
            title = "Shelf Unit Selection",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            selectInput(
              "shelf_unit_id",
              "Select Shelf Unit:",
              choices = c("Select a unit" = "")
            ),
            helpText("Select a shelf unit to view its detailed multi-tier layout showing all components."),
            actionButton("view_shelf", "View Shelf Details", icon = icon("search"))
          ),
          box(
            title = "Component Distribution",
            solidHeader = TRUE,
            status = "info",
            width = 9,
            plotlyOutput("component_co_location", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = textOutput("shelf_detail_title"),
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("shelf_detail_view", height = "500px")
          )
        )
      ),
      
      # Component Search Tab
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            title = "Advanced Component Search",
            solidHeader = TRUE,
            status = "primary",
            width = 4,
            textInput("search_term", "Search Term:", 
                      placeholder = "Enter component code or description"),
            selectInput(
              "search_type",
              "Component Type:",
              choices = c("All Types", "LAB", "BOM", "CAR", "ELC", "HYD", "FAS"),
              selected = "All Types"
            ),
            selectInput(
              "search_category",
              "Category:",
              choices = c("All Categories", "Labels", "Bill of Materials", "Carriers", 
                          "Electrical", "Hydraulic", "Fasteners"),
              selected = "All Categories"
            ),
            sliderInput(
              "price_range",
              "Price Range:",
              min = 0,
              max = 500,
              value = c(0, 500),
              step = 10
            ),
            checkboxInput("in_stock_only", "Show Only Items In Stock", value = FALSE),
            actionButton("search_button", "Search", icon = icon("search"), 
                         class = "btn-primary", width = "100%"),
            br(), br(),
            actionButton("reset_search", "Reset Filters", icon = icon("refresh"),
                         class = "btn-default", width = "100%")
          ),
          box(
            title = "Search Results",
            solidHeader = TRUE,
            status = "primary",
            width = 8,
            DTOutput("search_results")
          )
        ),
        fluidRow(
          tabBox(
            title = "Component Details",
            width = 12,
            tabPanel(
              "Component Information",
              fluidRow(
                column(
                  width = 4,
                  uiOutput("component_details")
                ),
                column(
                  width = 8,
                  plotlyOutput("component_history", height = "250px")
                )
              )
            ),
            tabPanel(
              "Location Map",
              plotlyOutput("component_location", height = "500px")
            ),
            tabPanel(
              "Inventory Levels",
              plotlyOutput("inventory_levels", height = "300px")
            )
          )
        )
      ),
      
      # Analytics Tab
      tabItem(
        tabName = "analytics",
        fluidRow(
          tabBox(
            title = "Component Analytics",
            width = 12,
            tabPanel(
              "Movement Patterns",
              plotlyOutput("movement_analysis", height = "400px")
            ),
            tabPanel(
              "Category Distribution",
              plotlyOutput("category_analysis", height = "400px")
            ),
            tabPanel(
              "Movement Types",
              plotlyOutput("movement_types", height = "400px")
            ),
            tabPanel(
              "Velocity Analysis",
              plotlyOutput("velocity_analysis", height = "400px")
            ),
            tabPanel(
              "Multi-Component Analysis",
              plotlyOutput("multi_component_analysis", height = "400px")
            )
          )
        )
      ),
      
      # Optimization Tab
      tabItem(
        tabName = "optimization",
        fluidRow(
          box(
            title = "Placement Optimization Opportunities",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            DTOutput("optimization_table")
          )
        ),
        fluidRow(
          box(
            title = "Component Grouping Recommendations",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            DTOutput("grouping_table")
          )
        ),
        fluidRow(
          box(
            title = "Optimization Impact",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("optimization_impact", height = "400px")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load or generate warehouse data
  warehouse_data <- reactiveVal(load_warehouse_data())
  
  # Run analytics when data is loaded
  analysis_results <- reactive({
    analyze_component_movement(warehouse_data())
  })
  
  # Get optimization recommendations
  optimization_results <- reactive({
    identify_optimization_opportunities(warehouse_data())
  })
  
  # Selected component for highlighting
  selected_component <- reactiveVal(NULL)
  
  # Selected shelf unit for detailed view
  selected_shelf_unit <- reactiveVal(NULL)
  
  # Populate shelf unit dropdown choices
  observe({
    shelving_units <- warehouse_data()$shelving_units
    
    if (!is.null(shelving_units)) {
      updateSelectInput(
        session,
        "shelf_unit_id",
        choices = c("Select a unit" = "", shelving_units$unit_id)
      )
    }
  })
  
  # Update selected shelf unit when dropdown changes
  observeEvent(input$view_shelf, {
    if (input$shelf_unit_id != "") {
      selected_shelf_unit(input$shelf_unit_id)
      # Switch to the shelf details tab
      updateTabItems(session, "sidebarMenu", "shelf_details")
    }
  })
  
  # Update selected shelf when clicking on main layout
  observeEvent(event_data("plotly_click", source = "warehouse_layout"), {
    click_data <- event_data("plotly_click", source = "warehouse_layout")
    
    if (!is.null(click_data)) {
      # Get point information and find associated shelf unit
      x_pos <- click_data$x
      y_pos <- click_data$y
      
      # Find which shelf unit was clicked
      shelving_units <- warehouse_data()$shelving_units
      
      for (i in 1:nrow(shelving_units)) {
        unit <- shelving_units[i, ]
        
        # Check if click is within this unit's boundaries
        if (x_pos >= unit$x_start && 
            x_pos <= (unit$x_start + unit$width) &&
            y_pos >= unit$y_start && 
            y_pos <= (unit$y_start + unit$length)) {
          
          # Set selected shelf unit
          selected_shelf_unit(unit$unit_id)
          
          # Update dropdown
          updateSelectInput(session, "shelf_unit_id", selected = unit$unit_id)
          
          # Switch to shelf details tab
          updateTabItems(session, "sidebarMenu", "shelf_details")
          
          break
        }
      }
    }
  })
  
  # Dashboard Value Boxes
  output$total_components_box <- renderValueBox({
    components <- warehouse_data()$components
    valueBox(
      nrow(components),
      "Total Components",
      icon = icon("box"),
      color = "blue"
    )
  })
  
  output$total_inventory_box <- renderValueBox({
    inventory <- warehouse_data()$inventory
    valueBox(
      formatC(sum(inventory$quantity), format = "d", big.mark = ","),
      "Total Items in Stock",
      icon = icon("warehouse"),
      color = "green"
    )
  })
  
  output$high_velocity_box <- renderValueBox({
    components <- warehouse_data()$components
    high_velocity_count <- sum(components$velocity_category == "High", na.rm = TRUE)
    valueBox(
      high_velocity_count,
      "High Velocity Components",
      icon = icon("bolt"),
      color = "yellow"
    )
  })
  
  output$low_stock_box <- renderValueBox({
    # Calculate components with low stock (below minimum threshold)
    inventory <- warehouse_data()$inventory
    components <- warehouse_data()$components
    
    low_stock <- inventory %>%
      left_join(components, by = "component_id") %>%
      filter(!is.na(minimum_stock)) %>%
      filter(quantity < minimum_stock) %>%
      pull(component_id) %>%
      unique() %>%
      length()
    
    valueBox(
      low_stock,
      "Low Stock Items",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Dashboard Layout
  output$dashboard_layout <- renderPlotly({
    create_warehouse_layout(warehouse_data(), color_by = "velocity", show_labels = FALSE)
  })
  
  # Distribution by tier
  output$tier_distribution <- renderPlotly({
    plot_component_distribution_by_tier(warehouse_data())
  })
  
  # Velocity Pie Chart
  output$velocity_pie <- renderPlotly({
    plot_velocity_distribution(analysis_results())
  })
  
  # Warehouse Layout Visualization
  output$warehouse_layout <- renderPlotly({
    p <- create_warehouse_layout(
      warehouse_data(), 
      color_by = input$color_by,
      show_labels = input$show_labels,
      show_tier = input$show_tier,
      highlight_component = selected_component()
    )
    
    # Add source for click events
    p <- p %>% event_register("plotly_click", "warehouse_layout")
    
    return(p)
  })
  
  # Component co-location heatmap
  output$component_co_location <- renderPlotly({
    create_component_heatmap(warehouse_data())
  })
  
  # Shelf detail title
  output$shelf_detail_title <- renderText({
    req(selected_shelf_unit())
    paste