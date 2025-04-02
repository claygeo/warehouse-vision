# WarehouseVision: Enhanced Component Search Module for Multi-Component Shelves
# This module provides advanced component search functionality

library(shiny)
library(DT)
library(plotly)
library(tidyverse)

# This function defines the UI for the search tab 
# But we're now using the UI defined in app.R directly
# Keeping this function for reference if needed
search_tab_ui <- function() {
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
  )
}

# Server logic for enhanced search
# This function is not used anymore as the logic is integrated directly into app.R
# Keeping for reference
component_search_server <- function(input, output, session) {
  # Store the selected component ID
  selected_component <- reactiveVal(NULL)
  
  # Reset search filters
  observeEvent(input$reset_search, {
    updateTextInput(session, "search_term", value = "")
    updateSelectInput(session, "search_type", selected = "All Types")
    updateSelectInput(session, "search_category", selected = "All Categories")
    updateSliderInput(session, "price_range", value = c(0, 500))
    updateCheckboxInput(session, "in_stock_only", value = FALSE)
  })
  
  # Enhanced search results
  search_results <- reactive({
    req(input$search_button)
    
    # Start with the reference data
    filtered <- warehouse_data()$components
    
    # Filter by search term (in both code and description)
    if(nchar(input$search_term) > 0) {
      filtered <- filtered %>%
        filter(
          grepl(input$search_term, component_id, ignore.case = TRUE) | 
          grepl(input$search_term, name, ignore.case = TRUE)
        )
    }
    
    # Filter by component type
    if(input$search_type != "All Types") {
      filtered <- filtered %>%
        filter(type == input$search_type)
    }
    
    # Filter by category
    if(input$search_category != "All Categories") {
      filtered <- filtered %>%
        filter(category == input$search_category)
    }
    
    # Filter by price range
    filtered <- filtered %>%
      filter(unit_price >= input$price_range[1] & unit_price <= input$price_range[2])
    
    # Get inventory data
    inventory <- warehouse_data()$inventory
    
    # Join with inventory data
    result <- filtered %>%
      left_join(
        inventory %>% 
          group_by(component_id) %>% 
          summarise(
            total_quantity = sum(quantity, na.rm = TRUE),
            bin_locations = n(),
            .groups = "drop"
          ),
        by = "component_id"
      ) %>%
      mutate(
        total_quantity = coalesce(total_quantity, 0),
        bin_locations = coalesce(bin_locations, 0),
        status = case_when(
          total_quantity == 0 ~ "Out of Stock",
          total_quantity < minimum_stock ~ "Low Stock",
          total_quantity < minimum_stock * 2 ~ "Adequate",
          TRUE ~ "Fully Stocked"
        )
      )
    
    # Filter for in-stock only if requested
    if(input$in_stock_only) {
      result <- result %>% filter(total_quantity > 0)
    }
    
    return(result)
  })
  
  # Display search results with improved formatting
  output$search_results <- renderDT({
    results <- search_results() %>%
      select(
        component_id, 
        name, 
        category,
        total_quantity,
        bin_locations,
        status
      )
    
    datatable(
      results,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = 5,  # Status column
            render = JS(
              "function(data, type, row, meta) {
                if (type === 'display') {
                  var color;
                  if (data === 'Out of Stock') {
                    color = 'red';
                  } else if (data === 'Low Stock') {
                    color = 'orange';
                  } else if (data === 'Adequate') {
                    color = 'blue';
                  } else {
                    color = 'green';
                  }
                  return '<span style=\"color:' + color + '\">' + data + '</span>';
                }
                return data;
              }"
            )
          )
        )
      )
    ) %>%
      formatCurrency('total_quantity', currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Update selected component when search result is clicked
  observeEvent(input$search_results_rows_selected, {
    selected_row <- input$search_results_rows_selected
    if(length(selected_row) > 0) {
      component_id <- search_results()[selected_row, "component_id"]
      selected_component(component_id)
    }
  })
  
  return(selected_component)
}