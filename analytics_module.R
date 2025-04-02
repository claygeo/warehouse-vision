# WarehouseVision: Component Analytics Module
# This module provides analytical insights about component usage, placement, and optimization

library(tidyverse)
library(plotly)
library(lubridate)

# Function to analyze component movement patterns
analyze_component_movement <- function(warehouse_data) {
  
  # Extract relevant data
  movement_log <- warehouse_data$movement_log
  components <- warehouse_data$components
  
  # Movement frequency over time
  movement_time_series <- movement_log %>%
    group_by(movement_date) %>%
    summarise(
      num_movements = n(),
      total_quantity = sum(quantity_moved)
    ) %>%
    arrange(movement_date)
  
  # Movement by component category
  movement_by_category <- movement_log %>%
    left_join(components, by = "component_id") %>%
    group_by(category) %>%
    summarise(
      num_movements = n(),
      total_quantity = sum(quantity_moved),
      avg_quantity = mean(quantity_moved)
    ) %>%
    arrange(desc(num_movements))
  
  # Movement type distribution
  movement_types <- movement_log %>%
    group_by(movement_type) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Component velocity analysis
  velocity_distribution <- components %>%
    group_by(velocity_category) %>%
    summarise(
      count = n(),
      percentage = n() / nrow(components) * 100
    )
  
  # Return a list of analysis results
  return(list(
    time_series = movement_time_series,
    by_category = movement_by_category,
    movement_types = movement_types,
    velocity = velocity_distribution
  ))
}

# Function to visualize movement time series
plot_movement_time_series <- function(analysis_results) {
  
  time_series <- analysis_results$time_series
  
  p <- plot_ly() %>%
    add_trace(
      data = time_series,
      x = ~movement_date,
      y = ~num_movements,
      type = "scatter",
      mode = "lines",
      name = "# of Movements",
      line = list(color = "#1f77b4")
    ) %>%
    add_trace(
      data = time_series,
      x = ~movement_date,
      y = ~total_quantity,
      type = "scatter",
      mode = "lines",
      name = "Total Quantity Moved",
      yaxis = "y2",
      line = list(color = "#ff7f0e")
    ) %>%
    layout(
      title = "Component Movement Over Time",
      xaxis = list(title = "Date"),
      yaxis = list(
        title = "Number of Movements",
        titlefont = list(color = "#1f77b4"),
        tickfont = list(color = "#1f77b4")
      ),
      yaxis2 = list(
        title = "Total Quantity Moved",
        overlaying = "y",
        side = "right",
        titlefont = list(color = "#ff7f0e"),
        tickfont = list(color = "#ff7f0e")
      ),
      legend = list(x = 0.1, y = 1),
      margin = list(l = 60, r = 60, b = 60, t = 60, pad = 4)
    )
  
  return(p)
}

# Function to visualize category distribution
plot_category_distribution <- function(analysis_results) {
  
  by_category <- analysis_results$by_category
  
  p <- plot_ly() %>%
    add_trace(
      data = by_category,
      x = ~reorder(category, num_movements),
      y = ~num_movements,
      type = "bar",
      name = "# of Movements",
      marker = list(color = "#1f77b4")
    ) %>%
    add_trace(
      data = by_category,
      x = ~reorder(category, num_movements),
      y = ~total_quantity,
      type = "bar",
      name = "Total Quantity Moved",
      yaxis = "y2",
      marker = list(color = "#ff7f0e")
    ) %>%
    layout(
      title = "Component Movement by Category",
      xaxis = list(title = "Category"),
      yaxis = list(
        title = "Number of Movements",
        titlefont = list(color = "#1f77b4"),
        tickfont = list(color = "#1f77b4")
      ),
      yaxis2 = list(
        title = "Total Quantity Moved",
        overlaying = "y",
        side = "right",
        titlefont = list(color = "#ff7f0e"),
        tickfont = list(color = "#ff7f0e")
      ),
      barmode = "group",
      legend = list(x = 0.1, y = 1),
      margin = list(l = 60, r = 60, b = 60, t = 60, pad = 4)
    )
  
  return(p)
}

# Function to visualize movement types
plot_movement_types <- function(analysis_results) {
  
  movement_types <- analysis_results$movement_types
  
  p <- plot_ly(
    data = movement_types,
    labels = ~movement_type,
    values = ~count,
    type = "pie",
    textinfo = "label+percent",
    insidetextorientation = "radial",
    marker = list(
      colors = c("#1f77b4", "#ff7f0e", "#2ca02c")
    )
  ) %>%
    layout(
      title = "Movement Type Distribution",
      showlegend = TRUE
    )
  
  return(p)
}

# Function to visualize velocity categories
plot_velocity_distribution <- function(analysis_results) {
  
  velocity <- analysis_results$velocity
  
  p <- plot_ly(
    data = velocity,
    x = ~velocity_category,
    y = ~count,
    type = "bar",
    text = ~paste0(round(percentage, 1), "%"),
    textposition = "auto",
    marker = list(
      color = c("#d62728", "#ff7f0e", "#2ca02c", "#7f7f7f")
    )
  ) %>%
    layout(
      title = "Component Velocity Distribution",
      xaxis = list(title = "Velocity Category"),
      yaxis = list(title = "Number of Components"),
      showlegend = FALSE
    )
  
  return(p)
}

# Function to identify component placement optimization opportunities
identify_optimization_opportunities <- function(warehouse_data) {
  
  # Extract data
  inventory <- warehouse_data$inventory
  components <- warehouse_data$components
  bins <- warehouse_data$bins
  
  # Join inventory with component and bin data
  inventory_analysis <- inventory %>%
    left_join(components, by = "component_id") %>%
    left_join(bins, by = "bin_id")
  
  # Find high-velocity items in inefficient locations (far from shipping/receiving)
  # Assuming zone A is closest to shipping/receiving area
  inefficient_placement <- inventory_analysis %>%
    filter(velocity_category == "High", zone != "A") %>%
    select(component_id, name, category, bin_id, zone, velocity_category) %>%
    mutate(
      recommendation = paste0("Move from zone ", zone, " to zone A for efficiency"),
      potential_impact = case_when(
        zone == "B" ~ "Medium",
        zone == "C" ~ "High"
      )
    )
  
  # Find frequently co-accessed components that are stored far apart
  # This is a simplified version - in a real system, we would analyze order data
  # Here we're just using a random subset as an example
  component_pairs <- tibble(
    component_1 = sample(components$component_id[components$velocity_category == "High"], 10),
    component_2 = sample(components$component_id[components$velocity_category == "High"], 10),
    co_access_frequency = sample(5:20, 10)
  ) %>%
    left_join(inventory %>% select(component_id, bin_id) %>% rename(component_1 = component_id, bin_1 = bin_id), by = "component_1") %>%
    left_join(inventory %>% select(component_id, bin_id) %>% rename(component_2 = component_id, bin_2 = bin_id), by = "component_2") %>%
    left_join(bins %>% select(bin_id, zone) %>% rename(bin_1 = bin_id, zone_1 = zone), by = "bin_1") %>%
    left_join(bins %>% select(bin_id, zone) %>% rename(bin_2 = bin_id, zone_2 = zone), by = "bin_2") %>%
    filter(zone_1 != zone_2) %>%  # Only include pairs in different zones
    mutate(
      recommendation = paste0("Consider placing components ", component_1, " and ", component_2, " closer together"),
      potential_impact = case_when(
        co_access_frequency >= 15 ~ "High",
        co_access_frequency >= 10 ~ "Medium",
        TRUE ~ "Low"
      )
    )
  
  # Return optimization recommendations
  return(list(
    inefficient_placement = inefficient_placement,
    component_pairs = component_pairs
  ))
}

# Example usage (uncomment to run):
# # Load the data
# warehouse_data <- readRDS("warehouse_data.rds")
# 
# # Perform analysis
# analysis_results <- analyze_component_movement(warehouse_data)
# 
# # Create visualizations
# plot_movement_time_series(analysis_results)
# plot_category_distribution(analysis_results)
# plot_movement_types(analysis_results)
# plot_velocity_distribution(analysis_results)
# 
# # Get optimization recommendations
# optimization_results <- identify_optimization_opportunities(warehouse_data)
# View(optimization_results$inefficient_placement)
# View(optimization_results$component_pairs)