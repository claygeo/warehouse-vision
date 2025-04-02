# WarehouseVision: Enhanced Visualization Module for Multi-Component Shelves
# This module provides the visualization functions for the warehouse layout

library(tidyverse)
library(plotly)
library(shiny)

# Function to create a basic warehouse floor plan visualization
create_warehouse_layout <- function(warehouse_data, 
                                   highlight_bin = NULL, 
                                   highlight_component = NULL, 
                                   show_labels = TRUE,
                                   color_by = "zone",
                                   show_tier = "All",
                                   detailed_view = FALSE) {
  
  # Extract data components
  shelving_units <- warehouse_data$shelving_units
  bins <- warehouse_data$bins
  inventory <- warehouse_data$inventory
  components <- warehouse_data$components
  warehouse_dims <- warehouse_data$warehouse_dimensions
  
  # Filter bins by tier if specified
  if (show_tier != "All") {
    bins <- bins %>% filter(tier == show_tier)
    
    # Filter inventory to match
    inventory <- inventory %>% filter(tier == show_tier)
  }
  
  # Join inventory data to bins for visualization
  bins_with_inventory <- bins %>%
    left_join(inventory, by = "bin_id") %>%
    left_join(components, by = "component_id")
  
  # Determine color mapping based on user selection
  if (color_by == "zone") {
    color_column <- "zone"
    color_title <- "Zone"
    colorscale <- list(Wall = "#1f77b4", Island = "#ff7f0e")
  } else if (color_by == "category") {
    color_column <- "category" 
    color_title <- "Component Category"
    # Generate colors based on unique categories
    categories <- unique(components$category)
    colorscale <- setNames(
      viridisLite::viridis(length(categories)),
      categories
    )
  } else if (color_by == "velocity") {
    color_column <- "velocity_category"
    color_title <- "Velocity"
    colorscale <- list(High = "#d62728", Medium = "#ff7f0e", Low = "#2ca02c", None = "#7f7f7f")
  } else if (color_by == "utilization") {
    color_column <- "capacity_percentage"
    color_title <- "Utilization %"
    # This will be handled differently for continuous color scale
  } else if (color_by == "component_count") {
    # Precompute component counts per bin
    bin_component_counts <- inventory %>%
      group_by(bin_id) %>%
      summarise(component_count = n_distinct(component_id), .groups = "drop")
    
    bins_with_inventory <- bins_with_inventory %>%
      left_join(bin_component_counts, by = "bin_id")
    
    color_column <- "component_count"
    color_title <- "Component Count"
    # This will be handled differently for continuous color scale
  } else {
    color_column <- "zone"
    color_title <- "Zone"
    colorscale <- list(Wall = "#1f77b4", Island = "#ff7f0e")
  }
  
  # Create the base plot
  p <- ggplot() +
    # Draw warehouse outline
    geom_rect(aes(xmin = 0, xmax = warehouse_dims$width, 
                  ymin = 0, ymax = warehouse_dims$length),
              fill = "white", color = "black", alpha = 0.1) +
    # Draw shelving units
    geom_rect(data = shelving_units,
              aes(xmin = x_start, xmax = x_start + width,
                  ymin = y_start, ymax = y_start + length,
                  fill = zone),
              color = "black", alpha = 0.3) +
    # Customize the theme
    theme_minimal() +
    labs(
      title = paste0("Warehouse Floor Plan", 
                    ifelse(show_tier != "All", paste0(" - ", show_tier, " Tier"), "")),
      x = "Width (feet)",
      y = "Length (feet)",
      fill = color_title
    ) +
    coord_fixed(ratio = 1)
  
  # Handle bins differently based on color_by type
  if (color_by %in% c("utilization", "component_count")) {
    # Continuous color scale
    p <- p + 
      geom_rect(data = bins_with_inventory,
               aes(xmin = x_position, 
                   xmax = x_position + section_width,
                   ymin = y_position, 
                   ymax = y_position + (ifelse(zone == "Wall", length, width)),
                   fill = .data[[color_column]],
                   text = paste0(
                     "Bin: ", bin_id, "<br>",
                     "Tier: ", tier, "<br>",
                     "Components: ", ifelse(is.na(component_count), 0, component_count), "<br>",
                     "Utilization: ", ifelse(is.na(capacity_percentage), 0, 
                                            paste0(capacity_percentage, "%")), "<br>",
                     "Total Quantity: ", ifelse(is.na(total_quantity), 0, total_quantity)
                   )),
               color = "black") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90")
  } else {
    # Categorical color scale
    p <- p + 
      geom_rect(data = bins_with_inventory,
               aes(xmin = x_position, 
                   xmax = x_position + section_width,
                   ymin = y_position, 
                   ymax = y_position + (ifelse(zone == "Wall", length, width)),
                   fill = .data[[color_column]],
                   text = paste0(
                     "Bin: ", bin_id, "<br>",
                     "Tier: ", tier, "<br>",
                     "Component: ", coalesce(name, "Empty"), "<br>",
                     "Category: ", coalesce(category, "N/A"), "<br>",
                     "Quantity: ", coalesce(as.character(quantity), "0"), "<br>",
                     "Last Accessed: ", coalesce(as.character(last_accessed), "N/A")
                   )),
               color = "black") +
      scale_fill_manual(values = colorscale, na.value = "gray90")
  }
  
  # Add bin labels if requested
  if (show_labels) {
    # For detailed view, show all labels
    if (detailed_view) {
      p <- p + 
        geom_text(data = bins,
                 aes(x = x_position + section_width/2, 
                     y = y_position + (ifelse(zone == "Wall", length, width))/2, 
                     label = bin_id),
                 size = 2.5, alpha = 0.7)
    } else {
      # For overview, sample some bins to avoid clutter
      p <- p + 
        geom_text(data = bins %>% filter(bin_id %in% sample(bin_id, min(30, nrow(bins)))),
                 aes(x = x_position + section_width/2, 
                     y = y_position + (ifelse(zone == "Wall", length, width))/2, 
                     label = bin_id),
                 size = 2.5, alpha = 0.7)
    }
  }
  
  # Highlight specific bin if requested
  if (!is.null(highlight_bin)) {
    highlight_data <- bins %>% filter(bin_id == highlight_bin)
    if (nrow(highlight_data) > 0) {
      p <- p + 
        geom_rect(data = highlight_data,
                 aes(xmin = x_position, 
                     xmax = x_position + section_width,
                     ymin = y_position, 
                     ymax = y_position + (ifelse(zone == "Wall", length, width))),
                 fill = NA, color = "red", size = 1.5)
    }
  }
  
  # Highlight bins containing a specific component if requested
  if (!is.null(highlight_component)) {
    highlight_bins <- inventory %>% 
      filter(component_id == highlight_component) %>%
      pull(bin_id)
    
    if (length(highlight_bins) > 0) {
      highlight_data <- bins %>% filter(bin_id %in% highlight_bins)
      p <- p + 
        geom_rect(data = highlight_data,
                 aes(xmin = x_position, 
                     xmax = x_position + section_width,
                     ymin = y_position, 
                     ymax = y_position + (ifelse(zone == "Wall", length, width))),
                 fill = NA, color = "red", size = 1.5)
    }
  }
  
  # Convert to an interactive plotly object
  p_interactive <- ggplotly(p, tooltip = "text")
  
  # Add layout options for plotly
  p_interactive <- p_interactive %>%
    layout(
      hoverlabel = list(bgcolor = "white", font = list(size =