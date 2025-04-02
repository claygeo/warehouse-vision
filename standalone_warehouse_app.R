# WarehouseVision: Enhanced U-Shaped Warehouse with Multi-Component Shelves
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(viridis)
library(viridisLite)

# Define warehouse dimensions for U-shaped layout
warehouse_width <- 120  # in feet
warehouse_length <- 80  # in feet

# Create U-shaped shelving layout
# Left wall
left_shelves <- tibble(
  unit_id = paste0("L", 1:8),
  x_start = 10,
  y_start = seq(10, 70, length.out = 8),
  width = 5,
  length = 8,
  zone = "Wall"
)

# Bottom wall
bottom_shelves <- tibble(
  unit_id = paste0("B", 1:10),
  x_start = seq(15, 105, length.out = 10),
  y_start = 10,
  width = 8,
  length = 5,
  zone = "Wall"
)

# Right wall
right_shelves <- tibble(
  unit_id = paste0("R", 1:8),
  x_start = 105,
  y_start = seq(10, 70, length.out = 8),
  width = 5,
  length = 8,
  zone = "Wall"
)

# Center island (double-sided)
center_shelves <- tibble(
  unit_id = paste0("C", 1:8),
  x_start = 60,
  y_start = seq(25, 65, length.out = 8),
  width = 5,
  length = 10,
  zone = "Island"
)

# Combine all shelving units
shelving_units <- bind_rows(left_shelves, bottom_shelves, right_shelves, center_shelves)

# Define tiers
tiers <- c("Bottom", "Middle", "Top")

# Create shelf sections (dividing each tier into segments that can hold different components)
shelf_sections <- expand_grid(
  unit_id = shelving_units$unit_id,
  tier = tiers,
  section = 1:4  # Each tier can have up to 4 sections
) %>%
  left_join(shelving_units, by = "unit_id") %>%
  mutate(
    section_width = case_when(
      zone == "Wall" & tier == "Bottom" ~ width / 1,
      zone == "Wall" & tier == "Middle" ~ width / 4,
      zone == "Wall" & tier == "Top" ~ width / 2,
      zone == "Island" & tier == "Bottom" ~ width / 1,
      zone == "Island" & tier == "Middle" ~ width / 4,
      zone == "Island" & tier == "Top" ~ width / 2,
      TRUE ~ width / 2
    ),
    valid_section = case_when(
      tier == "Bottom" ~ section == 1,
      tier == "Middle" ~ section <= 4,
      tier == "Top" ~ section <= 2,
      TRUE ~ FALSE
    ),
    section_x_offset = case_when(
      tier == "Bottom" ~ 0,
      tier == "Middle" ~ (section - 1) * section_width,
      tier == "Top" ~ (section - 1) * section_width * 2,
      TRUE ~ 0
    ),
    section_id = paste(unit_id, tier, section, sep = "-")
  ) %>%
  filter(valid_section)

# Create bin IDs for each section
bins <- shelf_sections %>%
  mutate(
    bin_id = paste0(substr(unit_id, 1, 1), 
                    substr(tier, 1, 1), 
                    section, 
                    "-", 
                    sample(100:999, n(), replace = TRUE)),
    x_position = x_start + section_x_offset,
    y_position = y_start,
    capacity = case_when(
      tier == "Bottom" ~ as.integer(runif(n(), 8000, 15000)),
      tier == "Middle" ~ as.integer(runif(n(), 4000, 8000)),
      tier == "Top" ~ as.integer(runif(n(), 2000, 4000))
    )
  ) %>%
  select(bin_id, unit_id, tier, section, section_id, x_position, y_position, 
         zone, capacity, section_width)

# Create sample components data
component_types <- c("LAB", "BOM", "CAR", "ELC", "HYD", "FAS")
component_categories <- c("Labels", "Bill of Materials", "Carriers", 
                          "Electrical", "Hydraulic", "Fasteners")
type_to_category <- setNames(component_categories, component_types)

components <- tibble(
  component_id = c(
    paste0(rep(component_types, each = 20), 
           rep(c("019", "119", "129", "139", "149", "159", "169", "179", "189", "199"), 
               times = length(component_types)))
  ),
  name = paste(rep(component_categories, each = 20), 
               seq(1, 20), sep = " Type "),
  category = sapply(substr(component_id, 1, 3), function(x) type_to_category[x]),
  type = substr(component_id, 1, 3),
  dimensions = paste0(sample(2:10, length(component_id), replace = TRUE), "x",
                      sample(2:10, length(component_id), replace = TRUE), "x",
                      sample(1:5, length(component_id), replace = TRUE)),
  unit_weight = round(runif(length(component_id), 0.1, 5), 2),
  unit_price = round(runif(length(component_id), 5, 500), 2),
  minimum_stock = sample(10:100, length(component_id), replace = TRUE),
  lead_time_days = sample(1:30, length(component_id), replace = TRUE)
)

# Generate inventory with multiple components per bin
bin_component_count <- bins %>%
  mutate(
    component_count = case_when(
      tier == "Bottom" ~ 1,
      tier == "Middle" ~ sample(1:4, n(), replace = TRUE),
      tier == "Top" ~ sample(1:2, n(), replace = TRUE)
    )
  )

# Generate inventory records
inventory <- tibble()

for (i in 1:nrow(bin_component_count)) {
  bin <- bin_component_count[i,]
  
  n_components <- bin$component_count
  
  if (bin$zone == "Wall") {
    if (bin$tier == "Bottom") {
      component_pool <- components %>% filter(type %in% c("CAR", "HYD", "FAS"))
    } else if (bin$tier == "Middle") {
      component_pool <- components %>% filter(type %in% c("BOM", "ELC", "FAS", "HYD"))
    } else {
      component_pool <- components %>% filter(type %in% c("LAB", "BOM", "ELC"))
    }
  } else {
    if (bin$tier == "Bottom") {
      component_pool <- components %>% filter(type %in% c("CAR", "FAS"))
    } else if (bin$tier == "Middle") {
      component_pool <- components %>% filter(type %in% c("BOM", "ELC", "LAB"))
    } else {
      component_pool <- components %>% filter(type %in% c("LAB", "ELC"))
    }
  }
  
  if (nrow(component_pool) == 0) {
    component_pool <- components
  }
  
  selected_components <- sample_n(component_pool, 
                                  size = min(n_components, nrow(component_pool)), 
                                  replace = FALSE)
  
  for (j in 1:nrow(selected_components)) {
    component <- selected_components[j,]
    
    remaining_capacity <- ifelse(j == 1, 
                                 bin$capacity, 
                                 bin$capacity / n_components)
    
    new_record <- tibble(
      inventory_id = paste0("INV-", sample(100000:999999, 1)),
      bin_id = bin$bin_id,
      component_id = component$component_id,
      unit_id = bin$unit_id,
      tier = bin$tier,
      section = bin$section,
      quantity = min(sample(100:5000, 1), remaining_capacity),
      capacity_percentage = NA_real_,
      last_updated = sample(seq(Sys.Date() - 90, Sys.Date(), by = "day"), 1),
      last_accessed = sample(seq(Sys.Date() - 60, Sys.Date(), by = "day"), 1)
    )
    
    inventory <- bind_rows(inventory, new_record)
  }
}

# Calculate capacity percentages (corrected version)
inventory_with_bins <- inventory %>%
  left_join(bins %>% select(bin_id, capacity), by = "bin_id") %>%
  group_by(bin_id) %>%
  mutate(
    total_quantity = sum(quantity),
    capacity_percentage = round((total_quantity / capacity) * 100, 1)
  ) %>%
  ungroup() %>%
  select(-capacity)  # Remove temporary capacity column after calculation

# Create sample movement data
movement_log <- tibble(
  movement_id = 1:1000,
  component_id = sample(components$component_id, 1000, replace = TRUE),
  quantity_moved = sample(c(
    sample(10:100, 700, replace = TRUE),
    sample(101:1000, 200, replace = TRUE),
    sample(1001:2000, 100, replace = TRUE)
  ), 1000, replace = TRUE),
  movement_type = sample(c("Inbound", "Outbound", "Internal Transfer"), 1000, replace = TRUE, 
                         prob = c(0.3, 0.5, 0.2)),
  movement_date = sample(seq(Sys.Date() - 90, Sys.Date(), by = "day"), 1000, replace = TRUE),
  user_id = sample(paste0("User", 1:5), 1000, replace = TRUE),
  bin_id = sample(inventory$bin_id, 1000, replace = TRUE)
)

# Calculate component velocity
component_velocity <- movement_log %>%
  group_by(component_id) %>%
  summarise(
    num_movements = n(),
    total_quantity = sum(quantity_moved),
    last_movement = max(movement_date),
    .groups = "drop"
  ) %>%
  mutate(
    velocity_category = case_when(
      num_movements > 10 ~ "High",
      num_movements > 5 ~ "Medium",
      TRUE ~ "Low"
    )
  )

components <- components %>%
  left_join(component_velocity, by = "component_id") %>%
  mutate(
    num_movements = replace_na(num_movements, 0),
    total_quantity = replace_na(total_quantity, 0),
    velocity_category = replace_na(velocity_category, "None")
  )

# Create warehouse data structure
warehouse_data <- list(
  warehouse_dimensions = list(width = warehouse_width, length = warehouse_length),
  shelving_units = shelving_units,
  bins = bins,
  components = components,
  inventory = inventory,
  movement_log = movement_log
)

# Function to create warehouse floor plan for main visualization
create_warehouse_layout <- function(color_by = "zone", show_tier = "All", show_labels = FALSE, highlight_component = NULL) {
  bins_with_inventory <- bins %>%
    left_join(inventory, by = c("bin_id", "tier", "unit_id", "section")) %>%
    left_join(components, by = "component_id")
  
  if (show_tier != "All") {
    bins_with_inventory <- bins_with_inventory %>%
      filter(tier == show_tier)
  }
  
  if (color_by == "zone") {
    color_column <- "zone"
    color_title <- "Zone"
    colorscale <- list(Wall = "#1f77b4", Island = "#ff7f0e")
  } else if (color_by == "category") {
    color_column <- "category" 
    color_title <- "Component Category"
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
  } else if (color_by == "component_count") {
    bin_component_counts <- inventory %>%
      group_by(bin_id) %>%
      summarise(component_count = n_distinct(component_id), .groups = "drop")
    
    bins_with_inventory <- bins_with_inventory %>%
      left_join(bin_component_counts, by = "bin_id")
    
    color_column <- "component_count"
    color_title <- "Component Count"
  } else {
    color_column <- "zone"
    color_title <- "Zone"
    colorscale <- list(Wall = "#1f77b4", Island = "#ff7f0e")
  }
  
  p <- plot_ly(type = 'scatter', mode = 'markers')
  
  p <- p %>% add_trace(
    x = c(0, warehouse_width, warehouse_width, 0, 0),
    y = c(0, 0, warehouse_length, warehouse_length, 0),
    mode = "lines",
    line = list(color = 'gray', width = 2),
    showlegend = FALSE,
    name = "Warehouse Walls"
  )
  
  for (i in 1:nrow(shelving_units)) {
    unit <- shelving_units[i,]
    x <- c(unit$x_start, unit$x_start + unit$width, unit$x_start + unit$width, unit$x_start, unit$x_start)
    y <- c(unit$y_start, unit$y_start, unit$y_start + unit$length, unit$y_start + unit$length, unit$y_start)
    
    p <- p %>% add_trace(
      x = x,
      y = y,
      mode = "lines",
      line = list(color = 'black', width = 1),
      fill = "toself",
      fillcolor = ifelse(unit$zone == "Wall", "rgba(173, 216, 230, 0.5)", "rgba(144, 238, 144, 0.5)"),
      text = paste("Unit:", unit$unit_id, "<br>Zone:", unit$zone),
      hoverinfo = "text",
      name = paste("Unit:", unit$unit_id),
      showlegend = FALSE
    )
  }
  
  if (color_by %in% c("utilization", "component_count")) {
    for (i in 1:nrow(bins_with_inventory)) {
      bin <- bins_with_inventory[i, ]
      
      if (is.na(bin[[color_column]])) next
      
      if (color_by == "utilization") {
        color_val <- paste0("rgba(", 
                            round(255 - (bin$capacity_percentage * 2.55)), ",", 
                            round(255 - (bin$capacity_percentage * 1.5)), ",", 
                            "255, 0.7)")
      } else {
        comp_count <- ifelse(is.na(bin$component_count), 0, bin$component_count)
        color_val <- viridisLite::viridis(5)[min(comp_count + 1, 5)]
      }
      
      x_display <- case_when(
        bin$tier == "Bottom" ~ bin$x_position - 1,
        bin$tier == "Middle" ~ bin$x_position,
        bin$tier == "Top" ~ bin$x_position + 1,
        TRUE ~ bin$x_position
      )
      
      y_display <- case_when(
        bin$tier == "Bottom" ~ bin$y_position - 1,
        bin$tier == "Middle" ~ bin$y_position,
        bin$tier == "Top" ~ bin$y_position + 1,
        TRUE ~ bin$y_position
      )
      
      p <- p %>% add_trace(
        x = x_display,
        y = y_display,
        mode = "markers",
        marker = list(
          size = 10,
          color = color_val
        ),
        text = paste0(
          "Bin: ", bin$bin_id, "<br>",
          "Tier: ", bin$tier, "<br>",
          "Components: ", ifelse(is.na(bin$component_count), 0, bin$component_count), "<br>",
          "Utilization: ", ifelse(is.na(bin$capacity_percentage), 0, paste0(bin$capacity_percentage, "%")), "<br>",
          "Total Quantity: ", ifelse(is.na(bin$total_quantity), 0, bin$total_quantity)
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
      
      if (show_labels) {
        p <- p %>% add_trace(
          x = x_display,
          y = y_display,
          mode = "text",
          text = bin$bin_id,
          textposition = "top center",
          textfont = list(size = 8, color = "black"),
          showlegend = FALSE,
          hoverinfo = "none"
        )
      }
    }
  } else {
    unique_bins <- bins_with_inventory %>% 
      distinct(bin_id, tier, x_position, y_position, zone)
    
    for (i in 1:nrow(unique_bins)) {
      bin <- unique_bins[i, ]
      
      bin_components <- bins_with_inventory %>% 
        filter(bin_id == bin$bin_id) %>%
        filter(!is.na(component_id))
      
      if (nrow(bin_components) == 0) next
      
      primary_component <- bin_components[1, ]
      
      marker_color <- "gray"
      
      if (!is.na(primary_component[[color_column]])) {
        if (color_column %in% names(colorscale)) {
          marker_color <- colorscale[[primary_component[[color_column]]]]
        }
      }
      
      x_display <- case_when(
        bin$tier == "Bottom" ~ bin$x_position - 1,
        bin$tier == "Middle" ~ bin$x_position,
        bin$tier == "Top" ~ bin$x_position + 1,
        TRUE ~ bin$x_position
      )
      
      y_display <- case_when(
        bin$tier == "Bottom" ~ bin$y_position - 1,
        bin$tier == "Middle" ~ bin$y_position,
        bin$tier == "Top" ~ bin$y_position + 1,
        TRUE ~ bin$y_position
      )
      
      component_list <- paste(
        sapply(1:nrow(bin_components), function(j) {
          paste0(j, ". ", bin_components$component_id[j], 
                 " (", bin_components$category[j], ")")
        }),
        collapse = "<br>"
      )
      
      hover_text <- paste0(
        "Bin: ", bin$bin_id, "<br>",
        "Tier: ", bin$tier, "<br>",
        "Components (", nrow(bin_components), "):<br>",
        component_list
      )
      
      p <- p %>% add_trace(
        x = x_display,
        y = y_display,
        mode = "markers",
        marker = list(
          size = 10,
          color = marker_color
        ),
        text = hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )
      
      if (show_labels) {
        p <- p %>% add_trace(
          x = x_display,
          y = y_display,
          mode = "text",
          text = bin$bin_id,
          textposition = "top center",
          textfont = list(size = 8, color = "black"),
          showlegend = FALSE,
          hoverinfo = "none"
        )
      }
    }
  }
  
  if (!is.null(highlight_component)) {
    highlight_bins <- inventory %>% 
      filter(component_id == highlight_component) %>%
      pull(bin_id)
    
    if (length(highlight_bins) > 0) {
      highlight_data <- bins %>% 
        filter(bin_id %in% highlight_bins) %>%
        distinct(bin_id, tier, x_position, y_position)
      
      for (i in 1:nrow(highlight_data)) {
        bin <- highlight_data[i, ]
        
        x_display <- case_when(
          bin$tier == "Bottom" ~ bin$x_position - 1,
          bin$tier == "Middle" ~ bin$x_position,
          bin$tier == "Top" ~ bin$x_position + 1,
          TRUE ~ bin$x_position
        )
        
        y_display <- case_when(
          bin$tier == "Bottom" ~ bin$y_position - 1,
          bin$tier == "Middle" ~ bin$y_position,
          bin$tier == "Top" ~ bin$y_position + 1,
          TRUE ~ bin$y_position
        )
        
        p <- p %>% add_trace(
          x = x_display,
          y = y_display,
          mode = "markers",
          marker = list(
            size = 15,
            color = 'red',
            symbol = 'circle-open',
            line = list(width = 3)
          ),
          text = paste0(
            "Bin: ", bin$bin_id, "<br>",
            "Tier: ", bin$tier, "<br>",
            "Contains: ", highlight_component
          ),
          hoverinfo = "text",
          name = paste("Location of", highlight_component),
          showlegend = TRUE
        )
      }
    }
  }
  
  if (color_by == "component_count") {
    p <- p %>% add_trace(
      x = rep(warehouse_width * 0.05, 5),
      y = seq(warehouse_length * 0.1, warehouse_length * 0.3, length.out = 5),
      mode = "markers",
      marker = list(
        size = 10,
        color = viridisLite::viridis(5)
      ),
      text = paste("Components:", 0:4),
      hoverinfo = "text",
      name = "Component Count",
      showlegend = FALSE
    )
    
    for (i in 1:5) {
      p <- p %>% add_trace(
        x = warehouse_width * 0.07,
        y = warehouse_length * 0.1 + (i-1) * (warehouse_length * 0.2 / 4),
        mode = "text",
        text = paste(i-1, "components"),
        textposition = "middle right",
        textfont = list(size = 10),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    }
  } else if (color_by == "utilization") {
    legend_colors <- c("#0000FF", "#4040FF", "#8080FF", "#C0C0FF", "#FFFFFF")
    legend_values <- c("100%", "75%", "50%", "25%", "0%")
    
    p <- p %>% add_trace(
      x = rep(warehouse_width * 0.05, 5),
      y = seq(warehouse_length * 0.1, warehouse_length * 0.3, length.out = 5),
      mode = "markers",
      marker = list(
        size = 10,
        color = legend_colors
      ),
      text = paste("Utilization:", legend_values),
      hoverinfo = "text",
      name = "Utilization %",
      showlegend = FALSE
    )
    
    for (i in 1:5) {
      p <- p %>% add_trace(
        x = warehouse_width * 0.07,
        y = warehouse_length * 0.1 + (i-1) * (warehouse_length * 0.2 / 4),
        mode = "text",
        text = legend_values[i],
        textposition = "middle right",
        textfont = list(size = 10),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    }
  }
  
  p <- p %>% layout(
    title = paste0("U-Shaped Warehouse Layout - ", 
                   ifelse(show_tier == "All", "All Tiers", paste0(show_tier, " Tier"))),
    xaxis = list(title = "Width (feet)", range = c(-5, warehouse_width + 5)),
    yaxis = list(title = "Length (feet)", range = c(-5, warehouse_length + 5), scaleanchor = "x"),
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center")
  )
  
  return(p)
}

# Function to create detailed shelf view showing all tiers and components
create_shelf_detail_view <- function(shelf_unit_id) {
  shelf_unit <- shelving_units %>% filter(unit_id == shelf_unit_id)
  
  if (nrow(shelf_unit) == 0) {
    return(NULL)
  }
  
  unit_bins <- bins %>% filter(unit_id == shelf_unit_id)
  
  unit_inventory <- inventory %>%
    filter(bin_id %in% unit_bins$bin_id) %>%
    left_join(components, by = "component_id")
  
  tier_data <- tibble(
    tier = c("Top", "Middle", "Bottom"),
    y_position = c(3, 2, 1)
  )
  
  shelf_height <- 1
  shelf_width <- unique(shelf_unit$width)[1]
  shelf_length <- unique(shelf_unit$length)[1]
  
  is_wall_shelf <- unique(shelf_unit$zone)[1] == "Wall"
  width_display <- ifelse(is_wall_shelf, shelf_width, shelf_length)
  
  p <- plot_ly()
  
  for (i in 1:nrow(tier_data)) {
    tier <- tier_data[i, ]
    
    p <- p %>% add_trace(
      x = c(0, width_display, width_display, 0, 0),
      y = c(tier$y_position - 0.4, tier$y_position - 0.4, 
            tier$y_position + 0.4, tier$y_position + 0.4, 
            tier$y_position - 0.4),
      type = "scatter",
      mode = "lines",
      line = list(color = "black", width = 1),
      fill = "toself",
      fillcolor = "rgba(200, 200, 200, 0.3)",
      name = paste(tier$tier, "Tier"),
      showlegend = FALSE
    )
    
    p <- p %>% add_trace(
      x = -0.5,
      y = tier$y_position,
      type = "scatter",
      mode = "text",
      text = paste(tier$tier, "Tier"),
      textposition = "middle right",
      textfont = list(size = 12, color = "black"),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  }
  
  for (t in c("Bottom", "Middle", "Top")) {
    tier_sections <- unit_bins %>% 
      filter(tier == t) %>%
      arrange(section)
    
    for (i in 1:nrow(tier_sections)) {
      section <- tier_sections[i, ]
      
      y_pos <- tier_data %>% 
        filter(tier == t) %>% 
        pull(y_position)
      
      section_width <- section$section_width
      section_start <- case_when(
        t == "Bottom" ~ 0,
        t == "Middle" ~ (section$section - 1) * (width_display / 4),
        t == "Top" ~ (section$section - 1) * (width_display / 2),
        TRUE ~ 0
      )
      section_end <- section_start + section_width
      
      section_inventory <- unit_inventory %>%
        filter(bin_id == section$bin_id)
      
      if (nrow(section_inventory) > 0) {
        category <- section_inventory$category[1]
        categories <- unique(components$category)
        color_index <- match(category, categories)
        fill_color <- if (!is.na(color_index)) {
          viridisLite::viridis(length(categories))[color_index]
        } else {
          "lightgrey"
        }
        
        component_list <- paste(
          sapply(1:nrow(section_inventory), function(j) {
            paste0(j, ". ", section_inventory$component_id[j], 
                   " (", formatC(section_inventory$quantity[j], format="d", big.mark=","), " units)")
          }),
          collapse = "<br>"
        )
        
        hover_text <- paste0(
          "Bin: ", section$bin_id, "<br>",
          "Tier: ", section$tier, "<br>",
          "Section: ", section$section, "<br>",
          "Components:<br>", component_list
        )
      } else {
        fill_color <- "lightgrey"
        hover_text <- paste0(
          "Bin: ", section$bin_id, "<br>",
          "Status: Empty"
        )
      }
      
      p <- p %>% add_trace(
        x = c(section_start, section_end, section_end, section_start, section_start),
        y = c(y_pos - 0.3, y_pos - 0.3, y_pos + 0.3, y_pos + 0.3, y_pos - 0.3),
        type = "scatter",
        mode = "lines",
        line = list(color = "black", width = 1),
        fill = "toself",
        fillcolor = fill_color,
        text = hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )
      
      p <- p %>% add_trace(
        x = (section_start + section_end) / 2,
        y = y_pos,
        type = "scatter",
        mode = "text",
        text = section$bin_id,
        textposition = "middle center",
        textfont = list(size = 9, color = "black"),
        showlegend = FALSE,
        hoverinfo = "none"
      )
    }
  }
  
  p <- p %>% layout(
    title = paste("Detailed View of Shelf Unit:", shelf_unit_id),
    xaxis = list(
      title = "Width (ft)",
      range = c(-1, width_display + 1)
    ),
    yaxis = list(
      title = "",
      range = c(0.5, 3.5),
      showticklabels = FALSE
    ),
    showlegend = TRUE
  )
  
  return(p)
}

# Function to analyze components by tier
analyze_component_distribution <- function() {
  tier_distribution <- inventory %>%
    group_by(tier) %>%
    summarise(
      component_count = n_distinct(component_id),
      total_quantity = sum(quantity),
      .groups = "drop"
    )
  
  bin_analysis <- inventory %>%
    group_by(bin_id) %>%
    summarise(
      component_count = n_distinct(component_id),
      total_quantity = sum(quantity),
      .groups = "drop"
    ) %>%
    group_by(component_count) %>%
    summarise(
      bin_count = n(),
      .groups = "drop"
    )
  
  return(list(
    tier_distribution = tier_distribution,
    bin_analysis = bin_analysis
  ))
}

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WarehouseVision"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Warehouse Layout", tabName = "layout", icon = icon("map")),
      menuItem("Shelf Details", tabName = "shelf_details", icon = icon("th-large")),
      menuItem("Component Search", tabName = "search", icon = icon("search")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("lab_components_box", width = 4),
          valueBoxOutput("bom_components_box", width = 4),
          valueBoxOutput("car_components_box", width = 4)
        ),
        fluidRow(
          valueBoxOutput("total_inventory_box", width = 6),
          valueBoxOutput("high_velocity_box", width = 6)
        ),
        fluidRow(
          box(
            title = "Warehouse Overview",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("dashboard_layout", height = "500px")
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
            title = "Bins by Component Count",
            solidHeader = TRUE, 
            status = "info",
            width = 6,
            plotlyOutput("bin_component_distribution", height = "300px")
          )
        )
      ),
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
              choices = c(
                "Zone" = "zone",
                "Category" = "category",
                "Component Count" = "component_count",
                "Utilization" = "utilization",
                "Velocity" = "velocity"
              ),
              selected = "zone"
            ),
            selectInput(
              "show_tier",
              "Show Shelf Tier:",
              choices = c("All", "Bottom", "Middle", "Top"),
              selected = "All"
            ),
            checkboxInput("show_labels", "Show Bin Labels", value = FALSE),
            hr(),
            helpText("Click on any shelf unit in the layout to see its detailed view."),
            actionButton("reset_selection", "Clear Selection", icon = icon("undo"))
          ),
          box(
            title = "U-Shaped Warehouse Layout",
            solidHeader = TRUE,
            status = "primary",
            width = 9,
            plotlyOutput("warehouse_layout", height = "600px")
          )
        )
      ),
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
              choices = c("Select a unit" = "", setNames(shelving_units$unit_id, shelving_units$unit_id))
            ),
            helpText("View detailed multi-tier layout showing all components."),
            actionButton("view_shelf", "View Details", icon = icon("search"))
          ),
          box(
            title = "Shelf Contents Summary",
            solidHeader = TRUE,
            status = "info",
            width = 9,
            uiOutput("shelf_summary")
          )
        ),
        fluidRow(
          box(
            title = "Shelf Detail View",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("shelf_detail_view", height = "500px")
          )
        )
      ),
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            title = "Search Components",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            textInput("search_term", "Search Term:", placeholder = "Enter component ID (e.g. LAB119)"),
            selectInput(
              "search_type",
              "Component Type:",
              choices = c("All Types", "LAB", "BOM", "CAR", "ELC", "HYD", "FAS"),
              selected = "All Types"
            ),
            selectInput(
              "search_category",
              "Category:",
              choices = c("All Categories", unique(components$category)),
              selected = "All Categories"
            ),
            checkboxInput("show_all_locations", "Show All Locations", value = TRUE),
            actionButton("search_button", "Search", icon = icon("search")),
            br(), br(),
            actionButton("reset_search", "Reset", icon = icon("refresh"))
          ),
          box(
            title = "Search Results",
            solidHeader = TRUE,
            status = "primary",
            width = 9,
            DTOutput("search_results")
          )
        ),
        fluidRow(
          box(
            title = "Component Location",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("component_location", height = "500px")
          )
        )
      ),
      tabItem(
        tabName = "analytics",
        fluidRow(
          tabBox(
            title = "Component Analytics",
            width = 12,
            tabPanel(
              "Inventory by Type",
              plotlyOutput("inventory_by_type", height = "400px")
            ),
            tabPanel(
              "Movement Analysis",
              plotlyOutput("movement_analysis", height = "400px")
            ),
            tabPanel(
              "Multi-Component Analysis",
              plotlyOutput("multi_component_analysis", height = "400px")
            )
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  selected_component <- reactiveVal(NULL)
  selected_shelf_unit <- reactiveVal(NULL)
  
  observeEvent(input$reset_selection, {
    selected_component(NULL)
    selected_shelf_unit(NULL)
  })
  
  output$lab_components_box <- renderValueBox({
    lab_count <- sum(grepl("^LAB", components$component_id))
    valueBox(lab_count, "LAB Components", icon = icon("tag"), color = "blue")
  })
  
  output$bom_components_box <- renderValueBox({
    bom_count <- sum(grepl("^BOM", components$component_id))
    valueBox(bom_count, "BOM Components", icon = icon("list"), color = "green")
  })
  
  output$car_components_box <- renderValueBox({
    car_count <- sum(grepl("^CAR", components$component_id))
    valueBox(car_count, "CAR Components", icon = icon("truck"), color = "purple")
  })
  
  output$total_inventory_box <- renderValueBox({
    valueBox(
      formatC(sum(inventory$quantity), format="d", big.mark=","),
      "Total Items in Stock",
      icon = icon("warehouse"),
      color = "yellow"
    )
  })
  
  output$high_velocity_box <- renderValueBox({
    high_velocity_count <- sum(components$velocity_category == "High", na.rm = TRUE)
    valueBox(
      high_velocity_count,
      "High Velocity Components",
      icon = icon("bolt"),
      color = "red"
    )
  })
  
  output$dashboard_layout <- renderPlotly({
    create_warehouse_layout(color_by = "category", show_tier = "All")
  })
  
  output$tier_distribution <- renderPlotly({
    analysis <- analyze_component_distribution()
    
    plot_ly() %>%
      add_trace(
        data = analysis$tier_distribution,
        x = ~tier,
        y = ~component_count,
        type = "bar",
        name = "Component Count",
        marker = list(color = c("Bottom" = "#1f77b4", "Middle" = "#ff7f0e", "Top" = "#2ca02c"))
      ) %>%
      layout(
        title = "Component Distribution by Tier",
        xaxis = list(title = "Tier", categoryorder = "array", 
                     categoryarray = c("Bottom", "Middle", "Top")),
        yaxis = list(title = "Number of Components")
      )
  })
  
  output$bin_component_distribution <- renderPlotly({
    analysis <- analyze_component_distribution()
    
    plot_ly() %>%
      add_trace(
        data = analysis$bin_analysis,
        x = ~as.factor(component_count),
        y = ~bin_count,
        type = "bar",
        marker = list(color = viridisLite::viridis(max(analysis$bin_analysis$component_count)))
      ) %>%
      layout(
        title = "Number of Bins by Component Count",
        xaxis = list(title = "Components per Bin"),
        yaxis = list(title = "Number of Bins")
      )
  })
  
  output$warehouse_layout <- renderPlotly({
    p <- create_warehouse_layout(
      color_by = input$color_by,
      show_tier = input$show_tier,
      show_labels = input$show_labels,
      highlight_component = selected_component()
    )
    
    return(p)
  })
  
  observeEvent(event_data("plotly_click", source = "warehouse_layout"), {
    click_data <- event_data("plotly_click", source = "warehouse_layout")
    
    if (!is.null(click_data)) {
      x_pos <- click_data$x
      y_pos <- click_data$y
      
      for (i in 1:nrow(shelving_units)) {
        unit <- shelving_units[i, ]
        
        if (x_pos >= unit$x_start && 
            x_pos <= (unit$x_start + unit$width) &&
            y_pos >= unit$y_start && 
            y_pos <= (unit$y_start + unit$length)) {
          
          selected_shelf_unit(unit$unit_id)
          updateSelectInput(session, "shelf_unit_id", selected = unit$unit_id)
          updateTabItems(session, "sidebarMenu", "shelf_details")
          
          break
        }
      }
    }
  })
  
  observeEvent(input$view_shelf, {
    if (input$shelf_unit_id != "") {
      selected_shelf_unit(input$shelf_unit_id)
      updateTabItems(session, "sidebarMenu", "shelf_details")
    }
  })
  
  output$shelf_summary <- renderUI({
    req(selected_shelf_unit())
    
    shelf_inventory <- inventory %>%
      filter(unit_id == selected_shelf_unit()) %>%
      left_join(components, by = "component_id")
    
    if (nrow(shelf_inventory) == 0) {
      return(p("No inventory data available for this shelf unit."))
    }
    
    total_components <- n_distinct(shelf_inventory$component_id)
    total_quantity <- sum(shelf_inventory$quantity)
    
    tier_counts <- shelf_inventory %>%
      group_by(tier) %>%
      summarise(
        components = n_distinct(component_id),
        quantity = sum(quantity),
        .groups = "drop"
      )
    
    div(
      h4(paste0("Shelf Unit: ", selected_shelf_unit())),
      p(paste0("Total Components: ", total_components)),
      p(paste0("Total Quantity: ", formatC(total_quantity, format="d", big.mark=","))),
      
      h4("Components by Tier:"),
      tags$table(
        class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th("Tier"),
            tags$th("Component Types"),
            tags$th("Total Quantity")
          )
        ),
        tags$tbody(
          lapply(1:nrow(tier_counts), function(i) {
            tags$tr(
              tags$td(tier_counts$tier[i]),
              tags$td(tier_counts$components[i]),
              tags$td(formatC(tier_counts$quantity[i], format="d", big.mark=","))
            )
          })
        )
      )
    )
  })
  
  output$shelf_detail_view <- renderPlotly({
    req(selected_shelf_unit())
    create_shelf_detail_view(selected_shelf_unit())
  })
  
  search_results <- reactive({
    req(input$search_button)
    
    filtered <- components
    
    if(nchar(input$search_term) > 0) {
      filtered <- filtered %>%
        filter(grepl(input$search_term, component_id, ignore.case = TRUE) |
                 grepl(input$search_term, name, ignore.case = TRUE))
    }
    
    if(input$search_type != "All Types") {
      filtered <- filtered %>%
        filter(type == input$search_type)
    }
    
    if(input$search_category != "All Categories") {
      filtered <- filtered %>%
        filter(category == input$search_category)
    }
    
    if (input$show_all_locations) {
      result <- filtered %>%
        left_join(inventory, by = "component_id") %>%
        filter(!is.na(bin_id)) %>%
        select(component_id, name, category, bin_id, tier, unit_id, quantity) %>%
        mutate(location = paste0(unit_id, " (", tier, ")"))
    } else {
      result <- filtered %>%
        left_join(
          inventory %>% 
            group_by(component_id) %>% 
            summarise(
              total_quantity = sum(quantity),
              locations = n_distinct(bin_id),
              .groups = "drop"
            ),
          by = "component_id"
        ) %>%
        select(component_id, name, category, total_quantity, locations, velocity_category)
    }
    
    return(result)
  })
  
  output$search_results <- renderDT({
    datatable(
      search_results(),
      selection = "single",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  observeEvent(input$search_results_rows_selected, {
    selected_row <- input$search_results_rows_selected
    if(length(selected_row) > 0) {
      component_id <- search_results()[selected_row, "component_id"]
      selected_component(component_id)
    }
  })
  
  observeEvent(input$reset_search, {
    updateTextInput(session, "search_term", value = "")
    updateSelectInput(session, "search_type", selected = "All Types")
    updateSelectInput(session, "search_category", selected = "All Categories")
    updateCheckboxInput(session, "show_all_locations", value = TRUE)
  })
  
  output$component_location <- renderPlotly({
    req(selected_component())
    
    create_warehouse_layout(
      color_by = "zone",
      show_tier = "All",
      show_labels = TRUE,
      highlight_component = selected_component()
    )
  })
  
  output$inventory_by_type <- renderPlotly({
    inventory_summary <- inventory %>%
      left_join(components, by = "component_id") %>%
      filter(!is.na(type)) %>%
      group_by(type) %>%
      summarise(
        total_quantity = sum(quantity),
        .groups = "drop"
      )
    
    plot_ly() %>%
      add_trace(
        data = inventory_summary,
        x = ~type,
        y = ~total_quantity,
        type = "bar",
        marker = list(color = viridisLite::viridis(nrow(inventory_summary)))
      ) %>%
      layout(
        title = "Total Inventory by Component Type",
        xaxis = list(title = "Component Type"),
        yaxis = list(title = "Total Quantity")
      )
  })
  
  output$movement_analysis <- renderPlotly({
    movement_summary <- movement_log %>%
      left_join(components, by = "component_id") %>%
      filter(!is.na(type)) %>%
      group_by(type, movement_type) %>%
      summarise(
        count = n(),
        total_quantity = sum(quantity_moved),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = movement_type,
        values_from = total_quantity,
        values_fill = list(total_quantity = 0)
      )
    
    plot_ly() %>%
      add_trace(
        data = movement_summary,
        x = ~type,
        y = ~Inbound,
        type = "bar",
        name = "Inbound"
      ) %>%
      add_trace(
        data = movement_summary,
        x = ~type,
        y = ~Outbound,
        type = "bar",
        name = "Outbound"
      ) %>%
      add_trace(
        data = movement_summary,
        x = ~type,
        y = ~`Internal Transfer`,
        type = "bar",
        name = "Internal Transfer"
      ) %>%
      layout(
        title = "Movement by Component Type",
        xaxis = list(title = "Component Type"),
        yaxis = list(title = "Total Quantity Moved"),
        barmode = "group"
      )
  })
  
  output$multi_component_analysis <- renderPlotly({
    multi_component_data <- inventory %>%
      group_by(bin_id, tier) %>%
      summarise(
        component_count = n_distinct(component_id),
        .groups = "drop"
      ) %>%
      group_by(tier, component_count) %>%
      summarise(
        bin_count = n(),
        .groups = "drop"
      )
    
    plot_ly() %>%
      add_trace(
        data = multi_component_data,
        x = ~as.factor(component_count),
        y = ~bin_count,
        color = ~tier,
        type = "bar",
        colors = c("Bottom" = "#1f77b4", "Middle" = "#ff7f0e", "Top" = "#2ca02c")
      ) %>%
      layout(
        title = "Multi-Component Storage Analysis",
        xaxis = list(title = "Number of Components per Bin"),
        yaxis = list(title = "Number of Bins"),
        barmode = "group"
      )
  })
}

# Run the application
shinyApp(ui, server)