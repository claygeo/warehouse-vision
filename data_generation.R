# WarehouseVision: Enhanced Data Model for Multi-Component Shelves
# This script generates sample data for a U-shaped warehouse with middle island

library(tidyverse)
library(lubridate)

# Function to generate or load warehouse data
load_warehouse_data <- function(use_saved = FALSE, file_path = "data/warehouse_data.rds") {
  if (use_saved && file.exists(file_path)) {
    return(readRDS(file_path))
  } else {
    # Generate new data
    warehouse_data <- generate_warehouse_data()
    
    # Save data if directory exists
    if (!use_saved && dir.exists(dirname(file_path))) {
      saveRDS(warehouse_data, file_path)
    }
    
    return(warehouse_data)
  }
}

# Function to generate warehouse data with multi-component shelf sections
generate_warehouse_data <- function() {
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
  # Join with shelving unit properties
  left_join(shelving_units, by = "unit_id") %>%
  # Generate section positions within each tier
  mutate(
    section_width = case_when(
      zone == "Wall" & tier == "Bottom" ~ width / 1,  # Bottom tier has 1 section (full width)
      zone == "Wall" & tier == "Middle" ~ width / 4,  # Middle tier has 4 sections
      zone == "Wall" & tier == "Top" ~ width / 2,     # Top tier has 2 sections
      zone == "Island" & tier == "Bottom" ~ width / 1, # Similar pattern for island shelves
      zone == "Island" & tier == "Middle" ~ width / 4,
      zone == "Island" & tier == "Top" ~ width / 2,
      TRUE ~ width / 2
    ),
    # Only keep valid sections based on tier
    valid_section = case_when(
      tier == "Bottom" ~ section == 1,                # Bottom tier has 1 section
      tier == "Middle" ~ section <= 4,                # Middle tier has 4 sections
      tier == "Top" ~ section <= 2,                   # Top tier has 2 sections
      TRUE ~ FALSE
    ),
    # Calculate position of each section
    section_x_offset = case_when(
      tier == "Bottom" ~ 0,
      tier == "Middle" ~ (section - 1) * section_width,
      tier == "Top" ~ (section - 1) * section_width * 2,
      TRUE ~ 0
    ),
    # Create unique section ID
    section_id = paste(unit_id, tier, section, sep = "-")
  ) %>%
  filter(valid_section)  # Remove invalid sections
  
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
  # First, determine how many components go in each bin
  bin_component_count <- bins %>%
    mutate(
      component_count = case_when(
        tier == "Bottom" ~ 1,          # Bottom tier has 1 component
        tier == "Middle" ~ sample(1:4, n(), replace = TRUE),  # Middle tier has 1-4 components
        tier == "Top" ~ sample(1:2, n(), replace = TRUE)      # Top tier has 1-2 components
      )
    )
  
  # Generate inventory records
  inventory <- tibble()
  
  for (i in 1:nrow(bin_component_count)) {
    bin <- bin_component_count[i,]
    
    # Select random components for this bin
    n_components <- bin$component_count
    
    # Pick components that make sense for this zone/tier
    if (bin$zone == "Wall") {
      # Wall shelves tend to have heavier items on bottom, lighter on top
      if (bin$tier == "Bottom") {
        component_pool <- components %>% filter(type %in% c("CAR", "HYD", "FAS"))
      } else if (bin$tier == "Middle") {
        component_pool <- components %>% filter(type %in% c("BOM", "ELC", "FAS", "HYD"))
      } else { # Top
        component_pool <- components %>% filter(type %in% c("LAB", "BOM", "ELC"))
      }
    } else { # Island
      # Islands tend to have more accessible, frequently used items
      if (bin$tier == "Bottom") {
        component_pool <- components %>% filter(type %in% c("CAR", "FAS"))
      } else if (bin$tier == "Middle") {
        component_pool <- components %>% filter(type %in% c("BOM", "ELC", "LAB"))
      } else { # Top
        component_pool <- components %>% filter(type %in% c("LAB", "ELC"))
      }
    }
    
    # If no components match criteria, use all components
    if (nrow(component_pool) == 0) {
      component_pool <- components
    }
    
    selected_components <- sample_n(component_pool, 
                                    size = min(n_components, nrow(component_pool)), 
                                    replace = FALSE)
    
    # Generate inventory records for each component in this bin
    for (j in 1:nrow(selected_components)) {
      component <- selected_components[j,]
      
      # Calculate remaining capacity in bin
      remaining_capacity <- ifelse(j == 1, 
                                  bin$capacity, 
                                  bin$capacity / n_components)
      
      # Generate inventory record
      new_record <- tibble(
        inventory_id = paste0("INV-", sample(100000:999999, 1)),
        bin_id = bin$bin_id,
        component_id = component$component_id,
        unit_id = bin$unit_id,
        tier = bin$tier,
        section = bin$section,
        quantity = min(sample(100:5000, 1), remaining_capacity),
        capacity_percentage = NA_real_,  # Will calculate after
        last_updated = sample(seq(Sys.Date() - 90, Sys.Date(), by = "day"), 1),
        last_accessed = sample(seq(Sys.Date() - 60, Sys.Date(), by = "day"), 1)
      )
      
      inventory <- bind_rows(inventory, new_record)
    }
  }
  
  # Calculate capacity percentages
  inventory_with_bins <- inventory %>%
    left_join(bins, by = "bin_id")
  
  inventory <- inventory_with_bins %>%
    group_by(bin_id) %>%
    mutate(
      total_quantity = sum(quantity),
      capacity_percentage = round((quantity / capacity) * 100, 1)
    ) %>%
    ungroup() %>%
    select(names(inventory), capacity_percentage, total_quantity, capacity)
  
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
  
  # Join velocity data to components
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
    shelf_sections = shelf_sections,
    components = components,
    inventory = inventory,
    movement_log = movement_log
  )
  
  return(warehouse_data)
}

# Helper function to analyze component movement
analyze_component_movement <- function(warehouse_data) {
  # Component type summary
  component_type_summary <- warehouse_data$components %>%
    group_by(type) %>%
    summarise(
      count = n(),
      avg_movements = mean(num_movements, na.rm = TRUE),
      total_quantity = sum(total_quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Inventory summary
  inventory_summary <- warehouse_data$inventory %>%
    left_join(warehouse_data$components, by = "component_id") %>%
    group_by(type) %>%
    summarise(
      total_quantity = sum(quantity),
      avg_quantity = mean(quantity),
      min_quantity = min(quantity),
      max_quantity = max(quantity),
      .groups = "drop"
    )
  
  # Movement summary
  movement_summary <- warehouse_data$movement_log %>%
    left_join(warehouse_data$components, by = "component_id") %>%
    group_by(type, movement_type) %>%
    summarise(
      count = n(),
      total_quantity = sum(quantity_moved),
      .groups = "drop"
    )
  
  # Bin utilization
  bin_utilization <- warehouse_data$inventory %>%
    group_by(bin_id, tier) %>%
    summarise(
      component_count = n_distinct(component_id),
      total_quantity = sum(quantity),
      capacity = first(capacity),
      utilization_percentage = round((total_quantity / capacity) * 100, 1),
      .groups = "drop"
    )
  
  # Component co-occurrence
  component_co_occurrence <- warehouse_data$inventory %>%
    select(bin_id, component_id) %>%
    inner_join(warehouse_data$inventory %>% 
                select(bin_id, component_id2 = component_id), 
              by = "bin_id") %>%
    filter(component_id < component_id2) %>%  # Avoid duplicates
    group_by(component_id, component_id2) %>%
    summarise(
      co_occurrence_count = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(co_occurrence_count))
  
  return(list(
    component_summary = component_type_summary,
    inventory_summary = inventory_summary,
    movement_summary = movement_summary,
    bin_utilization = bin_utilization,
    component_co_occurrence = component_co_occurrence
  ))
}

# Function to identify optimization opportunities
identify_optimization_opportunities <- function(warehouse_data) {
  # Identify bins with inefficient use of space
  inefficient_bins <- warehouse_data$inventory %>%
    group_by(bin_id, tier) %>%
    summarise(
      component_count = n_distinct(component_id),
      total_quantity = sum(quantity),
      capacity = first(capacity),
      utilization_percentage = round((total_quantity / capacity) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(utilization_percentage < 30) %>%
    arrange(utilization_percentage)
  
  # Identify frequently accessed components in inefficient locations
  movement_counts <- warehouse_data$movement_log %>%
    group_by(component_id) %>%
    summarise(
      movement_count = n(),
      .groups = "drop"
    )
  
  inefficient_placement <- warehouse_data$inventory %>%
    left_join(warehouse_data$components, by = "component_id") %>%
    left_join(movement_counts, by = "component_id") %>%
    mutate(
      movement_count = replace_na(movement_count, 0),
      access_score = case_when(
        tier == "Middle" ~ movement_count * 1,
        tier == "Bottom" ~ movement_count * 1.5,
        tier == "Top" ~ movement_count * 2,
        TRUE ~ movement_count
      )
    ) %>%
    filter(access_score > 20) %>%
    arrange(desc(access_score)) %>%
    select(
      component_id, name, category, bin_id, tier, quantity,
      movement_count, access_score
    )
  
  # Identify components that are frequently accessed together but stored separately
  component_pairs <- warehouse_data$movement_log %>%
    arrange(user_id, movement_date) %>%
    group_by(user_id) %>%
    mutate(
      prev_component = lag(component_id),
      time_diff = as.numeric(difftime(movement_date, lag(movement_date), units = "mins"))
    ) %>%
    filter(!is.na(prev_component), time_diff < 30, component_id != prev_component) %>%
    ungroup() %>%
    select(component_pair = prev_component, component_id) %>%
    # Ensure consistent ordering of component pairs
    rowwise() %>%
    mutate(
      comp_a = min(component_id, component_pair),
      comp_b = max(component_id, component_pair)
    ) %>%
    ungroup() %>%
    select(comp_a, comp_b) %>%
    group_by(comp_a, comp_b) %>%
    summarise(
      co_access_count = n(),
      .groups = "drop"
    ) %>%
    filter(co_access_count > 2) %>%
    arrange(desc(co_access_count))
  
  # Get bin information for these component pairs
  pair_locations <- tibble()
  
  for (i in 1:min(nrow(component_pairs), 20)) {
    pair <- component_pairs[i,]
    
    comp_a_location <- warehouse_data$inventory %>%
      filter(component_id == pair$comp_a) %>%
      select(component_id, bin_id, tier, unit_id) %>%
      rename(bin_id_a = bin_id, tier_a = tier, unit_id_a = unit_id)
    
    comp_b_location <- warehouse_data$inventory %>%
      filter(component_id == pair$comp_b) %>%
      select(component_id, bin_id, tier, unit_id) %>%
      rename(bin_id_b = bin_id, tier_b = tier, unit_id_b = unit_id)
    
    if (nrow(comp_a_location) > 0 && nrow(comp_b_location) > 0) {
      location_info <- crossing(comp_a_location, comp_b_location) %>%
        mutate(
          same_unit = unit_id_a == unit_id_b,
          same_tier = tier_a == tier_b,
          co_access_count = pair$co_access_count
        )
      
      pair_locations <- bind_rows(pair_locations, location_info)
    }
  }
  
  # Filter for pairs that are not already co-located
  separate_pairs <- pair_locations %>%
    filter(!same_unit) %>%
    left_join(warehouse_data$components %>% 
                select(component_id, name_a = name), 
              by = c("component_id.x" = "component_id")) %>%
    left_join(warehouse_data$components %>% 
                select(component_id, name_b = name), 
              by = c("component_id.y" = "component_id")) %>%
    arrange(desc(co_access_count)) %>%
    select(
      component_a = component_id.x, name_a, bin_id_a, tier_a,
      component_b = component_id.y, name_b, bin_id_b, tier_b,
      co_access_count
    )
  
  return(list(
    inefficient_bins = inefficient_bins,
    inefficient_placement = inefficient_placement,
    component_pairs = separate_pairs
  ))
}