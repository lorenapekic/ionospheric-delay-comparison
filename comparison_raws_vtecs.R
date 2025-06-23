  # Clear the environment
  rm(list=ls())
  
  # Load necessary libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(fuzzyjoin)
  library(stringr)
  library(readr)
  
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  #analysis_suffix <- "_only_valid_flat_bias"
  #analysis_suffix <- "_included_invalid_flat_bias"
  analysis_suffix <- "_only_valid_dyn_bias"
  #analysis_suffix <- "_included_invalid_dyn_bias"
  
  # Load the datasets
  gps_tec_raw <- read.table("data/gps-tec-output.Cmn", skip = 2, header = TRUE)
  tec_suite_raw <- read_csv(paste0("data/tec_suite/tec_suite_vtec_data", analysis_suffix, ".csv"))
  
  
  
  # Pre-process the first dataset (gps-tec)
  gps_tec_raw$datetime <- as.POSIXct((gps_tec_raw$MJdatet) * 86400, origin = "1858-11-17", tz = "UTC")
  
  # Pre-process the second dataset (tec_suite)
  base_date <- as.Date(gps_tec_raw$datetime[1])
  tec_suite_processed <- tec_suite_raw %>%
    mutate(
      datetime = as.POSIXct(base_date) + dhours(hour),
      PRN = as.numeric(str_remove(satellite, "G")),
      Vtec_tecsuite = vtec
    ) %>%
    select(PRN, datetime, Vtec_tecsuite)
  
  # Prepare the first dataset for merging
  gps_tec <- gps_tec_raw %>%
    rename(Vtec_TECGPS = Vtec) %>%
    select(PRN, datetime, Vtec_TECGPS)
  
  # --- MODIFICATION START: Merge the two VTEC data sources correctly ---
  
  # The original method incorrectly joined rows based only on time, mixing data
  # from different satellites. The new method ensures we only join data for the
  # SAME satellite.
  
  # 1. Get a list of satellites that appear in BOTH datasets
  common_prns <- intersect(unique(tec_suite_processed$PRN), unique(gps_tec$PRN))
  
  # 2. Initialize an empty list to store the joined data for each satellite
  combined_vtec_list <- list()
  
  # 3. Loop through each common satellite, join its data, and add to the list
  for (current_prn in common_prns) {
    
    # Filter data for the current satellite from both sources
    tec_suite_subset <- tec_suite_processed %>% filter(PRN == current_prn)
    gps_tec_subset <- gps_tec %>% filter(PRN == current_prn)
    
    # Perform the fuzzy join ONLY on data for this specific satellite
    joined_subset <- difference_inner_join(
      tec_suite_subset, 
      gps_tec_subset,
      by = "datetime",
      max_dist = 2, # 2-second tolerance
      distance_col = "time_diff"
    )
    
    # Add the result for this satellite to our list if any matches were found
    if (nrow(joined_subset) > 0) {
      combined_vtec_list[[as.character(current_prn)]] <- joined_subset
    }
  }
  
  # 4. Combine all the individual data frames into one final data frame
  combined_vtec <- bind_rows(combined_vtec_list) %>%
    # Rename columns for clarity. PRN.x and PRN.y will be identical
    # because we pre-filtered by satellite.
    select(PRN = PRN.x, datetime = datetime.x, Vtec_tecsuite, Vtec_TECGPS)
  
  # --- MODIFICATION END ---
  
  
  # Check if any data was combined before proceeding to plot
  if (nrow(combined_vtec) == 0) {
    stop("No matching data points found between the two datasets after filtering by satellite and time. Cannot generate plots.")
  }
  
  # --- Plotting and Saving ---
  
  # Create a directory to save the comparison plots.
  output_dir <- paste0("comparison_vtec_absolutes", analysis_suffix)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Get a list of unique satellites (PRNs) present in the combined data
  unique_prns <- unique(combined_vtec$PRN)
  
  # Loop through each satellite to create and save a plot
  for (prn_to_plot in unique_prns) {
    
    # Filter the data for the current satellite
    satellite_data <- combined_vtec %>% filter(PRN == prn_to_plot)
    
    # Reshape the data to a long format for easier plotting with ggplot2
    satellite_data_long <- satellite_data %>%
      pivot_longer(cols = c(Vtec_tecsuite, Vtec_TECGPS),
                   names_to = "Source",
                   values_to = "VTEC")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    time_gap_threshold_minutes <- 30
    
    satellite_data_segmented <- satellite_data_long %>%
      # We must group by Source so that we find gaps within the 'red' line
      # and 'blue' line independently.
      group_by(Source) %>%
      # Arrange by time to properly calculate time differences
      arrange(datetime) %>%
      mutate(
        # Calculate the time difference from the previous data point
        time_diff = difftime(datetime, lag(datetime), units = "mins"),
        
        # Create a new segment ID. The ID number increases every time a gap
        # larger than the threshold is found. The is.na() handles the very
        # first point in each group.
        segment_id = cumsum(time_diff > time_gap_threshold_minutes | is.na(time_diff))
      ) %>%
      ungroup()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Create the plot
    vtec_plot <- ggplot(satellite_data_segmented, aes(x = datetime, y = VTEC, color = Source, group= interaction(Source, segment_id))) +
      geom_point(size = 1.5, alpha = 0.6) +
      labs(
        title = paste("VTEC Comparison for Satellite PRN", prn_to_plot),
        x = "Datetime (UTC)",
        y = "VTEC (TECU)",
        color = "VTEC Source"
      ) +
      scale_color_manual(values = c("Vtec_tecsuite" = "blue", "Vtec_TECGPS" = "red")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Define the filename and save the plot.
    plot_filename <- file.path(output_dir, paste0("VTEC_comparison_PRN_", prn_to_plot, ".png"))
    ggsave(plot_filename, plot = vtec_plot, width = 12, height = 6, dpi = 300, bg = "white")
    
  }
  
  
  
  
  
  
  
  
  # --- START: AVERAGE VTEC COMPARISON ---
  
  # Calculate the average VTEC for each source across all satellites at each time point
  average_vtec <- combined_vtec %>%
    group_by(datetime) %>%
    summarise(
      avg_Vtec_tecsuite = mean(Vtec_tecsuite, na.rm = TRUE),
      avg_Vtec_TECGPS = mean(Vtec_TECGPS, na.rm = TRUE),
      .groups = 'drop' # Drop grouping after summarising
    )
  
  # Reshape the averaged data to a long format for plotting
  average_vtec_long <- average_vtec %>%
    pivot_longer(
      cols = c(avg_Vtec_tecsuite, avg_Vtec_TECGPS),
      names_to = "Source",
      values_to = "Average_VTEC"
    )
  
  # Create segments to handle gaps in the time series data for the average plot
  average_vtec_segmented <- average_vtec_long %>%
    group_by(Source) %>%
    arrange(datetime) %>%
    mutate(
      time_diff = difftime(datetime, lag(datetime), units = "mins"),
      segment_id = cumsum(time_diff > time_gap_threshold_minutes | is.na(time_diff))
    ) %>%
    ungroup()
  
  # Create the plot for the averaged VTEC data
  avg_vtec_plot <- ggplot(average_vtec_segmented, aes(x = datetime, y = Average_VTEC, color = Source, group = interaction(Source, segment_id))) +
    geom_point(size = 1.5, alpha = 0.6) +
    labs(
      title = "Average VTEC Comparison (All Satellites)",
      x = "Datetime (UTC)",
      y = "Average VTEC (TECU)",
      color = "Average VTEC Source"
    ) +
    scale_color_manual(
      values = c("avg_Vtec_tecsuite" = "blue", "avg_Vtec_TECGPS" = "red"),
      labels = c("avg_Vtec_tecsuite" = "Vtec_tecsuite (Avg)", "avg_Vtec_TECGPS" = "Vtec_TECGPS (Avg)")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Define the filename and save the average VTEC plot
  avg_plot_filename <- file.path(output_dir, paste0("VTEC_average_comparison", analysis_suffix, ".png"))
  ggsave(avg_plot_filename, plot = avg_vtec_plot, width = 12, height = 6, dpi = 300, bg = "white")
  
  # --- END: AVERAGE VTEC COMPARISON ---
  
  print("--- Script finished successfully! All plots have been saved. ---")