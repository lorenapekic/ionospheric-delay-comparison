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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

RUN_BIAS_CORRECTION <- TRUE
RUN_CIRCULAR_TIME <- TRUE

analysis_suffix <- "_only_valid_flat_bias"
#analysis_suffix <- "_included_invalid_flat_bias"
#analysis_suffix <- "_only_valid_dyn_bias"
#analysis_suffix <- "_included_invalid_dyn_bias"

if (grepl("only_valid", analysis_suffix)) {
  valid_print <- ""
} else if (grepl("included_invalid", analysis_suffix)) {
  valid_print <- "(Incl. Invalid)"
}

if (grepl("flat_bias", analysis_suffix)) {
  bias_method_print <- "(Flat Bias)"
} else if (grepl("dyn_bias", analysis_suffix)) {
  bias_method_print <- "(Dynamic Bias)"
}


if (RUN_BIAS_CORRECTION){
  fitted_correction_print <- "(Fitted Correction)"
  bias_correction_suffix <- "_fitted"
} else {
  fitted_correction_print <- ""
  bias_correction_suffix <- "_unfitted"
}

if (RUN_CIRCULAR_TIME){
  circular_time_print <- ""
  circular_time_suffix <- "_circular"
}else{
  circular_time_print <- "(Non-Circular Time)"
  circular_time_suffix <- "_noncircular"
}


load_gps_tec_data <- function(gps_file_path) {
  gps_tec_raw <- read.table(gps_file_path, skip = 2, header = TRUE)
  gps_tec <- gps_tec_raw %>%
    mutate(datetime = as.POSIXct((MJdatet) * 86400, origin = "1858-11-17", tz = "UTC")) %>%
    rename(Vtec_TECGPS = Vtec) %>%
    select(PRN, datetime, Vtec_TECGPS)
  return(gps_tec)
}

load_tec_suite_data <- function(tec_suite_file_path, analysis_suffix, base_date) {
  full_path <- paste0(tec_suite_file_path, "tec_suite_vtec_data", analysis_suffix, ".csv")
  tec_suite_raw <- read_csv(full_path)
  tec_suite_processed <- tec_suite_raw %>%
    mutate(
      datetime = as.POSIXct(base_date, tz = "UTC") + dhours(hour),
      PRN = as.numeric(str_remove(satellite, "G")),
      Vtec_tecsuite = vtec
    ) %>%
    select(PRN, datetime, Vtec_tecsuite)
  
  return(tec_suite_processed)
}

gps_data_path <- "data/gps-tec-output.Cmn"
tec_suite_path_prefix <- "data/tec_suite/"

gps_tec_data <- load_gps_tec_data(gps_file_path = gps_data_path)
tec_suite_data <- load_tec_suite_data(
  tec_suite_file_path = tec_suite_path_prefix,
  analysis_suffix = analysis_suffix,
  base_date = as.Date(gps_tec_data$datetime[1])
)



common_prns <- intersect(unique(tec_suite_data$PRN), unique(gps_tec_data$PRN))
combined_vtec_list <- list()

for (current_prn in common_prns) {
  
  tec_suite_subset <- tec_suite_data %>% filter(PRN == current_prn)
  gps_tec_subset <- gps_tec_data %>% filter(PRN == current_prn)
  
  joined_subset <- difference_inner_join(
    tec_suite_subset, 
    gps_tec_subset,
    by = "datetime",
    max_dist = 2,
    distance_col = "time_diff"
  )
  
  if (nrow(joined_subset) > 0) {
    combined_vtec_list[[as.character(current_prn)]] <- joined_subset
  }
}

combined_vtec <- bind_rows(combined_vtec_list) %>%
  select(PRN = PRN.x, datetime = datetime.x, Vtec_tecsuite, Vtec_TECGPS)


if (RUN_CIRCULAR_TIME){
  combined_vtec <- combined_vtec %>%
    mutate(datetime = update(datetime, hours = (hour(datetime) - 9) %% 24))
} else {
  print("ASDAS")
  combined_vtec <- combined_vtec %>%
    mutate(datetime = with_tz(force_tz(datetime, "UTC"), "America/Anchorage"))
  
}


if (RUN_BIAS_CORRECTION) {
  time_gap_threshold_minutes <- 30
  combined_vtec <- combined_vtec %>%
    arrange(PRN, datetime) %>%
    group_by(PRN) %>%
    mutate(
      time_diff_internal = difftime(datetime, lag(datetime), units = "mins"),
      segment_id = cumsum(is.na(lag(datetime)) | time_diff_internal > time_gap_threshold_minutes)
    ) %>%
    ungroup()
  
  combined_vtec <- combined_vtec %>%
    group_by(PRN, segment_id) %>%
    mutate(
      bias = mean(Vtec_TECGPS, na.rm = TRUE) - mean(Vtec_tecsuite, na.rm = TRUE),
      Vtec_tecsuite = Vtec_tecsuite + bias
    ) %>%
    ungroup() %>%
    select(-bias, -time_diff_internal)
}




output_dir <- paste0("comparison_vtec_absolutes/", circular_time_suffix, analysis_suffix, bias_correction_suffix)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


for (prn_to_plot in unique(combined_vtec$PRN)) {
  satellite_data <- combined_vtec %>% 
    filter(PRN == prn_to_plot) %>%
    pivot_longer(cols = c(Vtec_tecsuite, Vtec_TECGPS),
                 names_to = "Source",
                 values_to = "VTEC")
  
  min_time <- min(satellite_data$datetime, na.rm = TRUE)
  max_time <- max(satellite_data$datetime, na.rm = TRUE)
  x_axis_start <- floor_date(min_time, unit = "day")
  x_axis_end <- ceiling_date(max_time, unit = "day")
  
  vtec_plot <- ggplot(satellite_data, aes(x = datetime, y = VTEC, color = Source)) +
    geom_point(size = 1.5, alpha = 0.6) +
    coord_cartesian(xlim = c(x_axis_start, x_axis_end)) +
    labs(
      title = paste("VTEC Comparison for Satellite PRN", prn_to_plot, bias_method_print, valid_print, circular_time_print, fitted_correction_print),
      x = "Datetime (America/Anchorage)",
      y = "VTEC (TECU)",
      color = "VTEC Source"
    ) +
    scale_color_manual(values = c("Vtec_tecsuite" = "blue", "Vtec_TECGPS" = "red")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  plot_filename <- file.path(output_dir, paste0("VTEC_comparison_PRN_", prn_to_plot,  circular_time_suffix, analysis_suffix, bias_correction_suffix, ".png"))
  ggsave(plot_filename, plot = vtec_plot, width = 12, height = 6, dpi = 300, bg = "white")
  
}








average_vtec <- combined_vtec %>%
  group_by(datetime) %>%
  summarise(
    avg_Vtec_tecsuite = mean(Vtec_tecsuite, na.rm = TRUE),
    avg_Vtec_TECGPS = mean(Vtec_TECGPS, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(avg_Vtec_tecsuite, avg_Vtec_TECGPS),
    names_to = "Source",
    values_to = "Average_VTEC"
  )




min_time <- min(average_vtec$datetime, na.rm = TRUE)
max_time <- max(average_vtec$datetime, na.rm = TRUE)
x_axis_start <- floor_date(min_time, unit = "day")
x_axis_end <- ceiling_date(max_time, unit = "day")

avg_vtec_plot <- ggplot(average_vtec, aes(x = datetime, y = Average_VTEC, color = Source)) +
  geom_point(size = 1.5, alpha = 0.6) +
  coord_cartesian(xlim = c(x_axis_start, x_axis_end)) +
  labs(
    title = paste("Average VTEC Comparison", bias_method_print, valid_print, circular_time_print, fitted_correction_print),
    x = "Datetime (America/Anchorage)",
    y = "Average VTEC (TECU)",
    color = "Average VTEC Source"
  ) +
  scale_color_manual(
    values = c("avg_Vtec_tecsuite" = "blue", "avg_Vtec_TECGPS" = "red"),
    labels = c("avg_Vtec_tecsuite" = "Vtec tec_suite", "avg_Vtec_TECGPS" = "Vtec Gopi")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

avg_plot_filename <- file.path(output_dir, paste0("VTEC_avg_comparison", circular_time_suffix, analysis_suffix, bias_correction_suffix,  ".png"))
ggsave(avg_plot_filename, plot = avg_vtec_plot, width = 12, height = 6, dpi = 300, bg = "white")


print("--- Script finished successfully! All plots have been saved. ---")