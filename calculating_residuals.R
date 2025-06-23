# Clear the environment
rm(list=ls())

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(lubridate)
library(fuzzyjoin)
library(purrr)
library(stringr)
library(readr) # Added readr for read_csv

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

RUN_BIAS_CORRECTION <- TRUE

title_add <- "(Flat bias + fitted per satellite)"

analysis_suffix <- "_only_valid_flat_bias"
#analysis_suffix <- "_included_invalid_flat_bias"
#analysis_suffix <- "_only_valid_dyn_bias"
#analysis_suffix <- "_included_invalid_dyn_bias"


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

common_prns <- intersect(unique(gps_tec_data$PRN), unique(tec_suite_data$PRN))





common_prns <- intersect(unique(gps_tec_data$PRN), unique(tec_suite_data$PRN))

all_residuals <- map_dfr(common_prns, function(current_prn) {
  
  gps_subset <- gps_tec_data %>% filter(PRN == current_prn)
  tec_suite_subset <- tec_suite_data %>% filter(PRN == current_prn)
  
  joined_data <- difference_inner_join(
    gps_subset,
    tec_suite_subset,
    by = "datetime",
    max_dist = 2, # 2-second tolerance
    distance_col = "time_diff"
  ) %>%
    # Select and rename columns early
    select(
      PRN = PRN.x,
      datetime = datetime.x, 
      Vtec_tecsuite, 
      Vtec_TECGPS
    )

  # <<<< START OF INSERTED LOGIC >>>>
  if (RUN_BIAS_CORRECTION && nrow(joined_data) > 0) {
    
    time_gap_threshold_minutes <- 30
    
    joined_data <- joined_data %>%
      # We only need to arrange by datetime now
      arrange(datetime) %>%
      mutate(
        time_diff_internal = difftime(datetime, lag(datetime), units = "mins"),
        segment_id = cumsum(is.na(lag(datetime)) | time_diff_internal > time_gap_threshold_minutes)
      ) %>%
      # We no longer group by PRN, only by the new segment_id
      group_by(segment_id) %>%
      mutate(
        bias = mean(Vtec_TECGPS, na.rm = TRUE) - mean(Vtec_tecsuite, na.rm = TRUE),
        Vtec_tecsuite = Vtec_tecsuite + bias
      ) %>%
      ungroup() %>%
      select(-bias, -time_diff_internal, -segment_id)
  }
  # <<<< END OF INSERTED LOGIC >>>>
  
  # Calculate the final residual and return
  joined_data %>%
    mutate(
      r_t = Vtec_tecsuite - Vtec_TECGPS
    )
})




stats_per_prn <- all_residuals %>%
  group_by(PRN) %>%
  summarise(n = n(),
            min = min(r_t, na.rm = TRUE),
            Q1 = quantile(r_t, 0.25, na.rm = TRUE),
            median = median(r_t, na.rm = TRUE),
            Q3 = quantile(r_t, 0.75, na.rm = TRUE),
            max = max(r_t, na.rm = TRUE),
            mean = mean(r_t, na.rm = TRUE),
            variance = var(r_t, na.rm = TRUE),
            .groups = 'drop')

print("--- Descriptive Statistics per PRN ---")
print(stats_per_prn)

# Normality test (Shapiro-Wilk) per PRN
normality_tests <- all_residuals %>%
  group_by(PRN) %>%
  # Use a sample if N > 5000, as shapiro.test has a limit
  summarise(p_value = {
    data_sample <- if(n() > 4999) sample(r_t, 4999) else r_t
    tryCatch(shapiro.test(data_sample)$p.value, error = function(e) NA)
  },
  .groups = 'drop')

print("--- Shapiro-Wilk Normality Test P-Values per PRN ---")
print(normality_tests)

# Global Statistics (all PRNs combined)
global_stats <- all_residuals %>%
  summarise(n = n(),
            min = min(r_t, na.rm = TRUE),
            Q1 = quantile(r_t, 0.25, na.rm = TRUE),
            median = median(r_t, na.rm = TRUE),
            Q3 = quantile(r_t, 0.75, na.rm = TRUE),
            max = max(r_t, na.rm = TRUE),
            mean = mean(r_t, na.rm = TRUE),
            variance = var(r_t, na.rm = TRUE))

print("--- Global Statistics Across All PRNs ---")
print(global_stats)

# Global Normality Test (Anderson-Darling is better for large samples)
print("--- Global Anderson-Darling Normality Test ---")
global_normality_ad <- nortest::ad.test(all_residuals$r_t)
print(global_normality_ad)

# Visualization for Each PRN
# Directory to save plots
output_dir <- paste0("plots_residuals/", analysis_suffix)
dir.create(output_dir, recursive=TRUE)

# Generate and save a plot for each PRN
unique_prns <- unique(all_residuals$PRN)


# Global Density Plot
global_hist_plot <- ggplot(all_residuals, aes(x = r_t)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", alpha = 0.5) +
  geom_density(color = "darkred", size = 1) +
  labs(title = paste("Global Residuals Density Across All PRNs", title_add), x = "Residual (r_t)", y = "Density") +
  theme_minimal()

ggsave(file.path(output_dir, "histogram_residuals_GLOBAL.png"), plot = global_hist_plot, width = 10, height = 6, dpi = 300, bg = "white")


# Global Time Series Plot
global_ts_plot <- ggplot(all_residuals, aes(x = datetime, y = r_t, color = factor(PRN))) +
  geom_point(alpha = 0.4, size = 1) +
  stat_summary(
    fun = "mean",          # The function to apply (calculate the mean)
    geom = "line",         # The geometric object to draw (a line)
    color = "black",       # The color of the line
    linewidth = 0.8,       # The thickness of the line (use `size` for older ggplot2 versions)
    aes(group = 1)         # Important: Ensure it draws one single line across all PRNs
  ) +
  labs(title = paste("Residuals Over Time for All PRNs", title_add),
       x = "Datetime", y = "Residual (r_t)",
       color = "PRN") +
  theme_minimal()

ggsave(file.path(output_dir, "timeseries_residuals_GLOBAL.png"), plot = global_ts_plot, width = 12, height = 7, dpi = 300, bg = "white")


# Save master residuals table 
write.csv(all_residuals, "all_residuals.csv", row.names = FALSE)

print(paste("--- Script finished successfully! All plots saved in '", output_dir, "' directory. ---"))