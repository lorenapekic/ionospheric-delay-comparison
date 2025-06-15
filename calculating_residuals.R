rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(lubridate)
library(fuzzyjoin)
library(purrr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Read GPS TEC data
gps_tec <- read.table("data/gps-tec-output.Cmn", skip = 2, header = TRUE)
gps_tec$datetime <- as.POSIXct((gps_tec$MJdatet) * 86400, origin = "1858-11-17", tz = "UTC")

# Directory for TEC suite files
dat_files <- list.files(path = "./data/tecsuite", pattern = "\\.dat$", full.names = TRUE)
start_time <- as.POSIXct("2025-06-09 00:00:00", tz = "UTC")

# Initialize master residuals table
all_residuals <- tibble(PRN = numeric(), datetime = as.POSIXct(character()), 
                        Vtec_tecsuite = numeric(), Vtec_TECGPS = numeric(), r_t = numeric())

# Loop through PRNs 1 to 25
for (prn in 1:25) {
  prn_padded <- str_pad(prn, 2, pad = "0")
  file_pattern <- paste0("ac12_G", prn_padded, "_160_25.dat")
  dat_file <- dat_files[basename(dat_files) == file_pattern]
  
  if (length(dat_file) == 0) next
  
  gps_tec_prn <- gps_tec %>%
    filter(PRN == prn) %>%
    rename(Vtec_TECGPS = Vtec)
  
  tec_suite <- read.table(dat_file, skip = 10, header = FALSE,
                          col.names = c("tsn", "hour", "el", "az", "tec.l1l2", "tec.p1p2", "validity")) %>%
    mutate(datetime = start_time + (hour * 3600),
           Vtec_tecsuite = tec.l1l2)
  
  residuals <- difference_inner_join(
    gps_tec_prn, tec_suite,
    by = c("datetime" = "datetime"),
    max_dist = 2, distance_col = "time_diff"
  ) %>%
    mutate(PRN = prn, r_t = Vtec_tecsuite - Vtec_TECGPS) %>%
    select(PRN, datetime = datetime.x, Vtec_tecsuite, Vtec_TECGPS, r_t)
  
  all_residuals <- bind_rows(all_residuals, residuals)
}

# descriptive Statistics per PRN
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

print(stats_per_prn)

# normality test (Shapiro-Wilk) per PRN
normality_tests <- all_residuals %>%
  group_by(PRN) %>%
  summarise(p_value = tryCatch(shapiro.test(r_t)$p.value, error = function(e) NA),
            .groups = 'drop')

print(normality_tests)

# global Statistics (all PRNs combined)
global_stats <- all_residuals %>%
  summarise(n = n(),
            min = min(r_t, na.rm = TRUE),
            Q1 = quantile(r_t, 0.25, na.rm = TRUE),
            median = median(r_t, na.rm = TRUE),
            Q3 = quantile(r_t, 0.75, na.rm = TRUE),
            max = max(r_t, na.rm = TRUE),
            mean = mean(r_t, na.rm = TRUE),
            variance = var(r_t, na.rm = TRUE))

print(global_stats)

# global Normality Test
global_normality <- tryCatch(shapiro.test(all_residuals$r_t), error = function(e) NA)
print(global_normality)

# visualization for Each PRN
# directory to save plots
output_dir <- "plots"
dir.create(output_dir, showWarnings = FALSE)

# generate and save a plot for each PRN
unique_prns <- unique(all_residuals$PRN)

for (prn in unique_prns) {
  res_subset <- all_residuals %>% filter(PRN == prn)
  
  p1 <- ggplot(res_subset, aes(x = r_t)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.6) +
    geom_density(color = "red", size = 1) +
    labs(title = paste("Histogram + Density for Residuals, PRN", prn),
         x = "Residual (r_t)", y = "Density") +
    theme_minimal()
  
  p2 <- ggplot(res_subset, aes(y = r_t)) +
    geom_boxplot(fill = "lightgreen", alpha = 0.6) +
    labs(title = paste("Boxplot of Residuals, PRN", prn), y = "Residual (r_t)") +
    theme_minimal()
  
  filename <- paste0(output_dir, "/histogram_residuals_PRN_", prn, ".png")
  ggsave(filename, plot = p1, width = 10, height = 6, dpi = 300, bg = "white")
  filename <- paste0(output_dir, "/boxplot_residuals_PRN_", prn, ".png")
  ggsave(filename, plot = p2, width = 10, height = 6, dpi = 300,bg = "white")
}

# global Density Plot
ggplot(all_residuals, aes(x = r_t)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", alpha = 0.5) +
  geom_density(color = "darkred", size = 1) +
  labs(title = "Global Residuals Density Across All PRNs", x = "Residual (r_t)", y = "Density") +
  theme_minimal()

ggplot(all_residuals, aes(x = datetime, y = r_t, color = factor(PRN))) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, color = "black") +
  labs(title = "Residuals Over Time for All PRNs",
       x = "Datetime", y = "Residual (r_t)",
       color = "PRN") +
  theme_minimal()

# save master residuals table 
write.csv(all_residuals, "all_residuals.csv", row.names = FALSE)