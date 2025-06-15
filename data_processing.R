rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(lubridate)
library(fuzzyjoin)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# GPS TEC reading
gps_tec <- read.table("data/gps-tec-output.Cmn", skip = 2, header = TRUE)

# Convert MJD to POSIXct (datetime)
gps_tec$datetime <- as.POSIXct((gps_tec$MJdatet) * 86400, origin = "1858-11-17", tz = "UTC")

head(gps_tec)
summary(gps_tec)

gps_tec_1 <- gps_tec %>%
  filter(PRN == 1)

# TEC SUITE 1 .dat reading
tec_suite_1 <- read.table("data/tecsuite/ac12_G01_160_25.dat", skip = 10, header = FALSE,
                        col.names = c("tsn", "hour", "el", "az", "tec.l1l2", "tec.p1p2", "validity"))
start_time <- as.POSIXct("2025-06-09 00:00:00", tz = "UTC")
tec_suite_1$datetime <- start_time + (tec_suite_1$hour * 3600)

gps_tec_1 <- gps_tec_1 %>%
  rename(Vtec_TECGPS = Vtec)

tec_suite_1 <- tec_suite_1 %>%
  rename(Vtec_tecsuite = tec.l1l2)

# combined for satellite 1
residuals <- difference_inner_join(
  gps_tec_1, tec_suite_1,
  by = c("datetime" = "datetime"),
  max_dist = 2,
  distance_col = "time_diff"
)

# compute residuals
residuals <- residuals %>%
  mutate(r_t = Vtec_tecsuite - Vtec_TECGPS)

# draw plots
# Reshape to long format for ggplot
residuals_long <- residuals %>%
  select(datetime.x, Vtec_tecsuite, Vtec_TECGPS, r_t) %>%
  pivot_longer(cols = c(Vtec_tecsuite, Vtec_TECGPS, r_t), 
               names_to = "Variable", values_to = "Value")

ggplot(residuals_long, aes(x = datetime.x, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "VTEC and Residuals over Time",
       x = "Datetime", y = "Value",
       color = "Variable") +
  theme_minimal()

# placing all tecsuite files together - needed?
#dat_files <- list.files(path = "./data/tecsuite", pattern = "\\.dat$", full.names = TRUE)
#tec_suite_all <- lapply(dat_files, function(file) {
#  read.table(file, skip = 10, header = FALSE,
#             col.names = c("tsn", "hour", "el", "az", "tec.l1l2", "tec.p1p2", "validity"))
#}) %>% bind_rows()

#start_time <- as.POSIXct("2025-06-09 00:00:00", tz = "UTC")
#tec_suite_all$datetime <- start_time + (tec_suite_all$hour * 3600)

# sort by datetime
#tec_suite_all <- tec_suite_all %>%
#  arrange(datetime)

#head(tec_suite_all)