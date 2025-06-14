rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# GPS TEC reading
gps_tec <- read.table("data/gps-tec-output.Cmn", skip = 2, header = TRUE)

head(gps_tec)
summary(gps_tec)

# TEC SUITE reading
tec_suite <- read.table("data/tecsuite/ac12_G01_160_25.dat", skip = 10, header = FALSE,
                        col.names = c("tsn", "hour", "el", "az", "tec.l1l2", "tec.p1p2", "validity"))

head(tec_suite)
summary(tec_suite)