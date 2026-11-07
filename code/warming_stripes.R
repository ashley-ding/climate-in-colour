# # ============================================================
# GLOBAL TEMPERATURE INDEX (WARMING STRIPES)
# Data source: NASA GISS GISTEMP
# ============================================================
rm(list=ls())

# 1. Load libraries
library(tidyverse)
library(dplyr)
library(scales)
library(glue)

# 2. Define file paths
script_path <- rstudioapi::getActiveDocumentContext()$path

code_folder <- dirname(script_path)
project_folder <- dirname(code_folder)

# data
data_folder <- file.path(project_folder, "data")
data_path <- file.path(data_folder, "GLB.Ts+dSST.csv")

# figures
figures_folder <- file.path(project_folder, "figures")


# 3. Read and prepare data
data_df <- read.csv(data_path, skip = 1, na.strings = "***")

# Alternative: read directly from NASA
# url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
# data_df <- read.csv(url, skip = 1, na.strings = "***")

data_df <- data_df %>% select(year = Year, t_diff = J.D) %>%
                       mutate(year = as.integer(year),
                              t_diff = as.numeric(t_diff)) %>%
                       drop_na()













