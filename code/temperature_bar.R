# # ============================================================
# GLOBAL TEMPERATURE INDEX (BARS)
# Data source: NASA GISS GISTEMP
# ============================================================

# 1. Load libraries
library(tidyverse)
library(dplyr)

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

annotation <- data_df %>% arrange(year) %>%
                          slice(1, n()) %>%
                          mutate(t_diff = 0)
                          

# Check the available data range
print(range(data_df$year, na.rm = TRUE))

# 4. Chart 1: Global Temperature Index (1880–present)
plot <- data_df %>% ggplot(aes(x=year,
                               y=t_diff)) +
                    geom_col() %>%
                    geom_text(data = annotation, aes(label=year) +
                    theme_void()


print(plot)

ggsave(
  filename = file.path(
    figures_folder,
    "temperature_bar.png"
  ),
  plot = plot,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "transparent"
)