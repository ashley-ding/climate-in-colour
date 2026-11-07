# # ============================================================
# GLOBAL TEMPERATURE INDEX (WARMING STRIPES)
# Data source: NASA GISS GISTEMP
# ============================================================
# 0. Clean environment
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


# 4. Chart 1: Warming stripes
plot <- data_df %>% ggplot(aes(x = year,
                               y = 1,
                               fill = t_diff)) +
                    geom_tile(show.legend = FALSE) + 
                    scale_fill_stepsn(colors = c("#08306B",
                                                 "white",
                                                 "#67000D"),
                                      values = rescale(c(min(data_df$t_diff), 0, 
                                                       max(data_df$t_diff))),
                                      n.breaks = 12) +
                    coord_cartesian(expand = FALSE) +
                    scale_x_continuous(breaks = seq(1890, 2020, 30)) +
                    labs(title = glue("Global temperature change {min(data_df$year)}-{max(data_df$year)}")) +
                    theme_void() + 
                    theme(axis.text.x = element_text(color = "white",
                                                     margin = margin(t = 5,
                                                                     b = 10,
                                                                     unit = "pt")),
                          plot.title = element_text(color = "white",
                                                    margin = margin(t = 5,
                                                                    b = 10, 
                                                                    unit = "pt"),
                                                    hjust = 0.05),
                          plot.background = element_rect(fill = "black")
                          
                          )




print(plot)

ggsave(
  filename = file.path(
    figures_folder,
    "warming_stripes.png"
  ),
  plot = plot,
  width = 8,
  height = 4,
  # dpi = 300,
  # bg = "transparent"
)







