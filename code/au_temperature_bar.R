
# ============================================================
# AUSTRALIAN TEMPERATURE INDEX (BARS)
# Data source: Australian Bureau of Meteorology (BoM)
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
data_path <- file.path(data_folder, "AU_tmean.txt")

# figures
figures_folder <- file.path(project_folder, "figures")


# 3. Read and prepare data
data_df <- read.table(data_path,
                      header = FALSE,
                      sep = "",
                      col.names = c("period", "t_diff"),
                      colClasses = c("character", "numeric")
)

data_df <- data_df %>% mutate(year = as.integer(substr(period, 1, 4))) %>%
                       select(year, t_diff) %>%
                       drop_na() %>%
                       arrange(year)

# Automatically identify the first and last years
annotation <- data_df %>% slice(1, n()) %>%
                          mutate(t_diff = 0,
                                 x = year + c(-10, 10))

# Check the available data range
print(range(data_df$year, na.rm = TRUE))


# 4. Chart 1: Australian Temperature Index (1910–present)
plot <- data_df %>% ggplot(aes(x = year,
                               y = t_diff,
                               fill = t_diff)) +
                    geom_col(show.legend = FALSE) +
                    
                    # # Zero reference line
                    # geom_hline(yintercept = 0,
                    #            colour = "white",
                    #            linewidth = 0.4) +
                    
                    # Automatically label first and last years
                    geom_text(data = annotation, 
                              aes(x = x,
                                  label = year),
                              colour = "white") +
  
                    # geom_text(x = 1880, 
                    #           y = 1, 
                    #           hjust = 0.1,
                    #           label = glue("AUSTRALIAN ANNUAL MEAN TEMPERATURE ANOMALY ({min(data_df$year)}–{max(data_df$year)})"), 
                    #           color="white") +
                    
                                      
                    # Temperature colour scale
                    scale_fill_stepsn(colors = c("#142F67",
                                                 "white",
                                                 "#3E060A"),
                                      values = rescale(c(min(data_df$t_diff),
                                                         0,
                                                         max(data_df$t_diff))),
                                      limits = range(data_df$t_diff),
                                      n.breaks = 10,
                                      name = "Temperature\nanomaly (°C)") +
                    
                    # Axes
                    scale_x_continuous(breaks = seq(1910,
                                                    max(data_df$year),
                                                    10),
                                       expand = expansion(add = 3)) +
                    
                    scale_y_continuous(breaks = seq(-1.5, 2, 0.5),
                                       limits = c(-1.5, 2),
                                       expand = c(0, 0)) +
                    

                    
                    scale_fill_stepsn(colors = c("darkblue",
                                                 "white",
                                                 "darkred"),
                                      values = rescale(c(min(data_df$t_diff),
                                                         0,
                                                         max(data_df$t_diff))),
                                      limits = c(min(data_df$t_diff),
                                                 max(data_df$t_diff)),
                                      n.breaks = 10) +
                    
                    # Titles
                    labs(title = glue("AUSTRALIAN ANNUAL MEAN TEMPERATURE ANOMALY ({min(data_df$year)}–{max(data_df$year)})")) + 
                    
                    theme_void() +
                    theme(plot.background = element_rect(fill = "black"),
                          plot.title = element_text(color = "white"),
                          legend.text = element_text(colour = "white"))


print(plot)


# 5. Save figure
ggsave(
  filename = file.path(
    figures_folder,
    "au_temperature_bar.png"
  ),
  plot = plot,
  width = 8,
  height = 4,
  # dpi = 300,
  # bg = "transparent"
)
