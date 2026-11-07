# # ============================================================
# GLOBAL TEMPERATURE INDEX (BARS)
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

annotation <- data_df %>% arrange(year) %>%
                          slice(1, n()) %>%
                          mutate(t_diff = 0,
                                 x=year+c(-10, +10))

max_t_diff <- format(round(max(data_df$t_diff), 1), nsmall=1)                          

# Check the available data range
print(range(data_df$year, na.rm = TRUE))

# 4. Chart 1: Global Temperature Index (1880–present)
plot <- data_df %>% ggplot(aes(x=year,
                               y=t_diff,
                               fill=t_diff)) +
                    geom_col(show.legend=FALSE) +
                    geom_text(data=annotation, 
                              aes(x=x,
                                  label=year),
                              colour="white") +
                    geom_text(x=1880, 
                              y=1, 
                              hjust=0,
                              label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(data_df$year)}"), color="white") +
  
                    # scale_fill_gradient2(low="darkblue",
                    #                     mid="white",
                    #                     high="darkred",
                    #                     midpoint=0, 
                    #                     limits=c(-1.5, 0.5)) +
  
                    # scale_fill_gradientn(colors=c("darkblue",
                    #                               "white",
                    #                               "darkred"),
                    #                      values=rescale(c(min(data_df$t_diff),
                    #                                       0,
                    #                                       max(data_df$t_diff))),
                    #                      limits=c(min(data_df$t_diff),
                    #                               max(data_df$t_diff))) +
  
                    scale_fill_stepsn(colors=c("darkblue",
                                               "white",
                                               "darkred"),
                                      values=rescale(c(min(data_df$t_diff),
                                                       0,
                                                       max(data_df$t_diff))),
                                      limits=c(min(data_df$t_diff),
                                               max(data_df$t_diff)),
                                      n.breaks=10) +
  
  
                    theme_void() +
                    theme(plot.background=element_rect(fill="black"),
                          legend.text=element_text(colour="white"))


print(plot)

ggsave(
  filename = file.path(
    figures_folder,
    "temperature_bar.png"
  ),
  plot = plot,
  width = 7,
  height = 4,
  # dpi = 300,
  # bg = "transparent"
)