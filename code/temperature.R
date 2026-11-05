# ============================================================
# GLOBAL TEMPERATURE INDEX & SUSTAINABILITY MILESTONES
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
                       drop_na() %>%
                       arrange(year) 

# Check the available data range
print(range(data_df$year, na.rm = TRUE))

# 4. Chart 1: Global Temperature Index (1880–present)
plot <- data_df %>% ggplot(aes(x=year,
                               y=t_diff)) +
                    geom_line(aes(color="1"), 
                              size=0.5,
                              show.legend=FALSE) +
                    geom_point(fill="white", 
                               aes(color="1"),
                               shape=21,
                               show.legend=TRUE) +
                    geom_smooth(se=FALSE, # hide the shaded confidence interval
                                aes(color="2"),
                                size=1,
                                span=0.15,
                                show.legend=FALSE) + # control how much smoothing is applied
                    scale_x_continuous(breaks=seq(1880,2026,20), expand=c(0,0)) +
                    scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) +
                    scale_color_manual(name=NULL,
                                       breaks=c(1,2),
                                       values=c("gray", "black"),
                                       labels=c("Annual mean", "Lowess smoothing"),
                                       guide=guide_legend(override.aes=list(shape=15))) +
                    labs(x="YEAR",
                         y="Temperature anomaly (C)",
                         title="GLOBAL LAND-OCEAN TEMPERATURE INDEX", 
                         subtitle = paste0(
                           "Data source: NASA's Goddard Institute for Space Studies (GISS).\n",
                           "Credit: NASA/GISS"
                         )
                    ) + 
                    theme_light() +
                    theme(axis.ticks=element_blank(),
                          plot.title.position="plot",
                          plot.title=element_text(margin=margin(b=10),
                                                  # color="red",
                                                  face="bold"),
                          plot.subtitle=element_text(size=8,
                                                     margin=margin(b=10)),
                          legend.position=c(0.15, 0.9),
                          legend.title=element_text(size=0),
                          legend.key.height=unit(10, "pt"),
                          legend.margin=margin(0,0,0,0)
                    )

print(plot)


ggsave(
  filename = file.path(
    figures_folder,
    "temperature_line.png"
  ),
  plot = plot,
  width = 6,
  height = 4,
  # dpi = 300,
  # bg = "transparent"
)




# 5. Chart 2: THE ROAD TO PARIS
# Global Land-Ocean Temperature Index


# Define major climate milestones
cop_events <- tibble(
  year = c(
    1988, 1992, 1997, 2005, 2009, 2015, 2016
  ),
  
  event = c(
    "IPCC established",
    "UNFCCC adopted",
    "COP3: Kyoto Protocol",
    "Kyoto Protocol entered into force",
    "COP15: Copenhagen Accord",
    "COP21: Paris Agreement",
    "Paris Agreement\nentered into force"
  ),
  
  # Adjust label positions to avoid overlap
  label_x = c(
    1987, 1992, 1999, 2005, 2009, 2017, 2022
  ),
  
  # Alternate between top and bottom
  label_y = c(
    1.25, -0.5, 1, -0.25, 1.25, -0.75, 0.55
  )
)

# Define timeline and vertical line endpoints
cop_events <- cop_events %>%
  mutate(
    timeline_y = 0.25,
    
    # Align vertical lines with the text edges
    line_end = if_else(
      label_y > timeline_y,
      label_y - 0.05,  # Top labels
      label_y + 0.05   # Bottom labels
    ),
    
    # Align text relative to the line endpoint
    label_vjust = if_else(
      label_y > timeline_y,
      0,  # Top: bottom edge at label_y
      1   # Bottom: top edge at label_y
    )
  )


# Create chart

paris_plot <- data_df %>%
  filter(year >= 1980, year <= 2026) %>%
  ggplot(aes(x = year, y = t_diff)) +
  
  # Annual temperature series
  geom_line(
    color = "gray60",
    linewidth = 0.6
  ) +
  
  # Vertical milestone lines at actual event years
  geom_segment(
    data = cop_events,
    aes(
      x = year,
      xend = year,
      y = timeline_y,
      yend = line_end
    ),
    inherit.aes = FALSE,
    color = "gray80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  
  # Connect the 2016 line to its shifted label
  geom_segment(
    data = cop_events %>% filter(year == 2016),
    aes(
      x = year,
      xend = label_x,
      y = line_end,
      yend = line_end
    ),
    inherit.aes = FALSE,
    color = "gray80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  
  # Horizontal milestone timeline
  geom_hline(
    yintercept = 0.25,
    color = "gray",
    linewidth = 0.4
  ) +
  
  # Milestone markers at actual event years
  geom_point(
    data = cop_events,
    aes(x = year, y = timeline_y),
    inherit.aes = FALSE,
    color = "red",
    size = 2
  ) +
  
  # Milestone labels
  geom_text(
    data = cop_events,
    aes(
      x = label_x,
      y = label_y,
      label = event,
      vjust = label_vjust
    ),
    inherit.aes = FALSE,
    color = "gray35",
    size = 3,
    lineheight = 1.1,
    show.legend = FALSE
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(1980, 2030, 5),
    expand = c(0, 0)
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(-1, 1.5, 0.5),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(
    xlim = c(1980, 2026),
    ylim = c(-1, 1.5)
  ) +
  
  # Titles and data source
  labs(
    x = "YEAR",
    y = "Temperature anomaly (°C)",
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
    subtitle = paste0(
      "The Road to Paris\n",
      "Data source: NASA's Goddard Institute for Space Studies (GISS)."
    )
  ) +
  
  # Minimalist styling
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 9,
      lineheight = 1.5,
      margin = margin(b = 12)
    )
  )



print(paris_plot)


ggsave(
  filename = file.path(
    figures_folder,
    "road_to_paris.png"
  ),
  plot = paris_plot,
  width = 6,
  height = 4,
  # dpi = 300,
  # bg = "transparent"
)
