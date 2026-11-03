library(tidyverse)


# define file paths
script_path <- rstudioapi::getActiveDocumentContext()$path

code_folder <- dirname(script_path)
project_folder <- dirname(code_folder)

data_folder <- file.path(project_folder, "data")

data_path <- file.path(data_folder, "GLB.Ts+dSST.csv")

# read data
data_df = read.csv(data_path, skip=1)

# view(data_df)