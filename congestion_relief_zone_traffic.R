# Congestion Relief Zone Traffic


# Package Installation ----------------------------------------------------

# install.packages("pacman") if not already installed
  

pacman::p_load(tidyverse, here, tidymodels, ggpubr, bookdown, codebookr, httr2, jsonlite, dotenv, sf)


# Get Data Using Ad Hoc API Functions -------------------------------------

source("ad_hoc_functions.R") # WOW! Best way to call functions used across scripts

congestion_relief_entry_data <- get_data_json("https://data.ny.gov/api/v3/views/t6yz-b64h/query.json",
                                                "congestion_relief_vehicle_entries")
# data too big? Hold for now