
# Building Probability Model to Guess Score Rank --------------------------

library(tidyverse)
library(httr2)




# Step 1. Get Data from NYC Open Data Portal ------------------------------

my_url <- "https://data.cityofnewyork.us/api/v3/views/vx8i-nprf/query.json"

source("ad_hoc_functions.R") # functions script are pulled from Git using git checkout 

civil_servant_data <- get_data_json(my_url, "civil_service_list")
