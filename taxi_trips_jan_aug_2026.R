# Taxi Trends Jan 2026 - Aug 2026

pacman::p_load(tidyverse, httr2, sf, leaflet, tmap, arrow)


file_paths <-
  (list.files("data/taxi_data",
              pattern = "yellow_tripdata",
              full.names = TRUE)) # allows for full path name to be returned rather than file name
  
taxi_trips_data <- 
  arrow::open_dataset(file_paths)

glimpse(taxi_trips_data)

# What is the universe of analysis?

# 1. We know from August 2026 data that Curb Mobility and Creative Mobile Technologies are the two biggest vendors, much bigger than Helix - let's confirm first though using more data.

taxi_trips_data |>
  group_by(VendorID) |>
  summarize(trips_counts = n(),) |>
  mutate(trips_percent = trips_counts/sum(trips_counts, na.rm = TRUE),
         VendorName = case_when(
           VendorID == 1 ~ "Creative Mobile Technologies",
           VendorID == 2 ~ "Curb Mobility",
           VendorID == 7 ~ "Helix",
           VendorID == 6 ~ "Myle Technologies"
         )) |>
  collect()

# We know that Curb Mobility and Creative Mobile Technologies are the two biggest (and only) players

