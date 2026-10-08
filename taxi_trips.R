# Taxi Trips

pacman::p_load(tidyverse, httr2, duckdb, arrow, sf)

# .PARQUET source link: https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page

# source("ad_hoc_functions.R") - 48 million rows of data is too big for R to programmatically install, have to download .PARQUET files
# 
# if(!file.exists("data/yellow_cab_taxi_trips_2025.csv")){ 
# 
# yellow_cab_taxi_trips_2025 <- get_data_json("https://data.cityofnewyork.us/api/v3/views/irx2-vgm4/query.json")
# 
# } else{
# 
#   yellow_cab_taxi_trips_2025 <- readr::read_csv("data/yellow_cab_taxi_trips_2025.csv")
# 
# }


yellow_taxi_trips_aug26 <- arrow::read_parquet("data/yellow_tripdata_2026-08.parquet")

head(yellow_taxi_trips_aug26)

summary(yellow_taxi_trips_aug26)


# Data Dictionary ---------------------------------------------------------


# VendorID == 1 ~ "Creative Mobile Technologies" - Leads Taxi Industry in Integrated Payment Processing and Dispatch
# VendorID == 2 ~ "Curb Mobility, LLC" - Mobile app for Taxi hailing
# VendorID == 6 ~ "Myle Technologies Inc" - supposedly 0 passengers when removing NAs from passenger count? Provides ambulatory and wheelchaor transportation - probably censored since it's per trip
# VendorID == 7 ~ "Helix" - Helix Pay

# RatecodeID == 1 ~ "Standard Rate"
# RatecodeID == 2 ~ "JFK"
# RatecodeID == 3 ~ "Newark"
# RatecodeID == 4 ~ "Nassau or Westchester"
# RatecodeID == 5 ~ "Negotiated fare"
# RatecodeID == 6 ~ "Group Ride"
# RatecodeID == 99 ~ "Null/unknown"

# payment_type == 0 ~ "Flex Fare Trip"
# payment_type == 1 ~ "Credit card"
# payment_type == 2 ~ "Cash"
# payment_type == 3 ~ "No charge"
# payment_type == 4 ~ "Dispute"
# payment_type == 5 ~ "Unknown"
# payment_type == 6 ~ "Voided trip"

yellow_taxi_trips_aug26 |>
  group_by(VendorID) |>
  summarize(passenger_count = sum(passenger_count, na.rm = TRUE),
            fare_amount = sum(fare_amount, na.rm = TRUE)) |>
  filter(VendorID != 6) |>
  ggplot(aes(y = fct_reorder(fct(as.character(VendorID)), passenger_count), x = passenger_count/1e6)) +
  geom_col(orientation = "y") +
  geom_label(aes(label = scales::comma(passenger_count/1e6)), 
             position = "stack") +
  scale_y_discrete(
    labels = c(
      "1" = "Creative Mobile Technologies",
      "2" = "Curb Mobility, LLC",
      "7" = "Helix"
      )
  ) +
  cowplot::theme_cowplot() +
  labs(
    y = "Vendor",
    x = "Passengers (in millions)"
  )

ggsave(filename = "visualizations/figure_1.pdf")


yellow_taxi_trips_aug26 |>
  filter(VendorID == 6) |>
  count(passenger_count == NA) # confirms that all of the trips are censored, Myle Technologies in not In-Universe for this analysis


