# Taxi Trips

pacman::p_load(tidyverse, httr2, duckdb, arrow, sf, leaflet, tmap)

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

# prep yellow cab data

yellow_taxi_trips_aug26 <- arrow::read_parquet("data/taxi_data/yellow_tripdata_2026-08.parquet")

head(yellow_taxi_trips_aug26)

summary(yellow_taxi_trips_aug26)



# 0. Green Taxi and HVFH Data ---------------------------------------------


# # prep green cab data
# 
# green_taxi_trips_aug26 <- 
#   arrow::read_parquet("data/taxi_data/green_tripdata_2026-08.parquet") |>
#   select(!ehail_fee) # all NA
#  
# head(green_taxi_trips_aug26)
# 
# summary(green_taxi_trips_aug26)
# 
# # prep hvfhv cab data
# 
# hvfhv_trips_aug26 <- arrow::read_parquet("data/taxi_data/fhvhv_tripdata_2026-08.parquet") # High Volume For-Hire Vehicle, mispelled wrong on website
# 
# head(hvfhv_trips_aug26) # 20+ MILLION OBS

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

# yellow_taxi_trips_aug26 |>
#   filter(VendorID == 6) |>
#   count(is.na(passenger_count)) # confirms that all of the trips are censored, Myle Technologies in not In-Universe for this analysis


# 1. Passengers by Vendor ----------------------------------------------------


yellow_taxi_trips_aug26 |>
  group_by(VendorID) |>
  summarize(passenger_count = sum(passenger_count, na.rm = TRUE)) |>
  filter(VendorID != 6) |>
  ggplot(aes(y = fct_reorder(fct(as.character(VendorID)), passenger_count), x = passenger_count/1e6)) +
  geom_col(orientation = "y") +
  geom_text(aes(label = scales::comma(passenger_count/1e6)), hjust = 0, nudge_x = 0.05) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .15))
  ) +
  scale_y_discrete(
    labels = c(
      "1" = "Creative Mobile Technologies",
      "2" = "Curb Mobility, LLC",
      "7" = "Helix"
      )
  ) +
  cowplot::theme_cowplot() +
  labs(
    title = "Reported Yellow Taxi Passengers by Vendor",
    subtitle = "August 2026, Millions of Passengers",
    caption = "Source: 2025 Yellow Taxi Trip Data | NYC Open Data",
    x = "Passengers (in millions)"
  ) +
  theme(
    axis.title.y = element_blank()
  )

ggsave(filename = "visualizations/figure_1.pdf", dpi = 600, height = 6, width = 12) # PPTX standard



# 2. Fare Amount by Vendor ------------------------------------------------

yellow_taxi_trips_aug26 |>
  group_by(VendorID) |>
  summarize(fare_amount = sum(fare_amount, na.rm = TRUE)) |>
  filter(VendorID != 6) |>
  ggplot(aes(y = fct_reorder(fct(as.character(VendorID)), fare_amount), x = fare_amount/1e6)) +
  geom_col(orientation = "y") +
  geom_text(aes(label = scales::dollar((fare_amount/1e6))), hjust = 0, nudge_x = 0.05) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .15))
  ) +
  scale_y_discrete(
    labels = c(
      "1" = "Creative Mobile Technologies",
      "2" = "Curb Mobility, LLC",
      "7" = "Helix"
    )
  ) +
  cowplot::theme_cowplot() +
  labs(
    title = "Reported Annual Fare Amount by Vendor",
    subtitle = "August 2026, Millions of Dollars($)",
    caption = "Source: 2026 Yellow Taxi Trip Data | NYC Open Data",
    x = "Fare Amount (in millions $)"
  ) +
  theme(
    axis.title.y = element_blank()
  )

ggsave(filename = "visualizations/figure_2.pdf", dpi = 600, height = 6, width = 12) # PPTX standard


# 3. Count of Payment Type by Vendor -----------------------------------------

yellow_taxi_trips_aug26 |>
  filter(VendorID != 6) |>
  group_by(payment_type, VendorID) |>
  summarize(count = n(), .groups = "drop") |>
  group_by(VendorID) |>
  mutate(payment_prop = count/sum(count)) |>
  ggplot(aes(x = payment_prop, y = fct_reorder(fct(as.character(VendorID)), count), fill = fct(as.character(payment_type)), group = fct(as.character(payment_type)))) +
  geom_col(position = "stack", orientation = "y") +
  geom_text(aes(
    label = if_else(payment_prop >= 0.05, scales::percent(payment_prop, accuracy = .01), "")),
    position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_viridis_d(
    name = "Payment Type",
    labels = c(
      "0" = "Flex Fare",
      "1" = "Credit Card",
      "2" = "Cash",
      "3" = "No Charge",
      "4" = "Dispute",
      "5" = "Unknown",
      "6" = "Voided Trip"
    )
    ) +
  scale_y_discrete(
    labels = 
    c("1" = "Creative Mobile Technologies",
    "2" = "Curb Mobility, LLC",
    "7" = "Helix"
    )
    ) +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    title = "Acceptance of Credit Cards is prevalent across Vendors,\nbut Flex Fares Acceptance Varies",
    subtitle = "Passenger preference for ride payment varies across vendors,\n though credit card remains the dominant form of payment.",
    caption = "Source: 2026 Yellow Taxi Trip Data | NYC Open Data",
    x = "Percent of Total Trips",
  ) +
  cowplot::theme_cowplot() +
  theme(
    axis.title.y = element_blank()
  )

ggsave(filename = "visualizations/figure_3.pdf", dpi = 600, height = 6, width = 12) # PPTX standard


# 4. Geographic Breakdown - Location Pickup -------------------------------------------------


# crsuggest::suggest_crs(filter(taxi_zones, borough != "EWR"))

taxi_zones <-
  read_sf("data/taxi_data/taxi_zones/") |>  # taxi_zone shape data
  st_as_sf() |>
  st_transform(crs = 6524) # 6524 if including EWR, 6539 otherwise

yellow_cab_pickups <-
  yellow_taxi_trips_aug26 |>
  left_join(taxi_zones, join_by(PULocationID == LocationID)) |>
  st_as_sf() |>
  filter(VendorID != "6") |>
  mutate(Vendor_name = case_when(
    VendorID == 1 ~ "Creative Mobile Technologies",
    VendorID == 2 ~ "Curb Mobility, LLC", 
    VendorID == 7 ~ "Helix Pay",
  ),
  Vendor_name = fct(Vendor_name))

tictoc::tic()

pickups_aug2026 <-
  yellow_cab_pickups |>
  group_by(VendorID, Vendor_name, PULocationID, zone, geometry) |>
  summarize(num_pickups = n(),
            day = lubridate::day(tpep_pickup_datetime),
            time_of_day = lubridate::hour(tpep_pickup_datetime),
            .groups = "drop") |>
  tm_shape() +
  tm_polygons(fill = "num_pickups",
              fill.legend = tm_legend(title = "Number of Passenger Pickups",reverse = TRUE)) +
  tm_facets(by = "Vendor_name")
# tm_animate_fast(frames = "day")

tictoc::toc()

tmap_save(pickups_aug2026, filename = "visualizations/figure_4.pdf", dpi = 600, height = 6, width = 12)


# Old Code for sf() and patchwork() maps ----------------------------------

# yellow_cab_pickups |>
#   group_by(PULocationID, zone) |>
#   summarize(num_pickups = n(), .groups = "drop") |>
#   ggplot() +
#   geom_sf(aes(fill = num_pickups)) +
#   scale_fill_viridis_c(
#     name = "Number of Passenger Pickups",
#     values = scales::label_comma()
#   ) +
#   ggthemes::theme_map()
# 
# brooklyn_taxi_zones_plot_pu <-
#   taxi_zones |>
#   filter(borough == "Brooklyn") |>
#   ggplot() +
#   geom_sf() +
#   ggthemes::theme_map() +
#   facet_wrap(~borough)
# 
# bronx_taxi_zones_plot_pu <-
#   taxi_zones |>
#   filter(borough == "Bronx") |>
#   ggplot() +
#   geom_sf() +
#   ggthemes::theme_map() +
#   facet_wrap(~borough)
# 
# manhattan_taxi_zones_plot_pu <-
#   taxi_zones |>
#   filter(borough == "Manhattan") |>
#   ggplot() +
#   geom_sf() +
#   ggthemes::theme_map() +
#   facet_wrap(~borough)
# 
# queens_taxi_zones_plot_pu <-
#   taxi_zones |>
#   filter(borough == "Queens") |>
#   ggplot() +
#   geom_sf() +
#   ggthemes::theme_map() +
#   facet_wrap(~borough)
# 
# staten_island_taxi_zones_plot_pu <-
#   taxi_zones |>
#   filter(borough == "Staten Island") |>
#   ggplot() +
#   geom_sf() +
#   ggthemes::theme_map() +
#   facet_wrap(~borough)
# 
# library(patchwork)
# 
# wrap_plots(brooklyn_taxi_zones_plot, bronx_taxi_zones_plot, manhattan_taxi_zones_plot, queens_taxi_zones_plot, staten_island_taxi_zones_plot) +
#   plot_annotation(
#     title = "NYC Taxi Zones"
#   )


