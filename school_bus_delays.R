
pacman::p_load(tidyverse, here, tidymodels, ggpubr, bookdown, codebookr, httr2, jsonlite, dotenv, gtsummary, patchwork, cowplot)

# Build API Function to Recycle for NYC Open Data site --------------------

load_dot_env()

my_api_key <- Sys.getenv("nyc_open_data_api_key")

source("ad_hoc_functions.R")

nyc_opt_data_delays <- 
  get_data_csv("https://data.cityofnewyork.us/api/v3/views/ez4e-fazm/query.csv","bus_delays") |>
  select(!c(id, version, created_at, updated_at, created_on, informed_on, 
            incident_number,last_updated_on))


head(nyc_opt_data_delays)


# Exploratory Data Analysis ---------------------------------------------------------

# nyc_ops_data |>
#   # filter(school_year %in% c("2016-2017","2025-2026"), affiliation != "N/A", door_to_door_service == "Yes") |>
#   # group_by(affiliation, school_year) |>
#   # count(door_to_door_service) |>
#   # slice_max(order_by = n, n = 5) |>
#   # ggplot(aes(x = affiliation, y = n)) +
#   # geom_col() +
#   # ggthemes::theme_clean() + 
#   # theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
#   # scale_y_continuous(labels = scales::label_comma()) +
#   # labs( x = "School Affiliation",
#   #       y = "Number of Schools Offering Door-to-Door Bus Service") +
#   # geom_text(aes(label = n)) + 
#   # facet_wrap(~school_year)

# vector to subset based on school years of interest

school_years_of_interest <- c("2015-2016", "2016-2017", "2017-2018","2018-2019",
                              "2019-2020", "2020-2021", "2021-2022","2022-2023",
                              "2023-2024","2024-2025", "2025-2026")

# factorize school year variable
nyc_opt_data_delays$school_year <- forcats::fct(nyc_opt_data_delays$school_year)

# reorder since order makes sense

nyc_opt_data_delays$school_year <- forcats::fct_relevel(nyc_opt_data_delays$school_year, school_years_of_interest)

# filter for scope of interest: The Five Boros from 2015-2026

nyc_opt_data_delays <- 
  nyc_opt_data_delays |>
  filter(school_year %in% school_years_of_interest, 
         boro %in% c("Brooklyn","Bronx","Manhattan","Queens","Staten Island"))
  
# exploratory analysis - trend over time - as expected, 2020-2021 drop due to pandemic. 
# but, why did 2018-2019 have so many breakdowns? Why was there an upward trend pre-pandemic, 
# and now a flatter trend post-pandemic?

nyc_opt_data_delays |>
  group_by(school_year) |>
  summarize(num_delays = n()) |># increase in the number of instance - or stricter reporting?
  ggplot(aes(x = school_year, y = num_delays)) + 
  geom_col() +
  theme_classic(base_size = 12) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Number of Delays Across School Years: 2015-2026",
    x = "School Year",
    y = "Number of Delays",
    caption = "Source: Office of Pupil Transportation, City of New York"
  ) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(size = 14, face = 
                                "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(9),
    axis.text.x = element_text(angle = 45, vjust = .5))


# Exploratory Analysis #1 - Delay by Type ---------------------------------

# Clearly, heavy traffic is the biggest reason by wide margin

nyc_opt_data_delays |>
  group_by(reason) |>
  summarize(num_reason_type = n()) |>
  ungroup() |>
  slice_max(order_by = num_reason_type, n = 5) |> # top 5 issues since there is a clear front-runner looking at data
  ggplot(aes(x = sort(reason), y = num_reason_type)) +
  geom_col() +
  ggthemes::theme_clean() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Total Number of Delays by Reason Type from 2015-2026",
    x = "Reason",
    y = "Number of Instances",
    caption = "Source: Office of Pupil Transportation, City of New York"
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  geom_text(aes(label = comma(num_reason_type), vjust = -.5))


# Question - are these delays concentrated in one borough of NYC? Seems like Bronx and Manhattan lead compared to Queens. 
# Though, it's interesting that Brooklyn is the only boro with "other" as a leading category.


# Exploratory Analysis #2 - Delays by Boroughs ----------------------------


nyc_opt_data_delays |>
  group_by(boro, reason) |>
  summarize(
            num_reason_type = n()) |>
  ungroup() |>
  slice_max(order_by = num_reason_type, n = 5) |>
  ggplot(aes(x = sort(reason), y = num_reason_type, fill = boro)) +
  geom_col(position = "dodge") +
  ggthemes::theme_clean() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Total Number of Delayed School Buses in the Last Decade: 2015-2026",
    x = "Reason",
    y = "Number of Delays",
    caption = "Source: Office of Pupil Transportation, City of New York",
    fill = "Boroughs") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  geom_text(aes(label = comma(num_reason_type), vjust = -.5), position = position_dodge(width = .95), size = 2.4)



# Exploratory Analysis #3 - Exploring 2018-2019 Delays --------------------

nyc_opt_data_delays_1819 <- 
  nyc_opt_data_delays |>
  filter(school_year == "2018-2019") |> # 108,220 instances
  mutate(month = lubridate::month(occurred_on, label = T),
         weekday = lubridate::wday(occurred_on, label = T)
         )

nyc_opt_data_delays_1819 |>
  group_by(month, weekday) |>
  summarize(num_delay = n()) |>
  ggplot() +
  geom_col(aes(x = month, y = num_delay)) +
  theme_classic()



