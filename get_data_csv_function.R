
library(tidyverse)
library(here)
library(tidymodels)
library(ggthemes)
library(ggpubr)
library(bookdown)
library(codebookr)
library(httr2)
library(jsonlite)
library(dotenv)



# Build API Function to Recycle for NYC Open Data site --------------------

load_dot_env()

my_api_key <- Sys.getenv("nyc_open_data_api_key")

get_data_csv <- function(access_url){

  httr2::request(access_url) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |> # ideal for.csv data
    read_csv() # if data is .csv

}


nyc_data <- get_data_csv("http://data.cityofnewyork.us/api/v3/views/hg3c-2jsy/export.csv?accessType=DOWNLOAD")
