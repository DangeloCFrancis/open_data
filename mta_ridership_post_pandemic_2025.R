# MTA RIdership Pre-Post Pandemic 

get_data_json <- function(access_url){
  
  httr2::request(access_url) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    dplyr::bind_rows()
}


mta_ridership_data <- get_data_json("https://data.ny.gov/api/v3/views/vxuj-8kew/query.json")
