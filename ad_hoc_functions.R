# Functions 



# Web API - CSV -----------------------------------------------------------


get_data_csv <- function(access_url,desired_filename){
  
  api_data <- 
    httr2::request(access_url) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |> # ideal for.csv data
    read_csv() |> # if data is .csv
    janitor::clean_names()
  
  if(!dir.exists("data/")){
    
    dir.create("data/")
  
    } 
      
  file_path <- stringr::str_glue("data/{desired_filename}.csv")
  
  readr::write_csv(api_data, file_path)
  
      return(api_data)
  }


# Web API - JSON ----------------------------------------------------------


get_data_json <- function(access_url,desired_filename){
  
 api_data <- 
   httr2::request(access_url) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    dplyr::bind_rows() |>
    janitor::clean_names()
  
  if(!dir.exists("data/")){
    
    dir.create("data/")
    
  } 
 
 file_path <- stringr::str_glue("data/{desired_filename}.csv")
  
 readr::write_csv(api_data, file_path)
  
  return(api_data)
}


