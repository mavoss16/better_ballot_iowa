
library(readxl)
library(readr)

library(dplyr)
library(stringr)
library(purrr)

library(tidyxl)
library(unpivotr)

source("src/00_config.R")

read_data <- function(sheet_name, election_type, election_year){
  print(sheet_name)
  cells <- xlsx_cells(results_path(election_type, election_year, "detail.xlsx"), sheets = sheet_name)
  
  data <- behead(cells, direction = "up-left", name = "contest")
  data <- behead(data, direction = "up-left", name = "candidate")
  data <- behead(data, direction = "left", name = "precinct")
  data <- behead(data, direction = "up", name = "vote_type")
  
  clean_data <- data |>
    mutate(
      value = ifelse(!is.na(numeric), yes = numeric, no = character)
    ) |>
    select(
      contest, precinct, candidate, vote_type, value
    )
  
}



# Read each election's detail workbook ------------------------------------
# One sheet per contest, listed on the "Table of Contents" sheet. Writes
# IA_<type>_<year>_clean_long.rds; workbooks not downloaded yet are skipped

read_election <- function(election_type, election_year){
  detail_path <- results_path(election_type, election_year, "detail.xlsx")
  if(!file.exists(detail_path)){
    message("No ", detail_path, " yet; skipping")
    return(invisible(NULL))
  }

  index <- read_excel(detail_path, sheet = "Table of Contents", skip = 3) |>
    mutate(Page = as.character(Page))

  sheet_list <- index$Page[2:nrow(index)]

  results <- map(sheet_list, ~read_data(.x, election_type, election_year)) |>
    bind_rows()

  write_rds(results, results_path(election_type, election_year, "clean_long.rds"))
}

for(election_year in election_years){
  read_election("primary", election_year)
  read_election("general", election_year)
}
