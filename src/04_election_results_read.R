
library(readxl)
library(readr)

library(dplyr)
library(stringr)
library(purrr)

library(tidyxl)
library(unpivotr)


read_data <- function(sheet_name, election_type){
  print(sheet_name)
  cells <- xlsx_cells(paste0("data/election_results/IA_", election_type, "_2024_detail.xlsx"), sheets = sheet_name)
  
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



# Primary Election --------------------------------------------------------

primary_index <- read_excel("data/election_results/IA_primary_2024_detail.xlsx", sheet = "Table of Contents", skip = 3) |>
  mutate(Page = as.character(Page))

primary_sheet_list <- primary_index$Page[2:nrow(primary_index)]


primary_data_list <- map(primary_sheet_list, ~read_data(.x, "primary"))

all_primary_results <- bind_rows(primary_data_list)

write_rds(all_primary_results, "data/election_results/IA_primary_2024_clean_long.rds")


# General Election --------------------------------------------------------

general_index <- read_excel("data/election_results/IA_general_2024_detail.xlsx", sheet = "Table of Contents", skip = 3) |>
  mutate(Page = as.character(Page))

general_sheet_list <- general_index$Page[2:nrow(general_index)]

general_data_list <- map(general_sheet_list, ~read_data(.x, "general"))

all_general_results <- bind_rows(general_data_list)

write_rds(all_general_results, "data/election_results/IA_general_2024_clean_long.rds")
