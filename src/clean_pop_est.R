# Clean and combine Census Bureau adult population data for 2014-2025
# Author: Matthew Voss

library(readr)
library(dplyr)
library(janitor)

old_popest <- read_csv("data/pop_est/CC-EST2020-AGESEX-19.csv") |>
    clean_names() |>
    filter(!(year %in% c(1, 2, 13))) |>
    transmute(
        county = ctyname,
        year = case_when(
            year == 3 ~ 2010,
            year == 4 ~ 2011,
            year == 5 ~ 2012,
            year == 6 ~ 2013,
            year == 7 ~ 2014,
            year == 8 ~ 2015,
            year == 9 ~ 2016,
            year == 10 ~ 2017,
            year == 11 ~ 2018,
            year == 12 ~ 2019,
            year == 14 ~ 2020
        ),
        pop_est = popestimate,
        adult_pop = age18plus_tot
    )

new_popest <- read_csv("data/pop_est/cc-est2023-agesex-19.csv") |>
    clean_names() |>
    filter(!(year %in% c(1, 2))) |>
    transmute(
        county = ctyname,
        year = case_when(
            year == 3 ~ 2021,
            year == 4 ~ 2022,
            year == 5 ~ 2023
        ),
        pop_est = popestimate,
        adult_pop = age18plus_tot
    )


popest <- bind_rows(old_popest, new_popest) |>
    mutate(
        county = stringr::str_remove(county, " County")
    )

write_rds(popest, "clean_data/pop_estimates.rds")
