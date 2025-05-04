
library(readr)
library(fs)

library(dplyr)
library(stringr)
library(purrr)
library(janitor)


files <- dir_ls("data/house_district_reg/csvs")

csv_df <- data.frame(file_path = files) |>
  mutate(
    year_month = str_extract(file_path, "\\d\\d\\d\\d-\\d\\d"),
    year = str_extract(year_month, "^\\d\\d\\d\\d") |> as.numeric(),
    month = str_extract(year_month, "\\d\\d$") |> as.numeric()
  ) |>
  filter(year >= 2014)

data_list <- map(
  csv_df$file_path, 
  .f = function(x){
    read_csv(x) |>
      clean_names() |>
      mutate(file_path = x) |>
      rename_with(
        .cols = everything(),
        .fn = function(x){
          str_replace(x, "rep_", "republican_") |>
            str_replace("dem_", "democratic_") |>
            str_replace("grand_total_all", "grand_total")
        }
      )
    }
)

compare_df_cols(data_list) |> View()
compare_df_cols_same(data_list)


data <- bind_rows(data_list)

write_rds(data, "data/house_district_reg/house_district_reg_combined.rds")


data <- data |>
  mutate(
    year_month = str_extract(file_path, "\\d\\d\\d\\d-\\d\\d"),
    year = str_extract(year_month, "^\\d\\d\\d\\d") |> as.numeric(),
    month = str_extract(year_month, "\\d\\d$") |> as.numeric()
  ) |>
  mutate(
    # Registration Vars
    dem_active_pct = democratic_active / grand_total,
    rep_active_pct = republican_active / grand_total,
    no_party_active_pct = no_party_active / grand_total,
    lib_active_pct = libertarian_active / grand_total,
    two_party_active_pct = dem_active_pct + rep_active_pct,
    active_pct = total_active / grand_total,
    inactive_pct = total_inactive / grand_total,

    # reg_pct = grand_total / adult_pop,
    # dem_reg_pct = (democratic_active + democratic_inactive) / adult_pop,
    # rep_reg_pct = (republican_active + republican_inactive) / adult_pop,
    # two_party_reg_pct = (dem_reg_pct + rep_reg_pct)
  )


# Active Voter Change Over Time ------------------------------------------
latest <- data |>
  filter(year_month == "2025-01") |>
  select(
    county, contains("active")
  ) |>
  select(
    county, ends_with("_pct")
  ) |>
  rename_with(ends_with("_pct"), .fn = ~ paste0(.x, "_2025"))

earliest <- data |>
  filter(year_month == "2015-01") |>
    select(
      county, contains("active")
    ) |>
    select(
      county, ends_with("_pct")
    ) |>
    rename_with(ends_with("_pct"), .fn = ~ paste0(.x, "_2015"))

house_dist_active_diff <- left_join(latest, earliest, by = "county") |>
  mutate(
    active_diff = active_pct_2025 - active_pct_2015,
    rep_active_diff = rep_active_pct_2025 - rep_active_pct_2015,
    dem_active_diff = dem_active_pct_2025 - dem_active_pct_2015,
    no_party_active_diff = no_party_active_pct_2025 - no_party_active_pct_2015,
    two_party_active_diff = two_party_active_pct_2025 - two_party_active_pct_2015,
    .after = county
  )

