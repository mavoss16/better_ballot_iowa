

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)

library(tigris)
library(mapview)

file_list <- list.files("Voter Registration/data_csv", full.names = TRUE)

data_list <- map(
  file_list,
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

name_comparisons <- compare_df_cols(data_list)

data <- bind_rows(data_list) |>
  filter(county != "Totals") |>
  mutate(
    # Date Vars
    year_month = str_extract(file_path, "\\d\\d\\d\\d-\\d\\d"),
    year = str_extract(year_month, "^\\d\\d\\d\\d") |> as.numeric(),
    month = str_extract(year_month, "\\d\\d$") |> as.numeric(),
    .after = county
  ) |>
  mutate(
    # Registration Vars
    dem_active_pct = democratic_active / grand_total,
    rep_active_pct = republican_active / grand_total,
    no_party_active_pct = no_party_active / grand_total,
    lib_active_pct = libertarian_active / grand_total,
    two_party_active_pct = dem_active_pct + rep_active_pct,
    active_pct = total_active / grand_total,
    inactive_pct = total_inactive / grand_total
  )


latest <- data |>
  filter(year_month == "2025-01") |>
  select(
    county, ends_with("_pct")
  ) |>
  rename_with(ends_with("_pct"), .fn = ~ paste0(.x, "_2025"))

earliest <- data |>
  filter(year_month == "2015-01") |>
    select(
      county, ends_with("_pct")
    ) |>
    rename_with(ends_with("_pct"), .fn = ~ paste0(.x, "_2015"))

county_data <- left_join(latest, earliest, by = "county") |>
  mutate(
    active_diff = active_pct_2025 - active_pct_2015,
    rep_active_diff = rep_active_pct_2025 - rep_active_pct_2015,
    dem_active_diff = dem_active_pct_2025 - dem_active_pct_2015,
    no_party_active_diff = no_party_active_pct_2025 - no_party_active_pct_2015,
    two_party_active_diff = two_party_active_pct_2025 - two_party_active_pct_2015
    .after = county
  )

county_sf <- counties(state = "IA")

sf_data <- left_join(county_sf, county_data, by = c("NAME" = "county"))


library(ggplot2)

ggplot(sf_data, aes(fill = active_pct_2025)) +
  geom_sf() +
  theme_void()

mapview(sf_data, zcol = "active_pct_2025")

write_csv(data, "clean_data/registrations_merged_202405_202501.csv")
write_rds(sf_data, "clean_data/registrations_sf_202501.rds")
