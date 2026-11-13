
library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(scales)
library(stringr)


# Read Data --------------------------------------------------------------

general_wide   <- read_rds("data/election_results/ia_2024_leg_general_wide.rds")
target_districts_raw <- read_xlsx("target_districts/target_districts_2024.xlsx")
house_dist_reg <- read_rds("data/house_district_reg/house_district_reg_combined.rds")


# Build Election Margins Table -------------------------------------------

election_margins <- general_wide |>
  ungroup() |>
  transmute(
    race_level,
    district_num,
    total_voters,
    winner_candidate  = first_vote_cand,
    winner_party      = first_vote_party,
    winner_pct        = first_vote_pct,
    second_candidate  = second_vote_cand,
    second_party      = second_vote_party,
    second_pct        = second_vote_pct,
    margin,
    margin_pct        = percent(margin, accuracy = 0.1),
    margin_category
  ) |>
  mutate(
    Chamber = case_when(
      race_level == "State Representative" ~ "House",
      race_level == "State Senator"        ~ "Senate"
    )
  )


# Build District-Level Registration Percentages --------------------------

# Extract district number and use the most recent snapshot
latest_file <- max(house_dist_reg$file_path)

house_reg_pct <- house_dist_reg |>
  filter(file_path == latest_file) |>
  mutate(
    District = as.integer(str_extract(state_representative_district, "\\d+")),
    dem_reg_pct = democratic_active / grand_total,
    rep_reg_pct = republican_active / grand_total,
    no_party_reg_pct = no_party_active / grand_total
  ) |>
  select(District, dem_reg_pct, rep_reg_pct, no_party_reg_pct, grand_total)

# Senate districts are composed of two House districts (Senate N = House 2N-1 + House 2N)
senate_reg_pct <- house_dist_reg |>
  filter(file_path == latest_file) |>
  mutate(senate_district = ceiling(
    as.integer(str_extract(state_representative_district, "\\d+")) / 2
  )) |>
  group_by(senate_district) |>
  summarise(
    dem_reg_pct      = sum(democratic_active) / sum(grand_total),
    rep_reg_pct      = sum(republican_active) / sum(grand_total),
    no_party_reg_pct = sum(no_party_active)   / sum(grand_total),
    grand_total      = sum(grand_total),
    .groups = "drop"
  ) |>
  rename(District = senate_district)


# Join Margins onto Target Districts -------------------------------------

target_districts <- target_districts_raw |>
  left_join(
    election_margins |> select(Chamber, district_num, winner_candidate, margin, margin_category),
    by = c("Chamber", "District" = "district_num")
  ) |>
  relocate(
    winner_candidate, margin, margin_category, .before = 1
  )


# Additional Target Districts --------------------------------------------

# Senate 11 & 13 were not on the 2024 ballot (staggered 4-year terms);
# their margin columns will be NA.
additional_districts <- tibble(
  Chamber  = c("Senate", "Senate", "House", "House", "House",
               "House",  "House",  "House", "House",  "House"),
  District = c(11, 13, 99, 43, 46, 48, 55, 88, 15, 33)
) |>
  left_join(
    election_margins |> select(Chamber, district_num, margin_2024 = margin, margin_category_2024 = margin_category),
    by = c("Chamber", "District" = "district_num")
  )


# Save Output ------------------------------------------------------------

write_xlsx(
  list(
    target_districts     = target_districts,
    additional_districts = additional_districts
  ),
  "target_districts/target_districts_with_margins.xlsx"
)

cat("Saved to target_districts/target_districts_with_margins.xlsx\n")
cat(" - target_districts sheet:     ", nrow(target_districts), "rows\n")
cat(" - additional_districts sheet: ", nrow(additional_districts), "rows\n")
cat(" - Registration data from:     ", latest_file, "\n")
