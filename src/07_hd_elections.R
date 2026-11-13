
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

source("src/00_config.R")

# Classifies every legislative district x party registration by whether that
# voter had a meaningful vote, for each election year and chamber in
# 00_config.R. Writes:
#   data/voter_classification.rds/.csv: all years and chambers, one row per
#     year x chamber x district x party (only districts with a race that year:
#     half the Senate is up each cycle)
#   data/house_district_voter_classification.rds/.csv: 2024 House only, in the
#     original layout, for 09_newsletter.R

# Cascading categories:
# Pct of adults that aren't registered
# Pct of adults that are registered but didn't vote in general
# Pct of adults that voted in uncompetitive general
# Pct of adults that


# Registration by district and party for one chamber. Senate districts are
# summed from the two House districts nested in each
district_registration <- function(election_year, chamber){
  registration <- read_rds(house_reg_path(election_year)) |>
    filter(state_representative_district != "Totals") |>
    mutate(
      district_num = str_extract(state_representative_district, "\\d\\d\\d$") |> as.numeric()
    ) |>
    select(
      district_num,
      dem_total, rep_total, no_party_total, lib_total, other_total, grand_total
    )

  if(chamber == "Senate"){
    registration <- registration |>
      mutate(district_num = house_to_senate(district_num)) |>
      group_by(district_num) |>
      summarize(across(everything(), sum), .groups = "drop")
  }

  registration |>
    mutate(
      district = paste(chambers[[chamber]], "District", str_pad(district_num, 3, pad = "0")),
      .before = district_num
    ) |>
    rename(reg_total = grand_total) |>
    # Reports without a party's column (Libertarian since it lost major-party
    # status: 2022, 2025-) leave it NA; drop those rows rather than classify them
    pivot_longer(
      c(dem_total, rep_total, no_party_total, lib_total, other_total),
      names_to = "party",
      values_to = "reg_party",
      values_drop_na = TRUE
    ) |>
    mutate(
      party = party |> str_remove("_total") |> str_replace_all("_", " ") |> str_to_title()
    )
}


classify_districts <- function(election_year, chamber){
  inputs <- c(
    house_reg_path(election_year),
    leg_results_path("general", election_year, "long"),
    leg_results_path("general", election_year, "wide"),
    leg_results_path("primary", election_year, "wide")
  )
  if(!all(file.exists(inputs))){
    message("Missing inputs for ", election_year, " (", paste(inputs[!file.exists(inputs)], collapse = ", "), "); skipping")
    return(NULL)
  }

  race_level <- chambers[[chamber]]

  general_long <- read_rds(leg_results_path("general", election_year, "long")) |>
    filter(race_level == !!race_level)
  general_wide <- read_rds(leg_results_path("general", election_year, "wide")) |>
    filter(race_level == !!race_level)
  primary_wide <- read_rds(leg_results_path("primary", election_year, "wide")) |>
    filter(race_level == !!race_level)

  district_registration(election_year, chamber) |>
    # Only districts with a general election this year
    inner_join(
      general_wide |>
        ungroup() |>
        mutate(
          gen_winning_party = first_vote_party |> str_remove("\\."),
          gen_total = total_voters,
          gen_margin = margin,
          gen_margin_category = margin_category,
          gen_competitive = margin <= 0.1
        ) |>
        select(district_num, gen_total, gen_winning_party, gen_margin, gen_margin_category, gen_competitive),
      by = "district_num"
    ) |>
    left_join(
      general_long |>
        ungroup() |>
        mutate(party = str_remove(party, "\\."), gen_party = value, gen_vote_pct = vote_pct) |>
        select(district_num, party, candidate, gen_party, gen_vote_pct),
      by = c("district_num", "party")
    ) |>
    left_join(
      primary_wide |>
        ungroup() |>
        mutate(
          party = party |> str_remove("\\."),
          prim_voters = total_voters,
          prim_margin = margin,
          prim_margin_category = margin_category
        ) |>
        select(district_num, party, prim_voters, prim_margin, prim_margin_category),
      by = c("district_num", "party")
    ) |>
    mutate(
      voter_classification = case_when(
        gen_competitive == TRUE & party %in% c("Dem", "Rep") ~ "Meaningful (General)",
        gen_competitive == FALSE & party == gen_winning_party & prim_margin_category != "Uncontested" ~ "Meaningful (Primary)",
        gen_competitive == TRUE & !(party %in% c("Dem", "Rep")) ~ "None-of-the-Above",
        gen_competitive == FALSE & party == gen_winning_party ~ "Unneeded",
        gen_competitive == FALSE & party != gen_winning_party & (party %in% c("Dem", "Rep")) ~ "Outnumbered",
        gen_competitive == FALSE & party != gen_winning_party & !(party %in% c("Dem", "Rep")) ~ "Excluded"
      ),
      year = election_year,
      chamber = chamber
    ) |>
    select(
      year, chamber, district, district_num, party, voter_classification,
      reg_total, reg_party,
      candidate, gen_total, gen_party, gen_vote_pct, gen_winning_party, gen_margin, gen_margin_category, gen_competitive,
      prim_voters, prim_margin, prim_margin_category
    ) |>
    arrange(district_num)
}


districts <- expand_grid(year = election_years, chamber = names(chambers)) |>
  pmap(\(year, chamber) classify_districts(year, chamber)) |>
  bind_rows()


# Share of registered voters in each classification
districts |>
  group_by(year, chamber, voter_classification) |>
  summarize(
    reg = sum(reg_party),
    .groups = "drop_last"
  ) |>
  mutate(
    reg_pct = (reg / sum(reg)) * 100
  ) |>
  arrange(year, chamber, desc(reg_pct)) |>
  print(n = Inf)


write_csv(districts, "data/voter_classification.csv")
write_rds(districts, "data/voter_classification.rds")

house_2024 <- districts |>
  filter(year == 2024, chamber == "House") |>
  select(-year, -chamber) |>
  rename(state_representative_district = district)

write_csv(house_2024, "data/house_district_voter_classification.csv")
write_rds(house_2024, "data/house_district_voter_classification.rds")
