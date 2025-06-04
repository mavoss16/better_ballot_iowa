
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Cascading categories:
# Pct of adults that aren't registered
# Pct of adults that are registered but didn't vote in general
# Pct of adults that voted in uncompetitive general
# Pct of adults that 


house_primary_wide <- read_rds("data/election_results/ia_2024_leg_primary_wide.rds") |> 
  filter(race_level == "State Representative")
house_primary_long <- read_rds("data/election_results/ia_2024_leg_primary_long.rds") |> 
  filter(race_level == "State Representative")

house_general_wide <- read_rds("data/election_results/ia_2024_leg_general_wide.rds") |> 
  filter(race_level == "State Representative")
house_general_long <- read_rds("data/election_results/ia_2024_leg_general_long.rds") |> 
  filter(race_level == "State Representative")

registration <- read_rds("data/house_district_reg/ia_2024_house_reg.rds")


districts <- registration |>
  filter(state_representative_district != "Totals") |>
  mutate(
    district_num = str_extract(state_representative_district, "\\d\\d\\d$") |> as.numeric()
  ) |>
  select(
    state_representative_district, district_num,
    dem_total, rep_total, no_party_total, lib_total, other_total, grand_total
  ) |>
  rename(reg_total = grand_total) |>
  pivot_longer(
    c(dem_total, rep_total, no_party_total, lib_total, other_total),
    names_to = "party",
    values_to = "reg_party"
  ) |>
  mutate(
    party = party |> str_remove("_total") |> str_replace_all("_", " ") |> str_to_title()
  ) |>
  left_join(
    house_general_long |>
      ungroup() |>
      mutate(party = str_remove(party, "\\."), general_votes = value, general_vote_pct = vote_pct) |>
      select(district_num, party, candidate, general_votes, general_vote_pct)
  ) |>
  left_join(
    house_general_wide |> 
      ungroup() |>
      mutate(
        winning_party = first_vote_party |> str_remove("\\."),
        voters_total = total_voters,
        voters_winning = first_vote_num,
        margin = margin,
        margin_category = margin_category,
        competitive = margin <= 0.1
      ) |>
      select(district_num, voters_total, winning_party, margin, margin_category, competitive)
  )


districts <- districts |>
  mutate(
    voter_classification = case_when(
      competitive == TRUE & party %in% c("Dem", "Rep") ~ "Meaningful",
      competitive == TRUE & !(party %in% c("Dem", "Rep")) ~ "None-of-the-Above",
      competitive == FALSE & party == winning_party ~ "Unneeded",
      competitive == FALSE & party != winning_party & (party %in% c("Dem", "Rep")) ~ "Outnumbered",
      competitive == FALSE & party != winning_party & !(party %in% c("Dem", "Rep")) ~ "Excluded"
    )
  )


districts |> 
  group_by(voter_classification) |> 
  summarize(
    reg = sum(reg_party)
  ) |>
  mutate(
    reg_pct = reg / sum(reg)
  )


