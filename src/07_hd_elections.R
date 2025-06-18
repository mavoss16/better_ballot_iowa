
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
      mutate(party = str_remove(party, "\\."), gen_party = value, gen_vote_pct = vote_pct) |>
      select(district_num, party, candidate, gen_party, gen_vote_pct)
  ) |>
  left_join(
    house_general_wide |> 
      ungroup() |>
      mutate(
        gen_winning_party = first_vote_party |> str_remove("\\."),
        gen_total = total_voters,
        gen_margin = margin,
        gen_margin_category = margin_category,
        gen_competitive = margin <= 0.1
      ) |>
      select(district_num, gen_total, gen_winning_party, gen_margin, gen_margin_category, gen_competitive)
  ) |>
  left_join(
    house_primary_wide |>
      ungroup() |>
      mutate(
        party = party |> str_remove("\\."),
        prim_voters = total_voters,
        prim_margin = margin,
        prim_margin_category = margin_category
      ) |>
      select(district_num, party, prim_voters, prim_margin, prim_margin_category)
  )


districts <- districts |>
  mutate(
    voter_classification = case_when(
      gen_competitive == TRUE & party %in% c("Dem", "Rep") ~ "Meaningful (General)",
      gen_competitive == FALSE & party == gen_winning_party & prim_margin_category != "Uncontested" ~ "Meaningful (Primary)",
      gen_competitive == TRUE & !(party %in% c("Dem", "Rep")) ~ "None-of-the-Above",
      gen_competitive == FALSE & party == gen_winning_party ~ "Unneeded",
      gen_competitive == FALSE & party != gen_winning_party & (party %in% c("Dem", "Rep")) ~ "Outnumbered",
      gen_competitive == FALSE & party != gen_winning_party & !(party %in% c("Dem", "Rep")) ~ "Excluded"
    )
  )


districts |> 
  group_by(voter_classification) |> 
  summarize(
    reg = sum(reg_party)
  ) |>
  mutate(
    reg_pct = (reg / sum(reg)) * 100
  ) |>
  arrange(desc(reg_pct))


districts <- districts |>
  select(
    state_representative_district, district_num, party, voter_classification,
    reg_total, reg_party, 
    candidate, gen_total, gen_party, gen_vote_pct, gen_winning_party, gen_margin, gen_margin_category, gen_competitive,
    prim_voters, prim_margin, prim_margin_category
  )


write_csv(districts, "data/house_district_voter_classification.csv")
write_rds(districts, "data/house_district_voter_classification.rds")
