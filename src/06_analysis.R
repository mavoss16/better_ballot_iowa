
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggfittext)
library(ggplot2)

# Cascading categories:
  # Pct of adults that aren't registered
  # Pct of adults that are registered but didn't vote in general
  # Pct of adults that voted in uncompetetive general
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





registration <- registration |>
  mutate(
    district_num = str_extract(state_representative_district, "\\d+$") |> as.numeric(),
    party_supermajority = case_when(
      dem_total >= rep_total + no_party_total + lib_total + other_total ~ "D",
      rep_total >= dem_total + no_party_total + lib_total + other_total ~ "R",
      TRUE ~ NA_character_
    ),
    .after = state_representative_district
  )


house_general_wide |>
  group_by(margin_category) |>
  summarize(race_count = n(), total_voters = sum(total_voters)) |>
  ungroup() |>
  mutate(voter_pct = total_voters / sum(total_voters)) |>
  ggplot(aes(x = margin_category, y = voter_pct, label = voter_pct |> round(digits = 2))) +
  geom_col() +
  geom_text() +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal()



house_primary_wide |>
  group_by(margin_category) |>
  summarize(race_count = n(), total_voters = sum(total_voters)) |>
  ungroup() |>
  mutate(voter_pct = total_voters / sum(total_voters)) |>
  ggplot(aes(fill = margin_category, y = voter_pct, x = 1)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal()


house_general_long |>
  filter(
    district_num == 70
  ) |>
  ggplot(aes(y = vote_pct, x = 1, fill = candidate)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal()


reg_long <- registration |>
  select(state_representative_district, district_num, party_supermajority, ends_with("total")) |>
  pivot_longer(
    cols = c(dem_total, rep_total, no_party_total, lib_total, other_total),
    names_to = "party",
    values_to = "num_reg"
  ) |>
  mutate(
    reg_pct = num_reg / grand_total,
    party = case_when(
      party == "dem_total" ~ "Democratic",
      party == "rep_total" ~ "Republican",
      party == "lib_total" ~ "Libertarian",
      party == "no_party_total" ~ "No Party",
      party == "other_total" ~ "Other Party"
    ),
    label = paste0(party, ": ", (reg_pct * 100) |> round(1), "%")
  )

write_rds(reg_long, "data/house_district_reg/ia_2024_house_reg_long.rds")

party_pal <- c(
  "Democratic" = "darkblue",
  "Republican" = "darkred",
  "No Party" = "darkgrey",
  "Libertarian" = "yellow",
  "Other Party" = "darkgreen"
)

reg_long |>
  filter(
    district_num == 50
  ) |>
  ggplot(aes(y = reg_pct, label = label, x = 1, fill = party)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = party_pal) +
  geom_bar_text(position = "stack", place = "center") +
  theme_void() +
  theme(
    legend.position = "none"
  )

