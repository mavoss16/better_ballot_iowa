
library(readr)
library(dplyr)
library(ggplot2)

# Cascading categories:
  # Pct of adults that aren't registered
  # Pct of adults that are registered but didn't vote in general
  # Pct of adults that voted in uncompetetive general
  # Pct of adults that 


house_primary_wide <- read_rds("data/election_results/ia_2024_leg_primary_wide.rds") |> 
  filter(race_level == "State Representative")
house_general_wide <- read_rds("data/election_results/ia_2024_leg_general_wide.rds") |> 
  filter(race_level == "State Representative")
registration <- read_rds("data/house_district_reg/ia_2024_house_reg.rds")





registration <- registration |>
  mutate(
    party_supermajority = case_when(
      democratic_active >= republican_active + no_party_active + other_active ~ "D",
      republican_active >= democratic_active + no_party_active + other_active ~ "R",
      TRUE ~ NA_character_
    )
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
  ggplot(aes(x = margin_category, y = voter_pct)) +
  geom_col() +
  theme_minimal()
