

library(readr)
library(dplyr)
library(stringr)


primary_results <- read_rds("data/election_results/IA_primary_2024_clean_long.rds")
general_results <- read_rds("data/election_results/IA_general_2024_clean_long.rds")

primary_results <- primary_results |>
  mutate(
    candidate = ifelse(candidate == "", NA_character_, candidate),
    race_level = str_remove_all(contest, " District \\d+ .+$"),
    district_num = str_extract(contest, " District \\d+") |> str_remove_all(" District") |> as.numeric(),
    party = str_extract(contest, "Rep\\.|Dem\\.|Lib\\.")
  ) |>
  relocate(
    race_level, district_num, party, .after = contest
  )

candidate_parties <- primary_results |>
  distinct(race_level, district_num, candidate, party) |>
  filter(!is.na(candidate) & candidate != "Write-in")


general_results <- general_results |>
  mutate(
    candidate = ifelse(candidate == "", NA_character_, candidate),
    race_level = str_remove_all(contest, " District \\d+ .+$"),
    district_num = str_extract(contest, " District \\d+") |> str_remove_all(" District") |> as.numeric()
  ) |>
  relocate(
    race_level, district_num, .after = contest
  ) |>
  left_join(candidate_parties, by = c("race_level", "district_num", "candidate"))


missing_parties <- general_results |>
  filter(!is.na(candidate) & candidate != "Write-in" & is.na(party)) |>
  distinct(race_level, district_num, candidate, party)
