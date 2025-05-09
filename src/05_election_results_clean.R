

library(readr)
library(dplyr)
library(stringr)


primary_results <- read_rds("data/election_results/IA_primary_2024_clean_long.rds")
general_results <- read_rds("data/election_results/IA_general_2024_clean_long.rds")

primary_results <- primary_results |>
  mutate(
    value = as.numeric(value),
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
    value = as.numeric(value),
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

write_csv(missing_parties, "data/election_results/candidate_missing_parties.csv")
filled_parties <- read_csv("data/election_results/candidate_filled_parties.csv")

general_results <- 


ia_leg_primary <- primary_results |>
  filter(
    race_level %in% c("State Senator", "State Representative"),
    precinct == "Total:",
    vote_type == "Total Votes"
  ) |>
  group_by(contest) |>
  mutate(vote_pct = value / sum(value))
  


ia_leg_general <- general_results |>
  filter(
    race_level %in% c("State Senator", "State Representative"),
    precinct == "Total:",
    vote_type == "Total Votes"
  ) |>
  group_by(contest) |>
  mutate(vote_pct = value / sum(value))


if_exists <- function(value){
  ifelse(length(value) == 0 | is.na(value) | is.null(value), yes = NA_character_, no = value)
}

primary_wide <- ia_leg_primary |>
  group_by(contest, race_level, district_num, party) |>
  summarize(
    total_voters = sum(value),
    first_vote_num = sort(value, TRUE)[1],
    second_vote_num = sort(value, TRUE)[2],
    first_vote_pct = sort(vote_pct, TRUE)[1],
    second_vote_pct = sort(vote_pct, TRUE)[2],
    first_vote_cand = candidate[which(value == first_vote_num)],
    second_vote_cand = ifelse(!is.na(second_vote_pct), yes = candidate[which(value == second_vote_num)], no = NA_character_)
  ) |>
  mutate(
    margin = first_vote_pct - second_vote_pct,
    margin_category = case_when(
      second_vote_cand == "Write-in" ~ "Uncontested",
      first_vote_cand == "Write-in" ~ "No Candidates",
      margin <= 0.05 ~ "Less than 5%",
      margin <= 0.10 ~ "5-10%",
      margin <= 0.15 ~ "10-15%",
      margin <= 0.2 ~ "15-20%",
      margin > 0.2 ~ "20% or more"
    ) |> factor(levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more", "Uncontested", "No Candidates"))
  )

general_wide <- ia_leg_general |>
  group_by(contest, race_level, district_num) |>
  summarize(
    total_voters = sum(value),
    first_vote_num = sort(value, TRUE)[1],
    second_vote_num = sort(value, TRUE)[2],
    first_vote_pct = sort(vote_pct, TRUE)[1],
    second_vote_pct = sort(vote_pct, TRUE)[2],
    first_vote_cand = candidate[which(value == first_vote_num)],
    first_vote_party = party[which(value == first_vote_num)],
    second_vote_cand = ifelse(!is.na(second_vote_pct), yes = candidate[which(value == second_vote_num)], no = NA_character_),
    second_vote_party = ifelse(!is.na(second_vote_pct), yes = party[which(value == second_vote_num)], no = NA_character_)
  ) |>
  mutate(
    margin = first_vote_pct - second_vote_pct,
    margin_category = case_when(
      second_vote_cand == "Write-in" ~ "Uncontested",
      first_vote_cand == "Write-in" ~ "No Candidates",
      margin <= 0.05 ~ "Less than 5%",
      margin <= 0.10 ~ "5-10%",
      margin <= 0.15 ~ "10-15%",
      margin <= 0.2 ~ "15-20%",
      margin > 0.2 ~ "20% or more"
    ) |> factor(levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more", "Uncontested", "No Candidates"))
  )


write_rds(ia_leg_primary, "data/election_results/ia_2024_leg_primary_long.rds")
write_rds(primary_wide, "data/election_results/ia_2024_leg_primary_wide.rds")

write_rds(ia_leg_general, "data/election_results/ia_2024_leg_general_long.rds")
write_rds(general_wide, "data/election_results/ia_2024_leg_general_wide.rds")