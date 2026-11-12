

library(readr)
library(dplyr)
library(stringr)

source("src/00_config.R")


# Margin between the top two candidates in each contest, bucketed
summarize_margins <- function(results, ...){
  results |>
    group_by(contest, race_level, district_num, ...) |>
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
}


# Legislative totals for one election year ---------------------------------
# Writes ia_<year>_leg_<primary|general>_<long|wide>.rds. General election
# candidates get their party from the primary; any who didn't run in one
# (e.g. nominated by convention) must be filled in by hand in
# candidate_filled_parties_<year>.csv

clean_election_year <- function(election_year){
  primary_path <- results_path("primary", election_year, "clean_long.rds")
  general_path <- results_path("general", election_year, "clean_long.rds")
  if(!file.exists(primary_path) || !file.exists(general_path)){
    message("Results for ", election_year, " not read yet (04_election_results_read.R); skipping")
    return(invisible(NULL))
  }

  primary_results <- read_rds(primary_path) |>
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

  general_results <- read_rds(general_path) |>
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

  missing_path <- file.path("data", "election_results", paste0("candidate_missing_parties_", election_year, ".csv"))
  filled_path <- file.path("data", "election_results", paste0("candidate_filled_parties_", election_year, ".csv"))
  write_csv(missing_parties, missing_path)

  if(nrow(missing_parties) > 0){
    if(!file.exists(filled_path)){
      message(
        nrow(missing_parties), " general election candidates in ", election_year, " have no party. ",
        "Copy ", missing_path, " to ", filled_path, ", fill in the party column (e.g. Rep., Dem.), and re-run. ",
        "Skipping ", election_year
      )
      return(invisible(NULL))
    }

    filled_parties <- read_csv(filled_path, show_col_types = FALSE) |>
      select(race_level, district_num, candidate, party_new = party)

    general_results <- left_join(general_results, filled_parties, by = c("race_level", "district_num", "candidate")) |>
      mutate(
        party = ifelse(is.na(party), no = party, yes = party_new)
      ) |>
      select(-party_new)
  }


  ia_leg_primary <- primary_results |>
    filter(
      race_level %in% chambers,
      precinct == "Total:",
      vote_type == "Total Votes"
    ) |>
    group_by(contest) |>
    mutate(vote_pct = value / sum(value))

  ia_leg_general <- general_results |>
    filter(
      race_level %in% chambers,
      precinct == "Total:",
      vote_type == "Total Votes"
    ) |>
    group_by(contest) |>
    mutate(vote_pct = value / sum(value))

  # Primary contests are already split by party, so the wide file keeps its
  # original columns (no winner's party)
  primary_wide <- summarize_margins(ia_leg_primary, party) |>
    select(-first_vote_party, -second_vote_party)

  general_wide <- summarize_margins(ia_leg_general)


  write_rds(ia_leg_primary, leg_results_path("primary", election_year, "long"))
  write_rds(primary_wide, leg_results_path("primary", election_year, "wide"))

  write_rds(ia_leg_general, leg_results_path("general", election_year, "long"))
  write_rds(general_wide, leg_results_path("general", election_year, "wide"))
}


for(election_year in election_years){
  clean_election_year(election_year)
}
