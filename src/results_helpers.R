# Helpers shared by 05_election_results_clean.R and 04b_results_from_race_table.R
# (sourced after 00_config.R)

library(dplyr)
library(stringr)


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


# ia_transparency_project race table -> this pipeline's results ------------
# The race table (one row per candidate per race, statewide totals) is built
# from the per-county precinct files the state's results site publishes, for
# elections with no statewide SoS detail workbook. It already merges name
# spellings, fills general election parties from the primary, and flags
# write-ins that a county reported by name.

# Party labels as the SoS workbooks abbreviate them
abbreviate_party <- function(party){
  case_when(
    party == "Republican" ~ "Rep.",
    party == "Democratic" ~ "Dem.",
    party == "Libertarian" ~ "Lib.",
    party %in% c("Independent", "Nominated by Petition") ~ "Ind.",
    TRUE ~ party
  )
}

# Long results for one election, in the layout 05 writes: one "Total:" row per
# candidate, grouped by contest. Named write-ins become "Write-in", as the race
# table counts them, so margins and uncontested races match its reading
race_table_long <- function(race_table, election_year, election_type){
  races <- race_table |>
    filter(year == election_year, type == election_type, chamber %in% str_to_lower(names(chambers)))

  if(nrow(races) == 0) return(NULL)

  races |>
    mutate(
      race_level = unname(chambers[str_to_title(chamber)]),
      district_num = as.numeric(district),
      party = abbreviate_party(if(election_type == "primary") primary_party else party),
      # (A named write-in who won keeps the name)
      write_in = write_in & !winner,
      party = ifelse(election_type == "general" & (candidate == "Write-in" | write_in), NA_character_, party),
      candidate = ifelse(write_in, "Write-in", candidate),
      contest = paste0(
        race_level, " District ", district_num,
        if(election_type == "primary") paste0(" - ", party) else "",
        " ( 1)"
      ),
      precinct = "Total:",
      vote_type = "Total Votes"
    ) |>
    # Merge the "Write-in" tally with any write-ins reported by name
    group_by(contest, race_level, district_num, party, precinct, candidate, vote_type) |>
    summarize(value = sum(votes), .groups = "drop") |>
    group_by(contest) |>
    mutate(vote_pct = value / sum(value)) |>
    select(contest, race_level, district_num, any_of("party"), precinct, candidate, vote_type, value, vote_pct)
}
