
library(readr)
library(dplyr)
library(stringr)

source("src/00_config.R")
source("src/results_helpers.R")

# For each "race_table" year in 00_config.R, writes the same
# ia_<year>_leg_<primary|general>_<long|wide>.rds files 05 writes for SoS
# workbook years, from ia_transparency_project's race table. Rebuild that
# table there first (R/elections/legislative_results/01-03) after downloading
# new results. Elections not in the table yet (e.g. a general still being
# counted) are skipped.

race_table <- read_rds(race_table_path)

for(election_year in race_table_years()){
  for(election_type in c("primary", "general")){
    long <- race_table_long(race_table, election_year, election_type)

    if(is.null(long)){
      message("No ", election_year, " ", election_type, " in the race table yet; skipping")
      next
    }

    wide <- if(election_type == "primary"){
      summarize_margins(long, party) |>
        select(-first_vote_party, -second_vote_party)
    } else {
      summarize_margins(long)
    }

    write_rds(long, leg_results_path(election_type, election_year, "long"))
    write_rds(wide, leg_results_path(election_type, election_year, "wide"))
    message("Wrote ", election_year, " ", election_type, ": ", n_distinct(long$contest), " contests")
  }
}
