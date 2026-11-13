# Shared settings for the election pipeline (src/01-07).
# To add an election year, append it to `election_years`, set its
# `results_source`, and re-run 01-05 and 07. For an "sos" year, first drop the
# Secretary of State's detail workbooks into data/election_results/ as
# IA_primary_<year>_detail.xlsx and IA_general_<year>_detail.xlsx. Steps skip
# any year whose inputs aren't there yet.

election_years <- c(2024, 2026)

# Where each year's results come from:
#   "sos": the SoS statewide detail workbooks, read by 04 and cleaned by 05
#   "race_table": ia_transparency_project's race table (checked out next to
#     this repository), converted by 04b. For years with no statewide
#     workbook, where the state's results site has only per-county files
results_source <- c("2024" = "sos", "2026" = "race_table")
race_table_path <- file.path("..", "ia_transparency_project", "data", "elections", "processed", "legislative_results.rds")

sos_years <- function() election_years[results_source[as.character(election_years)] == "sos"]
race_table_years <- function() election_years[results_source[as.character(election_years)] == "race_table"]

# Month of the registration snapshot paired with each general election
# (November, the month of the election)
reg_month <- 11

# Chambers to classify, and how the SoS names each one's contests
chambers <- c(
  "House" = "State Representative",
  "Senate" = "State Senator"
)

# Iowa nests two House districts in each Senate district: SD n = HD 2n-1 + HD 2n
# (checked against the Plan2 shapefiles), so Senate registration is summed from
# House registration rather than scraped separately
house_to_senate <- function(house_district) ceiling(house_district / 2)

results_path <- function(type, year, suffix){
  file.path("data", "election_results", paste0("IA_", type, "_", year, "_", suffix))
}
leg_results_path <- function(type, year, shape){
  file.path("data", "election_results", paste0("ia_", year, "_leg_", type, "_", shape, ".rds"))
}
house_reg_path <- function(year){
  file.path("data", "house_district_reg", paste0("ia_", year, "_house_reg.rds"))
}
