# Shared settings for the election pipeline (src/01-07).
# To add an election year, append it to `election_years`, drop the Secretary of
# State's detail workbooks into data/election_results/ as
# IA_primary_<year>_detail.xlsx and IA_general_<year>_detail.xlsx, and re-run
# 01-05 and 07. Steps skip any year whose inputs aren't there yet.

election_years <- c(2024, 2026)

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
