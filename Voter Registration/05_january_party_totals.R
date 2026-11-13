library(dplyr)
library(readr)
library(stringr)
library(tidyr)

registrations_long <- read_rds("clean_data/registrations_long.rds")

january_party_totals <- registrations_long |>
  filter(
    month == 1,
    name %in% c(
      "democratic_active", "democratic_inactive",
      "republican_active", "republican_inactive",
      "no_party_active", "no_party_inactive",
      "libertarian_active", "libertarian_inactive"
    )
  ) |>
  mutate(
    party = case_when(
      str_starts(name, "democratic")  ~ "Democrat",
      str_starts(name, "republican")  ~ "Republican",
      str_starts(name, "no_party")    ~ "No Party",
      str_starts(name, "libertarian") ~ "Libertarian"
    )
  ) |>
  summarise(reg_voters = sum(value, na.rm = TRUE), .by = c(year, party)) |>
  arrange(year, party) |>
  ungroup() |>
  mutate(party_pct = reg_voters / sum(reg_voters), .by = year)

party_totals_wide <- january_party_totals |>
  pivot_wider(id_cols = party, names_from = year, values_from = reg_voters)


party_pct_wide <- january_party_totals |>
  pivot_wider(id_cols = party, names_from = year, values_from = party_pct)

library(openxlsx2)

wb <- wb_workbook()

wb$add_worksheet(sheet = "Party Reg. Voters")
wb$add_worksheet(sheet = "Party Reg. Voters Pct.")
wb$add_worksheet(sheet = "Reg. Voters Long")

wb$add_data(sheet = "Party Reg. Voters", x = party_totals_wide)
wb$add_data(sheet = "Party Reg. Voters Pct.", x = party_pct_wide)
wb$add_data(sheet = "Reg. Voters Long", x = january_party_totals)

wb_save(wb, file = "clean_data/registered_voters_party.xlsx")

write_csv(january_party_totals, "clean_data/january_party_totals.csv")
