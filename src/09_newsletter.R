
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)



house_boundaries <- st_read("data/district_boundaries/Plan2_House.shp") |>
  mutate(district_num = DISTRICT |> as.numeric())
house_primary_wide <- read_rds("data/election_results/ia_2024_leg_primary_wide.rds") |> 
  filter(
    race_level == "State Representative",
    party %in% c("Dem.", "Rep.")
  ) |>
  mutate(
    no_challengers = (margin_category %in% c("Uncontested", "No Candidates")) |> as.numeric(),
    competitive = (margin <= 0.1) |> as.numeric(),
    competitive = replace_na(competitive, 0)
  )

voters <- read_rds("data/house_district_voter_classification.rds")



# Primary Map -------------------------------------------------------------

district_primaries <- house_primary_wide |>
  group_by(district_num) |>
  summarize(
    no_challengers = sum(no_challengers),
    competitive = sum(competitive)
  ) |>
  mutate(
    classification = case_when(
      competitive == 2 ~ "Two Competitive Primaries (0)",
      competitive == 1 ~ "One Competitive Primary (2)",
      no_challengers == 1 ~ "One Challenged, Uncompetitive Primary (16)",
      no_challengers == 2 ~ "No Challenged Primaries (82)"
    ),
    classification = factor(
      classification,
      levels = c(
        "No Challenged Primaries (82)",
        "One Challenged, Uncompetitive Primary (16)",
        "One Competitive Primary (2)",
        "Two Competitive Primaries (0)"
      )
    )
  )

district_primaries <- left_join(house_boundaries, district_primaries)


color_values = c(
  "No Challenged Primaries (82)" = "darkgrey",
  "One Challenged, Uncompetitive Primary (16)" = "lightgrey",
  "One Competitive Primary (2)" = "#e5bdfc",
  "Two Competitive Primaries (0)" = "#a759d4"
)

ggplot() +
  geom_sf(data = district_primaries, aes(fill = classification), show.legend = TRUE) +
  scale_fill_manual(values = color_values, drop = FALSE) +
  labs(
    title = "Iowa House District Primaries in 2024", 
    subtitle = "Republican and Democratic Primaries\nChallenged: 2 or more candidates\nCompetitive: <10 percentage point margin", fill = "") +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 18, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    legend.margin = margin(r = 10)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  )



# Competitive Primary Pie ------------------------------------http://127.0.0.1:28299/graphics/10be4c5c-f756-485d-867b-5a4aef7f1a55.png-------------

competitive_primaries <- house_primary_wide |>
  filter(margin <= 0.1) |>
  mutate(party = stringr::str_remove(party, ".$"))

prim_pie <- voters |>
  filter(district_num %in% competitive_primaries$district_num) |>
  left_join(
    competitive_primaries |> transmute(
      district_num = district_num, party = party, competitive_primary = TRUE
    )
  ) |>
  group_by(voter_classification) |>
  summarize(
    voters = sum(reg_party)
  ) |>
  mutate(
    voter_pct = voters / sum(voters)
  )

ggplot(data = prim_pie, aes(x = "", y = voters, ))



sum(voters$prim_voters, na.rm = T)
sum()