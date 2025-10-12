
library(readr)
library(dplyr)
library(ggplot2)
library(sf)



house_boundaries <- st_read("data/district_boundaries/Plan2_House.shp") |>
  mutate(district_num = DISTRICT |> as.numeric())
house_general_wide <- read_rds("data/election_results/ia_2024_leg_general_wide.rds") |> 
  filter(race_level == "State Representative")


house_general_wide <- left_join(house_boundaries |> select(district_num, geometry), house_general_wide) |>
  mutate(
    competitive = case_when(
      margin <= 0.10 ~ "Competitive",
      margin > 0.10 ~ "Uncompetitive"
    )
  )

ggplot() +
  geom_sf(data = house_general_wide, aes(fill = competitive)) +
  scale_fill_manual(values = c("Competitive" = "#a759d4", "Uncompetitive" = "lightgrey")) +
  labs(title = "Competitive Iowa House Races in 2024", subtitle = "<10 percentage point margin", fill = "") +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 18, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "bottom",
    legend.margin = margin(r = 10)
  )
