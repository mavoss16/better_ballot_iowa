
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


# Read Data --------------------------------------------------------------

reg_long <- read_rds("clean_data/registrations_long.rds")


# Aggregate Statewide Totals by Month ------------------------------------

party_totals <- reg_long |>
  filter(name %in% c(
    "democratic_active", "democratic_inactive",
    "republican_active", "republican_inactive",
    "no_party_active",   "no_party_inactive",
    "other_active", "other_inactive"
  )) |>
  mutate(
    party = case_when(
      str_starts(name, "democratic") ~ "Democrat",
      str_starts(name, "republican") ~ "Republican",
      str_starts(name, "no_party")   ~ "No Party",
      str_starts(name, "other")   ~ "Other"
    )
  ) |>
  group_by(year_month, party) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    date  = ym(year_month),
    party = factor(party, levels = c("Republican", "No Party", "Democrat", "Other"))
  ) |>
  ungroup() |>
  mutate(pct = total / sum(total), .by = year_month)


# 100% Stacked Area Chart ------------------------------------------------

ggplot(party_totals, aes(x = date, y = total, fill = party)) +
  geom_area(position = "fill") +
  scale_fill_manual(
    values = c(
      "Democrat"   = "#1A6BB5",
      "No Party"   = "#B0B0B0",
      "Republican" = "#C0373A",
      "Other" = "yellow"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  labs(
    title    = "Iowa Voter Registration Composition Over Time",
    subtitle = "Share of active Democratic, No Party, and Republican registrations",
    x        = NULL,
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title           = element_text(size = 16, face = "bold"),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(size = 11, color = "grey40"),
    legend.position      = "bottom",
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.text.x          = element_text(angle = 45, hjust = 1)
  )


# Line Graph of Registration Percentages ---------------------------------

ggplot(party_totals |> filter(party != "Other"), aes(x = date, y = pct, color = party)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c(
      "Democrat"   = "#1A6BB5",
      "No Party"   = "#B0B0B0",
      "Republican" = "#C0373A"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.25, 0.42)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = c(0.01, 0)
  ) +
  labs(
    title    = "Iowa Voter Registration Share Over Time",
    subtitle = "Percentage of total Democrat, No Party, and Republican registrations",
    x        = NULL,
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title           = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(size = 11, color = "grey40", hjust = 0.5),
    legend.position      = "bottom",
    panel.grid.minor     = element_blank(),
    axis.text.x          = element_text(angle = 45, hjust = 1)
  )


# Pie Chart of Current Registration Composition --------------------------

latest_month <- max(party_totals$year_month)

pie_data <- party_totals |>
  filter(year_month == latest_month, party != "Other") |>
  arrange(desc(total)) |>
  mutate(
    label = paste0(party, "\n", scales::percent(pct, accuracy = 0.1))
  )

ggplot(pie_data, aes(x = "", y = pct, fill = party)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Democrat"   = "#1A6BB5",
      "No Party"   = "#B0B0B0",
      "Republican" = "#C0373A"
    )
  ) +
  labs(
    title    = "Iowa Voter Registration Composition",
    subtitle = paste0("Share of Republican, No Party, and Democrat registrations — March 2026"),
    fill     = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title          = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle       = element_text(size = 11, color = "grey40", hjust = 0.5),
    legend.position     = "none"
  )
