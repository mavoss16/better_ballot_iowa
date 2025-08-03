

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)

library(readr)
library(dplyr)

legislators <- read_csv("data/house_legislators.csv") |>
  arrange(district_num) |>
  mutate(name_num = paste0(name, " (", district_num, ")"))

page_fluid(
  includeCSS("www/styles.css"),
  shinyjs::useShinyjs(),
  br(),
  br(),
  layout_columns(
    col_widths = c(6, 6),
    column(
      width = 12,
      pickerInput(
        inputId = "legislator_input", 
        label = "Select Your House Representative",
        choices = legislators$name_num,
        selected = NULL,
        options = pickerOptions(liveSearch = TRUE)
      ),
      h4("OR"),
      searchInput(
        inputId = "address_input",
        label = "Search for Your Address",
        btnSearch = icon("magnifying-glass")
      ),
      tags$p("Address could not be found.", id = "search_warning", style = "color: red") |> shinyjs::hidden()
    ),
    pickerInput(
      inputId = "party_input",
      label = "Select your party registration",
      choices = c(
        "No Party", "Republican" = "Rep", "Democratic" = "Dem", "Libertarian" = "Lib", "Other"
      ),
      selected = NULL
    )
  ),
  
  # textOutput("district_info"),
  # textOutput("voter_class"),
  uiOutput("voter_class_ui"),
  br(),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "Competitiveness Grade", value = textOutput("grade")),
    value_box(title = "General Margin", value = textOutput("general_margin")),
    value_box(title = "Primary Margin", value = textOutput("primary_margin"))
  )
)
