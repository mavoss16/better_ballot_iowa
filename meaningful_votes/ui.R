

library(shiny)
library(bslib)
library(shinyWidgets)

page_fluid(
  title = "BBI Meaningful Votes",

  pickerInput(
    "district_input", label = "Pick an Iowa House District:",
    choices = 1:100
  ),

  h4("In House District XX, just XX% of votes are meaningful"),
  layout_columns(
    col_widths = c(6, 6),
    plotOutput("registration_plot"),
    plotOutput("general_plot")
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    plotOutput("rep_primary_plot"),
    plotOutput("dem_primary_plot"),
    plotOutput("lib_primary_plot")
  )

)


