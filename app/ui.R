#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinyWidgets)

library(readxl)
library(readr)
library(leaflet)
library(ggplot2)
library(plotly)
library(reactable)

var_info <- read_excel("data/var_info.xlsx")
county_sf <- read_rds("data/ia_counties_sf.rds")

vars <- as.list(var_info$name)
names(vars) <- var_info$full_name

page_sidebar(
  includeCSS("www/styles.css"),
  
  fillable = FALSE,
  title = "Iowa Active Voter and Voter Registration Data",
  
  sidebar = sidebar(
    width = 300,
    h5("Inputs"),
    pickerInput(
      inputId = "variable_input", label = "Select a Variable", 
      # choices = c("active_pct", "reg_pct")
      choices = vars, options = pickerOptions(container = "body")
    ),
    br(),
    p("Select a Date for the Map Variable"),
    layout_columns(
      col_widths = c(6, 6),
      pickerInput(
        inputId = "year_input", label = "Year", width = "120px",
        choices = 2024, options = pickerOptions(container = "body")
      ),
      pickerInput(
        inputId = "month_input", label = "Month", width = "120px",
        choices = 1, options = pickerOptions(container = "body")
      )
    ),
    br(),
    pickerInput(
      "county_selection", label = "Select Counties to Highlight",
      choices = county_sf$NAME |> sort(), multiple = TRUE, 
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, container = "body")
    ),
    br(),
    pickerInput(
      "county_filter", label = "Select Counties to Filter to",
      choices = county_sf$NAME |> sort(), multiple = TRUE, 
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, container = "body")
    )
  ),
  br(),
  layout_columns(
    col_widths = c(-1, 10, -1),
    plotlyOutput("var_trend"),
    br(),
    leafletOutput("var_map"),
    br(),
    reactableOutput("table")
  )
)