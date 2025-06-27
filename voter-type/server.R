
library(shiny)

library(readr)
library(dplyr)
library(stringr)
library(rvest)

library(tidygeocoder)
library(sf)


house_voters <- read_rds("data/house_district_voter_classification.rds")
house_boundaries <- st_read("data/district_boundaries/Plan2_House.shp")
house_results <- read_rds("data/ia_2024_leg_general_wide.rds") |> filter(race_level == "State Representative")
voter_type_text <- read_csv("data/voter_type_text.csv")

legislators <- read_csv("data/house_legislators.csv") |>
  arrange(district_num) |>
  mutate(name_num = paste0(name, " (", district_num, ")"))

percent <- scales::label_percent(accuracy = 0.1)

# Define server logic required to draw a histogram
function(input, output, session) {

  search_district <- reactive({
    if(input$address_input != ""){
      district_num <- geo(address = input$address_input, method = "osm") |>
        st_as_sf(coords = c("long", "lat"), crs = 4269, na.fail = FALSE) |>
        st_join(house_boundaries) |>
        pull(DISTRICT_N)
      # print(district_num)
      return(district_num)
      
    } else{
      return(NA)
    }
    
  }) |>
    bindEvent(input$address_input_search)
  
  observe({
    updateSearchInput(
      session = getDefaultReactiveDomain(),
      inputId = "address_input", value = ""
    )
  }) |>
    bindEvent(input$legislator_input)
  
  observe({
    if(is.na(search_district())){
      show(id = "search_warning")
    } else{
      hide(id = "search_warning")
      new_legislator <- legislators |>
        filter(district_num == search_district()) |>
        pull(name_num)
      updatePickerInput(inputId = "legislator_input", selected = new_legislator)
    }
  }) |>
    bindEvent(search_district())
  
  voter_type <- reactive({
    house_voters |>
      filter(
        district_num == str_extract(input$legislator_input, "\\d+"),
        party == input$party_input
      )
  })
  
  election <- reactive({
    house_results |>
      filter(district_num == str_extract(input$legislator_input, "\\d+"))
  })
  
  # output$district_info <- renderText({
  #   paste0("District: ", search_district())
  # })

  # output$voter_class <- renderText({
  #   voter_type()$voter_classification
  # })
  
  
  output$voter_class <- renderText({
    voter_type()$voter_classification
  })
  
  output$voter_class_ui <- renderUI({
    
    page_fluid(
      tags$h3(paste0("Your voter type is ", voter_type()$voter_classification)),
      tags$div(paste0("In your Iowa House district's November election, ", election()$first_vote_cand, " won with a margin of ", percent(election()$margin), ".")),
      br(),
      tags$div(
        voter_type_text |> filter(voter_classification == voter_type()$voter_classification) |> pull(explanation_text)
      )
    )
  })

  session$onSessionEnded(function() { stopApp() }) 
}
