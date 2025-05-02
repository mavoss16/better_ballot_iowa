#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(readxl)

library(dplyr)
library(tools)
library(lubridate)
library(stringr)

library(sf)
library(leaflet)

library(ggplot2)
library(plotly)

library(gt)
library(reactable)

var_info <- read_excel("data/var_info.xlsx")

sf_data <- read_rds("data/registrations_sf_summary.rds")

county_sf <- read_rds("data/ia_counties_sf.rds")

long_data <- read_rds("data/registrations_long.rds") |>
  left_join(var_info) |>
  mutate(
    year_month = ym(year_month),
    value = ifelse(str_detect(name, "pct"), yes = (value * 100) |> round(digits = 2), no = value),
    label = paste0("<strong>", county, " County<br>", full_name, ": </strong>", value, ifelse(str_detect(name, "pct"), yes = "%", no = ""))
  )


name_replace <- function(name){

  str_replace_all(name, "_", " ") |>
    toTitleCase() |>
    str_replace_all("pct", "%") |>
    str_replace_all("Dem ", "Dem - ") |>
    str_replace_all("Rep ", "Rep - ") |>
    str_replace_all("Lib ", "Lib - ") |>
    str_replace_all("No Party ", "No Party - ")
  
}


# Define server logic required to draw a histogram
function(input, output, session) {
  
  filtered_var_data <- reactive({
    
    if(!is.null(input$county_filter)){
      # Selection is made in County Filter
      long_data |>
        filter(
          name == input$variable_input,
          county %in% input$county_filter,
          !is.na(value)
        )
    } else{
      # Selection is not made in County Filter
      long_data |>
        filter(
          name == input$variable_input,
          !is.na(value)
        )
    }
  })

  observeEvent(input$variable_input, {
    # req(filtered_var_data())
    # req(nrow(filtered_var_data()) > 0)

    print(unique(filtered_var_data()$year))
    
    selected_val <- input$year_input

    if(nrow(filtered_date_data()) == 0){
      updatePickerInput(
        inputId = "year_input",
        choices = unique(filtered_var_data()$year), selected = max(unique(filtered_var_data()$year))
      )
    } else{
      updatePickerInput(
        inputId = "year_input",
        choices = unique(filtered_var_data()$year), selected = selected_val
      )
    }

  })

  observeEvent(input$year_input, {
    req(filtered_var_data())

    month_choices <- filtered_var_data() |>
      filter(year == input$year_input) |>
      pull(month) |>
      unique()

    updatePickerInput(
      inputId = "month_input",
      choices = month_choices, selected = max(month_choices)
    )
  })
  
  filtered_date_data <- reactive({
    
    req(filtered_var_data())
    filtered_var_data() |>
      filter(
        year == input$year_input,
        month == input$month_input
      )
  })
  
  output$table <- renderReactable({
    
    table_data <- sf_data |> 
      st_drop_geometry() |>
      select(NAME, contains("pct"), contains("diff")) |>
      mutate(
        across(c(contains("pct"), contains("diff")), function(.x){round(.x * 100, digits = 1)})
      ) |>
      rename_with(.fn = name_replace)
    
    reactable(
      table_data,
      filterable = TRUE, resizable = TRUE, 
      showPageSizeOptions = TRUE, pageSizeOptions = c(10, 25, 50, 99),
      columns = list(
        NAME = colDef(
          name = "County",
          format = colFormat(suffix = ""),
          minWidth = 115
        )
      ),
      defaultColDef = colDef(
        format = colFormat(suffix = "%")
      )
    )
    
  })
  
  map_data <- reactive({
    req(nrow(filtered_date_data()) == 99)
    map_data <- left_join(county_sf, filtered_date_data(), by = c("NAME" = "county"))
  })

  output$var_map <- renderLeaflet({
    
    leaflet() |>
      addTiles() |>
      setView(
        lng = -93.62463629705638, lat = 41.99232086345729,
        zoom = 7
      )
    
  })
  
  observe({
    req(map_data())
    
    vir_pal <- colorNumeric("Blues", domain = range(map_data()$value, na.rm = T))
    
    leafletProxy("var_map") |>
      clearControls() |>
      clearGroup("county_map") |>
      addPolygons(
        data = map_data(), fillColor = ~vir_pal(value),
        label = ~lapply(label, HTML),
        fillOpacity = 0.75,
        color = "black", weight = 0.5,
        group = "county_map"
      ) |>
      addLegend(
        pal = vir_pal, values = map_data()$value,
        position = "bottomright", opacity = 0.9,
        title = var_info[var_info$name == input$variable_input, "full_name"] |> pull(),
        labFormat = labelFormat(suffix = ifelse(str_detect(input$variable_input, "pct|%"), yes = "%", no = "")),
        group = "county_map"
      )
    
  })

  observe({
    leafletProxy("var_map") |>
      clearGroup("county_selection") |>
      addPolygons(
        data = map_data() |> filter(NAME %in% input$county_selection),
        fillOpacity = 0, opacity = 0.9, color = "yellow", weight = 2,
        group = "county_selection"
      )
  })

  
  output$var_trend <- renderPlotly({
    # print(input$variable_input)
    label_y <- function(input){
      if(str_detect(input, "pct")){
        return(scales::label_percent(scale = 1))
      } else{
        return(scales::label_comma())
      }
    }
    
    plot <- filtered_var_data() |>
      mutate(
        selected = as.character(county %in% input$county_selection)
      ) |>
      ggplot(aes(x = year_month, y = value, text = str_remove_all(label, "<strong>|</strong>"), color = selected, linewidth = selected, group = county)) +
      geom_line() +
      # geom_line(
      #   data = filtered_var_data() |> filter(!(county %in% input$county_selection)),
      #   aes(x = year_month, y = value, group = county),
      #   color = "black"
      # ) +
      # geom_line(
      #   data = filtered_var_data() |> filter(county %in% input$county_selection),
      #   aes(x = year_month, y = value, group = county),
      #   color = "#0f74bc", linewidth = 1.5
      # ) +
      scale_color_manual(values = c("TRUE" = "#0f74bc", "FALSE" = "black")) +
      scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.4)) +
      scale_y_continuous(labels = label_y(input$variable_input)) +
      scale_x_date(date_breaks = "year", labels = scales::label_date(format = "%Y")) +
      labs(x = "Date", y = var_info[var_info$name == input$variable_input, "full_name"] |> pull()) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
    
    # plot
    # plot <- ggplotly(plot) |>
    #   layout(dragmode = "lasso") |>
    #   # config(
    #   #   displayModeBar = TRUE,
    #   #   modeBarButtonsToAdd = list("select"),
    #   #   modeBarButtonsToRemove = list("hoverClosestCartesian", "hoverCompareCartesian")
    #   # ) |>
    #   event_register("plotly_selected")
    
    ggplotly(p = plot, tooltip = "text") |>
      config(displayModeBar = FALSE)

  })

}
