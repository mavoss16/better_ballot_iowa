
library(shiny)
library(bslib)

library(readr)

library(dplyr)
library(forcats)

library(ggplot2)
library(ggfittext)
library(gt)


house_primary_long <- read_rds("data/ia_2024_leg_primary_long.rds") |> 
  filter(race_level == "State Representative")
house_general_long <- read_rds("data/ia_2024_leg_general_long.rds") |> 
  filter(race_level == "State Representative")
registration_long <- read_rds("data/ia_2024_house_reg_long.rds")

party_vote_pal <- c(
  "Dem." = "darkblue",
  "Rep." = "darkred",
  "Lib." = "yellow",
  "Ind." = "darkgrey"
)

party_reg_pal <- c(
  "Democratic" = "darkblue",
  "Republican" = "darkred",
  "No Party" = "darkgrey",
  "Libertarian" = "yellow",
  "Other Party" = "darkgreen"
)


theme_bbi <- function(){
  
  theme_minimal() %+replace%
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.title.position = "plot",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
  
}


# Define server logic required to draw a histogram
function(input, output, session) {

  filtered_primary <- reactive({
    house_primary_long |>
      filter(district_num == input$district_input) |>
      mutate(
        label = paste0(candidate, ": ", (vote_pct * 100) |> round(1), "% (", value |> prettyNum(big.mark = ","), " votes)")
      ) |>
      arrange(desc(vote_pct))
  })
  
  filtered_general <- reactive({
    house_general_long |>
      filter(district_num == input$district_input) |>
      mutate(label = paste0(party, ": ", (vote_pct * 100) |> round(1), "% (", value |> prettyNum(big.mark = ","), " votes)"))
  })
  
  filtered_reg <- reactive({
    registration_long |>
      filter(district_num == input$district_input)
  })
  
  
  output$dem_primary_plot <- renderPlot({
    
    data <- filtered_primary() |>
      filter(party == "Dem.") |>
      mutate(candidate = fct_reorder(candidate, vote_pct) |> fct_rev())
    colors <- colorRampPalette(c("darkblue", "lightblue"))(nrow(data))
    names(colors) <- data$candidate
    
    ggplot(data = data, aes(y = vote_pct, label = label, x = 1, fill = candidate)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = colors) +
      geom_bar_text(position = "stack", place = "center") +
      labs(title = "Democratic Primary") +
      theme_bbi()
    
  })
  
  output$rep_primary_plot <- renderPlot({
    
    data <- filtered_primary() |>
      filter(party == "Rep.") |>
      mutate(candidate = fct_reorder(candidate, vote_pct) |> fct_rev())
    colors <- colorRampPalette(c("darkred", "pink"))(nrow(data))
    names(colors) <- data$candidate

    ggplot(data = data, aes(y = vote_pct, label = label, x = 1, fill = candidate)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = colors) +
      geom_bar_text(position = "stack", place = "center") +
      labs(title = "Republican Primary") +
      theme_bbi()
    
  })
  
  output$lib_primary_plot <- renderPlot({
    
    data <- filtered_primary() |>
      filter(party == "Lib.") |>
      mutate(candidate = fct_reorder(candidate, vote_pct) |> fct_rev())
    colors <- colorRampPalette(c("yellow", "lightyellow"))(nrow(data))
    names(colors) <- data$candidate
    
    ggplot(data = data, aes(y = vote_pct, label = label, x = 1, fill = candidate)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = colors) +
      geom_bar_text(position = "stack", place = "center") +
      labs(title = "Libertarian Primary") +
      theme_bbi()
    
  })
  
  output$general_plot <- renderPlot({
    
    ggplot(filtered_general(), aes(y = vote_pct, label = label, x = 1, fill = party)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = party_vote_pal) +
      geom_bar_text(position = "stack", place = "center") +
      labs(title = "General Election") +
      theme_bbi()
    
  })
  
  output$registration_plot <- renderPlot({
    
    ggplot(filtered_reg(), aes(y = reg_pct, label = label, x = 1, fill = party)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = party_reg_pal) +
      geom_bar_text(position = "stack", place = "center") +
      labs(title = "Party Registration") +
      theme_bbi()
    
  })
  
  

}
