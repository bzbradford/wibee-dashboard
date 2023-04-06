## SPECIES COMPOSITION PIE CHARTS ##

# UI ----

#' requires global vars:
#' - surveys
#' - surveys_long

speciesCompUI <- function() {
  ns <- NS("species-comp")
  
  div(
    class = "data-tab",
    plotlyOutput(ns("allChart"), width = "45%", inline = T),
    plotlyOutput(ns("selectedChart"), width = "45%", inline = T)
  )
}


# Server ----

#' requires global vars:
#' - surveys
#' - surveys_long
#' 
#' @param filtered_surveys a `reactive()` expression containing `filtered_surveys()`
#' @param filtered_surveys_long a `reactive()` expression containing `filtered_surveys_long()`
#' @param which_bees a `reactive()` expression containing `input$which_bees`

speciesCompServer <- function(filtered_surveys, filtered_surveys_long, which_bees) {
  moduleServer(
    id = "species-comp",
    function(input, output, session) {
      ns <- session$ns
      
      
      ## whole dataset chart ----
      
      output$allChart <- renderPlotly({
        df <- surveys_long %>%
          filter(bee_name %in% which_bees()) %>%
          droplevels() %>%
          group_by(bee_name, bee_color) %>%
          summarise(mean_count = round(mean(count), 1), .groups = "drop")
        
        df %>% 
          plot_ly(
            labels = ~ bee_name,
            values = ~ mean_count,
            type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            hoverinfo = "text",
            text = ~ paste(mean_count, bee_name, "per survey"),
            marker = list(
              colors = levels(df$bee_color),
              line = list(color = "#ffffff", width = 1)),
            sort = F,
            direction = "clockwise",
            showlegend = F
          ) %>%
          add_annotations(
            y = 1.075,
            x = 0.5, 
            text = paste0("<b>All surveys (", nrow(surveys), ")</b>"), 
            showarrow = F,
            font = list(size = 15))
      })
      
      
      ## selected data chart ----
      output$selectedChart <- renderPlotly({
        df <- filtered_surveys_long() %>%
          group_by(bee_name, bee_color) %>%
          summarise(mean_count = round(mean(count), 1), .groups = "drop")
        
        df %>% 
          plot_ly(labels = ~ bee_name, values = ~ mean_count, type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            hoverinfo = "text",
            text = ~ paste(mean_count, bee_name, "per survey"),
            marker = list(
              colors = levels(df$bee_color),
              line = list(color = "#ffffff", width = 1)),
            sort = F,
            direction = "clockwise",
            showlegend = F
          ) %>%
          add_annotations(
            y = 1.075, 
            x = 0.5, 
            text = paste0("<b>Selected surveys (", nrow(filtered_surveys()), ")</b>"), 
            showarrow = F,
            font = list(size = 15))
      })
    }
  )
}
