## SPECIES COMPOSITION PIE CHARTS ##

# UI ----

speciesCompUI <- function() {
  ns <- NS("speciesComp")
  uiOutput(ns("ui"))
}


# Server ----

#' requires global vars:
#' - surveys
#' - surveys_long
#' 
#' @param data a `reactive()` expression containing `filtered_surveys()`
#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`
#' @param which_bees a `reactive()` expression containing `input$which_bees`

speciesCompServer <- function(data, data_long, which_bees) {
  moduleServer(
    id = "speciesComp",
    function(input, output, session) {
      ns <- session$ns
      
      # data_ready <- reactive({
      #   (nrow(data()) > 0) &
      #     (nrow(data_long()) > 0) &
      #     (!is.null(which_bees()))
      # })
      
      output$ui <- renderUI({
        div(
          class = "data-tab",
          plotlyOutput(ns("plotAll"), width = "45%", inline = T),
          plotlyOutput(ns("plotSelected"), width = "45%", inline = T)
        )
      })
      
      output$plotAll <- renderPlotly({
        plot_data <- surveys_long %>%
          filter(bee_name %in% which_bees()) %>%
          droplevels() %>%
          group_by(bee_name, bee_color) %>%
          summarise(mean_count = round(mean(count), 1), .groups = "drop")
        
        bee_colors <- levels(plot_data$bee_color)
        
        plot_data %>%
          plot_ly(
            labels = ~ bee_name,
            values = ~ mean_count,
            type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            hoverinfo = "text",
            text = ~ paste(mean_count, bee_name, "per survey"),
            marker = list(
              colors = bee_colors,
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
      
      output$plotSelected <- renderPlotly({
        plot_data <- data_long() %>%
          group_by(bee_name, bee_color) %>%
          summarise(mean_count = round(mean(count), 1), .groups = "drop")
        
        n_surveys <- nrow(data())
        bee_colors <- levels(plot_data$bee_color)
        
        plot_data %>% 
          plot_ly(labels = ~ bee_name, values = ~ mean_count, type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            hoverinfo = "text",
            text = ~ paste(mean_count, bee_name, "per survey"),
            marker = list(
              colors = bee_colors,
              line = list(color = "#ffffff", width = 1)),
            sort = F,
            direction = "clockwise",
            showlegend = F
          ) %>%
          add_annotations(
            y = 1.075, 
            x = 0.5, 
            text = paste0("<b>Selected surveys (", n_surveys, ")</b>"), 
            showarrow = F,
            font = list(size = 15))
      })
    }
  )
}
