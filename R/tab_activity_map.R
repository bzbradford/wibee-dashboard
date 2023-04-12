## Gridded map showing surveys or pollinator activity

# UI ----

activityMapUI <- function() {
  ns <- NS("activityMap")
  div(
    class = "data-tab",
    uiOutput(ns("ui"))
  )
}


# Server ----

#' @param data a `reactive()` expression containing `filtered_surveys()`
#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

activityMapServer <- function(data, data_long) {
  moduleServer(
    id = "activityMap",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        (nrow(data()) > 0) & (nrow(data_long()) > 0)
      })
      
      output$ui <- renderUI({
        if (!data_ready()) return(noSurveysMsg())
        
        tagList(
          radioButtons(
            inputId = ns("type"),
            label = "Choose summary value to show on the map:",
            choices = c(
              "Number of surveys",
              "Insect visits per survey",
              "Number of users"
            ),
            inline = T
          ),
          radioButtons(
            inputId = ns("size"),
            label = "Choose spatial aggregation size (degrees):",
            choices = c(0.05, 0.1, 0.25, 0.5),
            selected = 0.1,
            inline = T
          ),
          leafletOutput(ns("map"), height = "600px"),
          div(class = "plot-caption", "This map displays a summary of all the surveys currently selected by your filters. Change your filter selections or choose a different summary type to change what is shown on this map.")
        )
      })
      
      output$map <- renderLeaflet({
        req(data_ready())
        req(input$type)
        req(input$size)
        
        div <- as.numeric(input$size)
        df <- data() %>%
          mutate(
            lat = round(lat / div) * div,
            lng = round(lng / div) * div
          ) %>%
          group_by(lat, lng)
        
        df_long <- data_long() %>%
          mutate(
            lat = round(lat / div) * div,
            lng = round(lng / div) * div
          ) %>%
          group_by(lat, lng, id)
        
        if (input$type == "Number of surveys") {
          df <- df %>%
            summarise(value = n(), .groups = "drop")
        } else if (input$type == "Insect visits per survey") {
          df <- df_long %>%
            summarise(total_visits = sum(count), .groups = "drop_last") %>%
            summarise(value = round(mean(total_visits), 1), .groups = "drop")
        } else if (input$type == "Number of users") {
          df <- df %>%
            summarise(value = n_distinct(user_id), .groups = "drop")
        }
        
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = df$value
        )
        
        df %>%
          leaflet() %>%
          addTiles() %>%
          addRectangles(
            lng1 = ~ lng - div / 2, lng2 = ~ lng +  div / 2,
            lat1 = ~ lat - div / 2, lat2 = ~ lat + div / 2,
            label = ~ paste0(input$data_map_summary_type, ": ", value),
            weight = 0.25,
            opacity = 0.9,
            color = "darkgrey",
            fillOpacity = .9,
            fillColor = ~pal(value),
            highlight = highlightOptions(
              weight = 2,
              color = "red")
          )
      })
    }
  )
}
