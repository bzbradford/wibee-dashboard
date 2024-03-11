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
      
      map_data <- reactive({
        req(data_ready(), input$type, input$size)
        
        div <- as.numeric(input$size)

        if (input$type == "Number of surveys") {
          df <- data() %>%
            mutate(across(c(lat, lng), ~ round(.x / div) * div)) %>%
            summarise(value = n(), .by = c(lat, lng))
        } else if (input$type == "Number of users") {
          df <- data() %>%
            mutate(across(c(lat, lng), ~ round(.x / div) * div)) %>%
            summarise(value = n_distinct(user_id), .by = c(lat, lng))
        } else {
          # "Insect visits per survey"
          df <- data_long() %>%
            mutate(across(c(lat, lng), ~ round(.x / div) * div)) %>%
            summarise(total_visits = sum(count), .by = c(lat, lng, id)) %>%
            summarise(value = round(mean(total_visits), 1), .by = c(lat, lng))
        }
        
        df %>%
          mutate(
            lat1 = lat - div/2, lat2 = lat + div/2,
            lng1 = lng - div/2, lng2 = lng + div/2,
            label = paste0(input$type, ": ", value)
          )
      })
      
      addGrids <- function(map, df, div) {
        pal <- colorNumeric(
          palette = "viridis",
          domain = sqrt(df$value),
          reverse = F
        )
        
        map %>%
          addRectangles(
            data = df,
            lat1 = ~lat1,
            lat2 = ~lat2,
            lng1 = ~lng1,
            lng2 = ~lng2,
            label = ~label,
            group = "grids",
            color = "darkgrey",
            weight = 0.25,
            opacity = 0.9,
            fillColor = ~pal(sqrt(value)),
            fillOpacity = .9,
            highlight = highlightOptions(
              weight = 2,
              color = "red"
            )
          )
      }
      
      output$map <- renderLeaflet({
        leaflet() %>%
          setView(lng = -89.7, lat = 44.8, zoom = 7) %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
          addGrids(isolate(map_data()))
      })
      
      observe({
        leafletProxy("map") %>%
          clearGroup("grids") %>%
          addGrids(map_data())
      })
      
    }
  )
}
