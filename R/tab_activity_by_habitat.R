## Plot pollinator activity by habitat type

# UI ----

activityByHabitatUI <- function() {
  ns <- NS("activityByHabitat")
  uiOutput(ns("ui"))
}


# Server ----

#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

activityByHabitatServer <- function(data_long) {
  moduleServer(
    id = "activityByHabitat",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({nrow(data_long()) > 0})
      
      output$ui <- renderUI({
        if (!data_ready()) return(div(class = "well", "No surveys selected. Change your filters above or hit 'reset filters' below."))
        
        tagList(
          plotlyOutput(ns("plot")),
          div(class = "plot-caption", "This chart compares total pollinator visitation rates across different habitat types. The number of surveys represented by each habitat is shown in parentheses in the labels.")
        )
      })
      
      output$plot <- renderPlotly({
        req(data_ready())
        
        plot_data <- data_long() %>%
          group_by(habitat_name, bee_name, bee_color) %>%
          summarise(
            visit_rate = round(mean(count), 1),
            n = n(),
            .groups = "drop") %>%
          droplevels() %>%
          mutate(x = fct_inorder(paste0("(", n, ") ", habitat_name)))
        
        plot_data %>%
          plot_ly(
            type = "bar",
            x = ~ x,
            y = ~ visit_rate,
            color = ~ bee_name,
            colors = ~ levels(.$bee_color),
            marker = list(line = list(color = "#ffffff", width = .25))) %>%
          layout(
            barmode = "stack",
            title = list(text = "<b>Pollinator visitation rates by habitat type</b>", font = list(size = 15)),
            xaxis = list(title = "", fixedrange = T),
            yaxis = list(title = "Number of insect visits per survey", fixedrange = T),
            hovermode = "x unified"
          )
      })
    }
  )
}

