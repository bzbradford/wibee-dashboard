## Plot pollinator activity by crop type

# UI ----

activityByCropUI <- function() {
  ns <- NS("activityByCrop")
  div(
    class = "data-tab",
    uiOutput(ns("ui"))
  )
}


# Server ----

#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

activityByCropServer <- function(data_long) {
  moduleServer(
    id = "activityByCrop",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data_long()) > 0
      })
      
      output$ui <- renderUI({
        if (!data_ready()) return(noSurveysMsg())
        
        tagList(
          plotlyOutput(ns("plot"), height = "600px"),
          div(class = "plot-caption", "This interactive chart compares total pollinator visitation rates across all of the different crops and non-crop plants surveyed with the app. The number of surveys represented by each plant species or group is shown in parentheses in the labels.")
        )
      })
      
      output$plot <- renderPlotly({
        req(data_ready())
        
        # estimate label lengths to increase plot margin
        margin <- min(max(40, 10 + 4 * max(nchar(data_long()$plant_label))), 200)
        
        plot_data <- data_long() %>%
          group_by(plant_label, bee_name, bee_color) %>%
          summarise(
            visit_rate = round(mean(count), 1),
            n = n(),
            .groups = "drop") %>%
          droplevels() %>%
          mutate(x = fct_inorder(paste0("(", n, ") ", plant_label)))
        
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
            title = list(text = "<b>Pollinator visitation rates by plant type</b>", font = list(size = 15)),
            xaxis = list(title = "", fixedrange = T, tickangle = 45),
            yaxis = list(title = "Number of visits per survey", fixedrange = T),
            hovermode = "x unified",
            margin = list(b = margin)
          )
      })
    }
  )
}

