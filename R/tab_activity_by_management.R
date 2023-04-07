## Data tab: Bee activity by management type ##

# UI ----

activityByMgmtUI <- function() {
  ns <- NS("activityByMgmt")
  uiOutput(ns("ui"))
}


# Server ----

#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

activityByMgmtServer <- function(data_long) {
  moduleServer(
    id = "activityByMgmt",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data_long()) > 0
      })
      
      output$ui <- renderUI({
        if (!data_ready()) return(div(class = "well", "No surveys selected. Change your filters above or hit 'reset filters' below."))
        
        div(
          class = "data_tab",
          plotlyOutput(ns("plot")),
          div(class = "plot-caption", "This chart compares total pollinator visitation rates by user-reported management practices. The number of surveys represented by each practice is shown in parentheses in the labels.")
        )
      })
      
      output$plot <- renderPlotly({
        req(data_ready())
        
        plot_data <- data_long() %>%
          group_by(management_name, bee_name, bee_color) %>%
          summarise(
            visit_rate = round(mean(count), 1),
            n = n(),
            .groups = "drop") %>%
          droplevels() %>%
          mutate(x = fct_inorder(paste0("(", n, ") ", management_name)))
        
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
            title = list(
              text = "<b>Pollinator visitation rates by management type</b>",
              font = list(size = 15)),
            xaxis = list(title = "", fixedrange = T),
            yaxis = list(title = "Number of visits per survey", fixedrange = T),
            hovermode = "x unified"
          )
      })
    }
  )
}
