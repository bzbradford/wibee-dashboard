## Data tab: Surveys by date ##


# UI ----

surveysByDateUI <- function() {
  ns <- NS("surveysByDate")
  div(class = "data-tab", uiOutput(ns("ui")))
}


# Server ----

#' @param data a `reactive()` expression containing `filtered_surveys()`

surveysByDateServer <- function(data) {
  moduleServer(
    id = "surveysByDate",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data()) > 0
      })
      
      output$ui <- renderUI({
        if (!data_ready()) return(div(class = "well", "No surveys selected. Change your filters above or hit 'reset filters' below."))
        
        tagList(
          plotlyOutput(ns("plot")),
          div(class = "plot-caption", "This chart shows the number of surveys completed each week and (optionally) the user ID of the person who conducted the survey, within the date range specified in the data filters above.")
        )
      })
      
      output$plot <- renderPlotly({
        req(data_ready())
        
        plot_data <- data() %>%
          group_by(year, week) %>%
          summarise(n_surveys = n(), .groups = "drop_last") %>%
          mutate(date = as.Date(paste0(year, "-01-01")) + weeks(week - 1))
        
        plot_data %>%
          plot_ly(
            x = ~ date,
            y = ~ n_surveys,
            type = "bar",
            xperiodalignment = "left",
            marker = list(line = list(color = "#ffffff", width = .25))) %>%
          layout(
            barmode = "stack",
            title = list(
              text = "<b>Weekly total number of completed surveys</b>",
              font = list(size = 15)),
            xaxis = list(
              title = "",
              type = "date",
              tickformat = "%b %d<br>%Y"),
            yaxis = list(title = "Number of surveys"),
            hovermode = "x unified",
            showlegend = F,
            bargap = 0
          )
      })
    }
  )
}
