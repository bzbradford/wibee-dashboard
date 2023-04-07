## Data Tab: Bee activity by date ##

# UI ----

activityByDateUI <- function() {
  ns <- NS("activityByDate")
  div(class = "data-tab", uiOutput(ns("ui"))
  )
}


# Server ----

#' @param data a `reactive()` expression containing `filtered_surveys()`
#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

activityByDateServer <- function(data, data_long) {
  moduleServer(
    id = "activityByDate",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data()) > 0
      })
      
      
      ## Plot UI ----
      
      output$ui <- renderUI({
        if (!data_ready()) return(div(class = "well", "No surveys selected. Change your filters above or hit 'reset filters' below."))
        
        tagList(
          radioButtons(
            inputId = ns("grouping"),
            label = "Choose time period for grouping:",
            choices = c("Day", "Week", "Month"),
            inline = T
          ),
          plotlyOutput(ns("plot")),
          div(class = "plot-caption", "This chart shows seasonal trends in pollinator activity by showing the average activity by pollinator group across all surveys conducted on a given day, week, or month. The date range and year(s) can be selected in the survey filters above. All selected years are combined in this plot to highlight seasonal trends.")
        )
      })
      
      
      ## Plot Output ----
      
      output$plot <- renderPlotly({
        req(input$grouping)
        req(data_ready())
        
        grouping <- input$grouping
        df <- data_long()
        
        # bring all dates to the same year
        cur_year <- max(df$year)
        df <- df %>%
          mutate(date = as.Date(paste(cur_year, month, day, sep = "-")))
        counts <- data() %>%
          mutate(date = as.Date(paste(cur_year, month, day, sep = "-")))
        
        plt <- plot_ly()
        
        # set data and plot layouts depending on grouping
        if (grouping == "Day") {
          survey_counts <- counts %>%
            count(date)
          plot_data <- df %>%
            group_by(date, bee_name, bee_color) %>%
            summarise(count = round(mean(count), 1), .groups = "drop") %>%
            droplevels()
          bee_colors <- levels(plot_data$bee_color)
          
          plt <- plt %>%
            layout(
              barmode = "stack",
              title = list(text = "<b>Daily average pollinator visits per survey</b>", font = list(size = 15)),
              xaxis = list(title = "", type = "date", tickformat = "%B %d"),
              yaxis = list(title = "Insect visits per survey"),
              hovermode = "x unified",
              legend = list(orientation = "h"),
              bargap = 0
            )
        } else if (grouping == "Week") {
          survey_counts <- counts %>%
            count(week) %>%
            mutate(date = as.Date(paste(cur_year, 1 + (week - 1) * 7), "%Y %j"))
          plot_data <- df %>%
            group_by(week, bee_name, bee_color) %>%
            summarise(count = round(mean(count), 1), .groups = "drop") %>%
            mutate(date = as.Date(paste(cur_year, 1 + (week - 1) * 7), "%Y %j")) %>%
            droplevels()
          bee_colors <- levels(plot_data$bee_color)
          
          plt <- plt %>%
            layout(
              barmode = "stack",
              title = list(text = "<b>Weekly average pollinator visits per survey</b>", font = list(size = 15)),
              xaxis = list(
                title = "",
                type = "date",
                tickformat = "%b %d"),
              yaxis = list(title = "Insect visits per survey"),
              hovermode = "x unified",
              legend = list(orientation = "h"),
              bargap = 0
            )
        } else if (grouping == "Month") {
          survey_counts <- counts %>%
            count(month) %>%
            mutate(date = as.Date(paste(cur_year, month, 15, sep = "-")))
          plot_data <- df %>%
            group_by(month, bee_name, bee_color) %>%
            summarise(count = round(mean(count), 1), .groups = "drop") %>%
            mutate(date = as.Date(paste(cur_year, month, 15, sep = "-"))) %>%
            droplevels()
          bee_colors <- levels(plot_data$bee_color)
          
          plt <- plt %>%
            layout(
              barmode = "stack",
              title = list(
                text = "<b>Monthly average pollinator visits per survey</b>",
                font = list(size = 15)),
              xaxis = list(
                title = "",
                type = "date",
                tickformat = "%B",
                dtick = "M1",
                ticklabelmode = "period"),
              yaxis = list(
                title = "Insect visits per survey"),
              hovermode = "x unified",
              legend = list(orientation = "h"),
              bargap = 0
            )
        }
        
        # add the bee counts and dummy survey count data for legend
        plt %>%
          add_trace(
            data = survey_counts,
            x = ~date,
            y = 0,
            type = "bar",
            opacity = 0,
            text = ~n,
            colors = bee_colors,
            hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
            showlegend = F
          ) %>%
          add_trace(
            data = plot_data,
            x = ~date,
            y = ~count,
            type = "bar",
            color = ~ bee_name,
            colors = bee_colors,
            marker = list(line = list(color = "#ffffff", width = .25))
          )
      })
    }
  )
}
