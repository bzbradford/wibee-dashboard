## SPECIES COMPOSITION PIE CHARTS ##

# Helpers ----

make_pie <- function(df, title) {
  n_surveys <- length(unique(df$id))
  
  plot_data <- df %>%
    group_by(bee_name, bee_color) %>%
    summarise(mean_count = round(mean(count), 1), .groups = "drop") %>%
    droplevels()
  
  plt <- plot_data %>%
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
      direction = "clockwise"
    ) %>%
    add_annotations(
      y = 1.075,
      x = 0.5, 
      text = paste0("<b>", title, " (", n_surveys, ")</b>"), 
      showarrow = F,
      font = list(size = 15)
    ) %>%
    layout(
      showlegend = F,
      margin = list(l = 0, r = 0),
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  
  tagList(
    div(class = "pie", renderPlotly(plt))
  )
}


# UI ----

speciesCompUI <- function() {
  ns <- NS("speciesComp")
  div(
    class = "data-tab",
    h4("Pollinator composition pie charts"),
    wellPanel(
      strong("Give a title to your chart"),
      div(
        class = "flex-row",
        div(textInput(ns("title"), label = NULL)),
        div(actionButton(ns("addPlot"), "Pin current plot")),
        div(actionButton(ns("clearPlots"), "Reset plots")),
        div(
          materialSwitch(
            ns("showCurrent"),
            "Show current?",
            value = T,
            status = "success",
            inline = T
          )
        )
      )
    ),
    uiOutput(ns("ui"))
  )
}


# Server ----

#' @param cur_surveys_long a `reactive()` expression containing `filtered_surveys_long()`

speciesCompServer <- function(cur_surveys_long) {
  moduleServer(
    id = "speciesComp",
    function(input, output, session) {
      ns <- session$ns
      
      pinned_plots <- reactiveVal(tagList())
      
      output$ui <- renderUI({
        if (length(pinned_plots()) == 0) {
          first_plot <- make_pie(cur_surveys_long(), "All Wisconsin surveys")
          pinned_plots(first_plot)
        }
        
        plots <- pinned_plots()
        if (input$showCurrent) {
          plots <- c(plots, make_pie(cur_surveys_long(), "Currently selected surveys"))
        }
        
        div(class = "flex-row", plots)
      })
      
      observeEvent(input$addPlot, {
        req(input$title != "")
        req(nrow(cur_surveys_long()) > 0)
        
        new_plot <- make_pie(cur_surveys_long(), input$title)
        pinned_plots(c(pinned_plots(), new_plot))
      })
      
      observeEvent(input$clearPlots, {
        pinned_plots(c())
      })
    }
  )
}


