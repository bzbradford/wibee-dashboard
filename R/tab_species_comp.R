## SPECIES COMPOSITION PIE CHARTS ##


# UI ----

speciesCompUI <- function() {
  ns <- NS("speciesComp")
  div(
    class = "data-tab",
    wellPanel(
      h4("Pollinator composition pie charts"),
      p(em("Compare pollinator species compositions across different survey groups with this display tool. You select a set of surveys using the filters above (for instance, select surveys conducted in prairies), then pin the pie chart to your list for comparison with other survey sets.")),
      strong("Input a chart title, then pin to list"),
      uiOutput(ns("plotControls"))
    ),
    uiOutput(ns("pinnedPlots"))
  )
}



# Pie builder ----

make_pie <- function(df, title, id) {
  ns <- NS("speciesComp")
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
    layout(
      showlegend = F,
      margin = list(l = 0, r = 0),
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  
  onclick <- paste0("Shiny.onInputChange('", ns("removePlot"), "', ", id, ", {priority: 'event'})")
  
  tagList(
    div(
      class = "pie",
      h4(
        class = "plot-title",
        paste0(title, " (", n_surveys, ")")
      ),
      renderPlotly(plt),
      div(
        align = "right",
        tags$span(
          class = "plot-close",
          onclick = onclick,
          "close"
        )
      )
    )
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
      show_current <- reactiveVal(TRUE)
      first_run <- reactiveVal(TRUE)
      
      output$pinnedPlots <- renderUI({
        if (length(pinned_plots()) == 0 & first_run()) {
          first_plot <- make_pie(cur_surveys_long(), "All Wisconsin surveys", 1)
          pinned_plots(first_plot)
          first_run(FALSE)
        }
        
        plots <- pinned_plots()
        if (show_current()) {
          cur_plot <- make_pie(cur_surveys_long(), "Currently selected surveys", 999)
          plots <- c(plots, cur_plot)
        }
        
        div(class = "flex-row", plots)
      })
      
      output$plotControls <- renderUI({
        div(
          class = "flex-row",
          div(textInput(ns("title"), label = NULL)),
          div(actionButton(ns("addPlot"), "Pin current plot")),
          {
            if (!show_current()) div(actionButton(ns("toggleCurrent"), "Show current plot"))
          },
          div(actionButton(ns("clearPlots"), "Reset plots"))
        )
      })
      
      observeEvent(input$addPlot, {
        req(input$title != "")
        req(nrow(cur_surveys_long()) > 0)
        
        new_plot <- make_pie(
          cur_surveys_long(),
          input$title,
          length(pinned_plots()) + 1
        )
        pinned_plots(c(pinned_plots(), new_plot))
      })
      
      observeEvent(input$removePlot, {
        id <- input$removePlot
        print(id)
        
        if (id == 999) {
          show_current(FALSE)
        } else {
          plots <- pinned_plots()
          plots[input$removePlot] <- NULL
          pinned_plots(plots)
        }
      })
      
      observeEvent(input$toggleCurrent, {
        show_current(!show_current())
      })
      
      observeEvent(input$clearPlots, {
        pinned_plots(c())
        first_run(TRUE)
        show_current(TRUE)
      })
    }
  )
}
