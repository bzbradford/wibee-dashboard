## SPECIES COMPOSITION PIE CHARTS ##


# UI ----

speciesCompUI <- function() {
  ns <- NS("speciesComp")
  div(
    class = "data-tab",
    wellPanel(
      h4("Pollinator species composition pie charts"),
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
  
  plot_title <- paste0(title, " (", n_surveys, ")")
  onclick <- paste0("Shiny.onInputChange('", ns("removePlot"), "', '", id, "', {priority: 'event'})")
  
  tagList(
    div(
      class = "pie",
      h4(class = "plot-title", plot_title),
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
      
      wi_surveys <- surveys_long %>%
        filter(inwi, bee_name != "Wild bees") %>%
        droplevels()
      
      pinned_plots <- reactiveVal(list())
      show_current <- reactiveVal(TRUE)
      first_run <- reactiveVal(TRUE)
      msg <- reactiveVal()
      
      output$pinnedPlots <- renderUI({
        if (length(pinned_plots()) == 0 & first_run()) {
          plots <- list()
          plot_id <- "_all"
          plots[[plot_id]] <- make_pie(wi_surveys, "All Wisconsin surveys", plot_id)
          pinned_plots(plots)
          first_run(FALSE)
        }
        
        plots <- pinned_plots()
        if (show_current() & nrow(cur_surveys_long()) > 0) {
          cur_plot <- make_pie(cur_surveys_long(), "Currently selected surveys", "cur")
          plots <- c(plots, cur_plot)
        }
        
        div(class = "flex-row", plots)
      })
      
      output$plotControls <- renderUI({
        tagList(
          div(
            class = "flex-row",
            div(textInput(ns("title"), label = NULL, value = isolate(input$title))),
            div(actionButton(ns("addPlot"), "Pin current plot")),
            {if (!show_current()) div(actionButton(ns("toggleCurrent"), "Show current plot"))},
            div(actionButton(ns("clearPlots"), "Reset plots"))
          ),
          {if (!is.null(msg())) div(style = "color: red; font-style: italic;", msg())}
        )
      })
      
      observeEvent(input$addPlot, {
        if (nrow(cur_surveys_long()) == 0) {
          msg("There aren't any surveys currently selected.")
          return()
        }
        
        if (input$title == "") {
          msg("Please give your plot a title to pin it.")
          return()
        }
        
        if (length(pinned_plots()) >= 8) {
          msg("You can pin up to 8 plots, delete some to pin another.")
          return()
        }
        
        msg(NULL)
        plots <- pinned_plots()
        plot_id <- gsub("[^[:alnum:]]", "", input$title)
        plots[[plot_id]] <- make_pie(cur_surveys_long(), input$title, plot_id)
        pinned_plots(plots)
        updateTextInput(inputId = "title", value = "")
      })
      
      observeEvent(input$removePlot, {
        msg(NULL)
        id <- input$removePlot
        
        if (id == "cur") {
          show_current(FALSE)
        } else {
          plots <- pinned_plots()
          print(plots)
          plots[[input$removePlot]] <- NULL
          pinned_plots(plots)
        }
      })
      
      observeEvent(input$toggleCurrent, {
        msg(NULL)
        show_current(!show_current())
      })
      
      observeEvent(input$clearPlots, {
        pinned_plots(c())
        first_run(TRUE)
        show_current(TRUE)
        msg(NULL)
      })
    }
  )
}
