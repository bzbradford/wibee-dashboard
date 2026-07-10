## SPECIES COMPOSITION PIE CHARTS ##

# UI ----

speciesCompUI <- function() {
  ns <- NS("speciesComp")
  div(
    class = "data-tab",
    wellPanel(
      h4("Pollinator species composition pie charts"),
      p(em(
        "Compare pollinator species compositions across different survey groups with this display tool. You select a set of surveys using the filters above (for instance, select surveys conducted in prairies), then pin the pie chart to your list for comparison with other survey sets."
      )),
      strong("Input a chart title, then pin to list"),
      uiOutput(ns("plotControls"))
    ),
    uiOutput(ns("pinnedPlots"))
  )
}


# Pie builder ----

make_pie <- function(df, title, onclick) {
  n_surveys <- length(unique(df$id))

  plot_data <- df |>
    group_by(bee_name, bee_color) |>
    summarise(mean_count = round(mean(count), 1), .groups = "drop") |>
    droplevels()

  plt <- plot_data |>
    plot_ly(
      labels = ~bee_name,
      values = ~mean_count,
      type = "pie",
      textposition = "inside",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~ paste(mean_count, bee_name, "per survey"),
      marker = list(
        colors = levels(plot_data$bee_color),
        line = list(color = "#ffffff", width = 1)
      ),
      sort = FALSE,
      direction = "clockwise"
    ) |>
    layout(
      showlegend = FALSE,
      margin = list(l = 0, r = 0),
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )

  plot_title <- paste0(title, " (", n_surveys, ")")

  div(
    class = "pie",
    h4(class = "plot-title", plot_title),
    plt,
    div(
      align = "right",
      tags$span(
        class = "plot-close",
        onclick = onclick,
        "close"
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

      pie_onclick <- function(id) {
        sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns("removePlot"),
          id
        )
      }

      rv <- reactiveValues(
        pinned_plots = NULL,
        plot_num = 0,
        show_current = TRUE,
        msg = NULL
      )

      # deduce if the wild bees toggle is active
      show_wild_bees <- reactive({
        "Wild bees" %in% cur_surveys_long()$bee_name
      })

      # all surveys data for pie chart
      all_surveys <- reactive({
        df <- if (show_wild_bees()) {
          surveys_long |> filter(bee_name %in% wildbee_names)
        } else {
          surveys_long |> filter(bee_name != "Wild bees")
        }
        droplevels(df)
      })

      # default all surveys pie chart
      all_surveys_plot <- reactive({
        make_pie(
          all_surveys(),
          "All surveys",
          onclick = pie_onclick(id = "_all")
        )
      })

      # reset plots
      observe({
        if (is.null(rv$pinned_plots)) {
          rv$pinned_plots <- list(
            `_all` = all_surveys_plot()
          )
        }
      })

      output$pinnedPlots <- renderUI({
        plots <- rv$pinned_plots
        if (rv$show_current & nrow(cur_surveys_long()) > 0) {
          cur_plot <- make_pie(
            cur_surveys_long(),
            "Currently selected surveys",
            onclick = pie_onclick(id = "cur")
          )
          plots <- c(plots, list(cur_plot))
        }

        div(class = "flex-row", plots)
      })

      output$plotControls <- renderUI({
        tagList(
          div(
            class = "flex-row",
            div(textInput(
              ns("title"),
              label = NULL,
              value = isolate(input$title)
            )),
            div(actionButton(ns("addPlot"), "Pin current plot")),
            {
              if (!rv$show_current) {
                div(actionButton(ns("toggleCurrent"), "Show current plot"))
              }
            },
            div(actionButton(ns("clearPlots"), "Reset plots"))
          ),
          {
            if (!is.null(rv$msg)) {
              div(style = "color: red; font-style: italic;", rv$msg)
            }
          }
        )
      })

      observeEvent(input$addPlot, {
        surveys <- cur_surveys_long()
        plots <- rv$pinned_plots

        if (nrow(surveys) == 0) {
          rv$msg <- "There aren't any surveys currently selected."
          return()
        }

        if (input$title == "") {
          rv$msg <- "Please give your plot a title to pin it."
          return()
        }

        if (length(rv$pinned_plots) >= 8) {
          rv$msg <- "You can pin up to 8 plots, delete some to pin another."
          return()
        }

        rv$msg <- NULL
        rv$plot_num <- rv$plot_num + 1
        plot_id <- paste0("plt_", rv$plot_num)
        plots[[plot_id]] <- make_pie(
          surveys,
          input$title,
          onclick = pie_onclick(id = plot_id)
        )
        rv$pinned_plots <- plots
        updateTextInput(inputId = "title", value = "")
      })

      observeEvent(input$removePlot, {
        rv$msg <- NULL
        id <- input$removePlot

        if (id == "cur") {
          rv$show_current <- FALSE
        } else {
          plots <- rv$pinned_plots
          plots[[input$removePlot]] <- NULL
          rv$pinned_plots <- plots
        }
      })

      observeEvent(input$toggleCurrent, {
        rv$msg <- NULL
        rv$show_current <- !rv$show_current
      })

      observeEvent(input$clearPlots, {
        rv$pinned_plots <- NULL
        rv$show_current <- TRUE
        rv$plot_num <- 0
        rv$msg <- NULL
      })
    }
  )
}
