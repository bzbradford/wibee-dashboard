## Gridded map showing surveys or pollinator activity

# UI ----

dataTableUI <- function() {
  ns <- NS("dataTable")
  div(
    class = "data-tab",
    uiOutput(ns("ui"))
  )
}


# Server ----

#' @param data_long a `reactive()` expression containing `filtered_surveys_long()`

dataTableServer <- function(data_long) {
  moduleServer(
    id = "dataTable",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data_long()) > 0
      })
      
      choices <- list(
        "Survey ID" = "id",
        "User ID" = "user_id",
        "Year" = "year",
        "Date" = "date",
        "Location" = "grid_pt",
        "Habitat" = "habitat",
        "Crop/Plant" = "crop",
        "Management" = "management"
      )
      
      selected <- c("habitat", "crop", "management")
      
      output$ui <- renderUI({
        if (!data_ready()) return(noSurveysMsg())
        
        tagList(
          p("The table below shows the surveys you have currently selected by the filters above. Check or uncheck the grouping variables to simplify or expand the summary table. If a row shows n > 1, some surveys were averaged together. You can select all the columns to get each individual survey. Click the download button to save a copy of the table.", em("Note: this data is for personal or educational use only. Other use or use in a publication is not permitted without the consent of the team. ", a("Email us with any inquiries.", href = "mailto:pollinators@wisc.edu"))),
          fixedRow(
            column(8,
              checkboxGroupInput(
                inputId = ns("groups"),
                label = "Select which variables to include in table:",
                choices = choices,
                selected = selected,
                inline = T)
            ),
            column(4,
              align = "right",
              downloadButton(ns("download_data"), "Download data")
            )
          ),
          DTOutput(ns("table"))
        )
      })
      
      table_data <- reactive({
        data_long() %>%
          mutate(date = as.character(date)) %>%
          group_by_at(input$groups) %>%
          group_by(bee_name, .add = T) %>%
          summarise(
            n = n(),
            visit_rate = round(mean(count), 1),
            .groups = "drop_last") %>%
          mutate("Total rate" = sum(visit_rate)) %>%
          ungroup() %>%
          pivot_wider(names_from = bee_name, values_from = visit_rate) %>%
          mutate("row" = row_number()) %>%
          select("row", everything())
      })
      
      output$table <- renderDT(
        table_data(),
        rownames = F,
        options = list(pageLength = 25)
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste0("WiBee data ", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".csv")
        },
        content = function(file) {
          write_csv(table_data(), file)
        }
      )
    }
  )
}
