## Show some information about WiBee users

# UI ----

userStatsUI <- function() {
  ns <- NS("userStats")
  div(
    class = "data-tab",
    uiOutput(ns("ui"))
  )
}


# Server ----

#' @param data a `reactive()` expression containing `filtered_surveys()`

userStatsServer <- function(data) {
  moduleServer(
    id = "userStats",
    function(input, output, session) {
      ns <- session$ns
      
      data_ready <- reactive({
        nrow(data()) > 0
      })
      
      output$ui <- renderUI({
        if (!data_ready()) return(noSurveysMsg())
        
        tagList(
          plotlyOutput(ns("plot"), height = "600px"),
          div(class = "plot-caption", "This graph shows the total number of surveys conducted each year, further grouped by the number of users who have conducted the same amount of surveys. This summary is based on the surveys selected by the main survey filters shown above.")
        )
      })
      
      output$plot <- renderPlotly({
        req(data_ready())
        
        df1 <- data() %>%
          mutate(year = format(date, "%Y")) %>%
          group_by(year, user_id) %>%
          summarise(surveys_per_user = n(), .groups = "drop") %>%
          group_by(year, surveys_per_user) %>%
          summarise(n_users = n(), total_surveys = sum(surveys_per_user), .groups = "drop") %>%
          arrange(desc(total_surveys)) %>%
          mutate(
            parent = year,
            label = paste0("<B>", total_surveys, " surveys</B>\n", n_users, " users with\n", surveys_per_user, " surveys each"),
            id = interaction(year, label),
            value = total_surveys)
        
        df2 <- df1 %>%
          group_by(year) %>%
          summarise(
            total_surveys = sum(total_surveys),
            total_users = sum(n_users)) %>%
          mutate(
            parent = "Selected surveys",
            label = paste0("<B>", year, ": ", total_surveys, " surveys by ", total_users, " users</B>"),
            id = year,
            value = total_surveys)
        
        df3 <- df1 %>%
          bind_rows(df2) %>%
          arrange(id)
        
        df3 %>%
          plot_ly(
            type = "treemap",
            ids = ~ id,
            labels = ~ label,
            values = ~ value,
            parents = ~ parent,
            hoverinfo = "label",
            marker = list(colorscale = "Greens", reversescale = T),
            branchvalues = "total"
          )
      })
    }
  )
}
