# server.R

server <- function(input, output, session) {
  # Filtered surveys ----

  filter_returns <- surveyFiltersServer()

  filtered_surveys <- reactive({
    filter_returns$wide()
  })

  filtered_surveys_long <- reactive({
    df <- filter_returns$long()

    if (input$group_wild) {
      df |>
        filter(bee_name %in% wildbee_names) |>
        droplevels()
    } else {
      df |>
        filter(bee_name != "Wild bees") |>
        droplevels()
    }
  })

  # Module servers ----

  # species composition pie charts
  speciesCompServer(
    cur_surveys_long = filtered_surveys_long
  )

  # bee activity by date
  activityByDateServer(
    data = filtered_surveys,
    data_long = filtered_surveys_long
  )

  # number of surveys by date
  surveysByDateServer(
    data = filtered_surveys
  )

  # bee activity by date
  activityByHabitatServer(
    data_long = filtered_surveys_long
  )

  # bee activity by management type
  activityByMgmtServer(
    data_long = filtered_surveys_long
  )

  # bee activity by crop type
  activityByCropServer(
    data_long = filtered_surveys_long
  )

  # map showing surveys or bee activity
  activityMapServer(
    data = filtered_surveys,
    data_long = filtered_surveys_long
  )

  # data table
  dataTableServer(
    data_long = filtered_surveys_long
  )

  # user stats
  userStatsServer(
    data = filtered_surveys
  )
}
