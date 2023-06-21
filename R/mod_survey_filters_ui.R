## Survey filters UI ##

surveyFiltersUI <- function() {
  ns <- NS("surveyFilters")
  
  tagList(
    bsCollapse(
      multiple = TRUE,
      open = "map",
      
      
      ## By year ----
      
      bsCollapsePanel(
        value = "years",
        style = "primary",
        title = "Select year(s) to show",
        tagList(
          p(em("Select which year or years of survey data you want to see. After changing your year selections, grid point selections may have changed on the map.")),
          div(
            class = "well",
            style = "padding-bottom: 0px;",
            checkboxGroupInput(
              inputId = ns("years"),
              label = "Surveys from year:",
              choiceNames = lapply(year_summary$label, HTML),
              choiceValues = year_summary$year,
              selected = year_summary$year
            )
          ),
          div(
            style = "text-align: center; font-weight: bold;",
            textOutput(ns("survey_count"))
          )
        )
      ),
      
      
      ## By map grid ----
      
      bsCollapsePanel(
        value = "map",
        style = "primary",
        title = "Select survey locations on the map",
        tagList(
          p(
            style = "margin-top:.5em; margin-bottom:.5em",
            em("Click on individual grid cell(s) to show only results from those areas. Note: some surveys are from outside Wisconsin. Click 'Select all' to see them.")
          ),
          leafletOutput(ns("map"), height = 600),
          div(
            class = "flex-row map-btns",
            div(actionButton(ns("map_select_visible"), "Select visible")),
            div(actionButton(ns("map_zoom_all"), "Select all")),
            div(actionButton(ns("map_clear_selection"), "Clear selection")),
            div(actionButton(ns("map_reset"), "Reset map")),
            div(
              style = "max-width: 60%;",
              strong(textOutput(ns("survey_count_loc")))
            )
          )
        )
      ),
      
      
      ## By user ID ----
      
      bsCollapsePanel(
        value = "users",
        style = "primary",
        title = "Show surveys from specific user(s)",
        tagList(
          p(
            style = "margin-bottom:.5em",
            em("Filter survey data by selecting which User IDs you want to show data from. You can find your User ID in the WiBee app under Profile. Add one at a time, or separated by commas. The selected users list will show the total number of surveys submitted by that User ID.")
          ),
          wellPanel(
            fluidRow(
              column(6,
                textInput(
                  inputId = ns("user_id"),
                  label = "User ID:",
                  value = ""
                ),
                div(
                  style = "margin-top:15px",
                  actionButton(ns("add_user_id"), "Add ID to list"),
                  actionButton(ns("reset_user_ids"), "Reset list of IDs"),
                )
              ),
              column(6,
                p(strong("Selected users:")),
                uiOutput(ns("selected_users_display"))
              )
            ),
          ),
          div(
            style = "text-align: center; font-weight: bold;",
            textOutput(ns("survey_count_users"))
          )
        )
      ),
      
      
      ## By date range ----
      
      bsCollapsePanel(
        value = "dates",
        style = "primary",
        title = "Select survey date range",
        tagList(
          p(em("Filter survey data by selecting which date range you want to see data for."), style = "margin-bottom:.5em"),
          wellPanel(
            sliderInput(
              inputId = ns("date_range"),
              label = "Date range (across all selected years):",
              min = date_slider_min,
              max = date_slider_max,
              value = c(date_slider_min, date_slider_max),
              width = "100%",
              timeFormat = "%b %d"),
            div(
              style = "margin-top:15px",
              actionButton(ns("reset_date"), "Reset date")
            ),
          ),
          div(
            style = "text-align: center; font-weight: bold;",
            textOutput(ns("survey_count_date"))
          )
        )
      ),
      
      
      ## By habitat/management/pollinator ----
      
      bsCollapsePanel(
        value = "surveys",
        style = "primary",
        title = "Select surveys by habitat or management type",
        tagList(
          p(
            style = "margin-bottom:.5em",
            em("Filter survey data by selecting which habitats or management types you want to see surveys for. Number of matching surveys for each habitat or reported management practice is shown in parentheses.")
          ),
          wellPanel(
            fixedRow(
              column(6,
                checkboxGroupInput(
                  inputId = ns("which_habitat"),
                  label = "Survey habitat:",
                  choiceNames = levels(habitats$label),
                  choiceValues = habitats$type,
                  selected = habitats$type
                ),
                div(actionButton(ns("which_habitat_all"), "All"), style = "display:inline-block"),
                div(actionButton(ns("which_habitat_none"), "None"), style = "display:inline-block")
              ),
              column(6,
                checkboxGroupInput(
                  inputId = ns("which_mgmt"),
                  label = "Management type:",
                  choiceNames = levels(managements$label),
                  choiceValues = managements$type,
                  selected = managements$type
                ),
                div(actionButton(ns("which_mgmt_all"), "All"), style = "display:inline-block"),
                div(actionButton(ns("which_mgmt_none"), "None"), style = "display:inline-block")
              )
            )
          ),
          div(
            style = "text-align: center; font-weight: bold;",
            textOutput(ns("survey_count_site"))
          )
        )
      ),
      
      
      ## By crop/plant ----
      
      bsCollapsePanel(
        value = "plants",
        style = "primary",
        title = "Select crop(s) or flowering plant(s) observed during surveys",
        list(
          p(
            style = "margin-bottom:.5em",
            em("Filter survey data by selecting which crops, focal plants (featured plants for surveys shown in the app), or other non-crop flowering plants you want to see data for. Number of matching surveys for each plant is shown in parentheses.")
          ),
          wellPanel(
            fixedRow(
              column(3,
                checkboxGroupInput(
                  inputId = ns("which_crops"),
                  label = "Crops:",
                  choiceNames = levels(select_crops$label),
                  choiceValues = select_crops$type,
                  selected = select_crops$type
                ),
                div(
                  actionButton(ns("which_crops_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_crops_none"), "None"),
                  style = "display:inline-block"
                )
              ),
              column(4,
                checkboxGroupInput(
                  inputId = ns("which_focal_noncrops"),
                  label = "Focal non-crop plants:",
                  choiceNames = lapply(as.list(levels(focal_noncrops$label)), HTML),
                  choiceValues = focal_noncrops$type,
                  selected = focal_noncrops$type),
                div(
                  actionButton(ns("which_focal_noncrops_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_focal_noncrops_none"), "None"),
                  style = "display:inline-block"
                )
              ),
              column(5,
                checkboxGroupInput(
                  inputId = ns("which_noncrops"),
                  label = "Other non-crop plant:",
                  choiceNames = lapply(as.list(levels(select_noncrops$label)), HTML),
                  choiceValues = select_noncrops$type,
                  selected = select_noncrops$type),
                div(
                  actionButton(ns("which_noncrops_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_noncrops_none"), "None"),
                  style = "display:inline-block"
                )
              )
            ),
            fixedRow(
              align = "center",
              style = "margin-top: 1em;",
              div(
                actionButton(ns("select_all_plants"), "Select all plants"),
                style = "display:inline-block"
              ),
              div(
                actionButton(ns("select_no_plants"), "Clear all plant selections"),
                style = "display:inline-block"
              )
            )
          ),
          div(
            style = "text-align: center; font-weight: bold;",
            textOutput(ns("survey_count_plant"))
          )
        )
      )
    ),
    
    # Show final selected surveys count ----
    
    div(
      strong(textOutput(ns("survey_count_final"))),
      class = "well",
      style = "text-align: center; font-size: larger;"
    )
  )
}
