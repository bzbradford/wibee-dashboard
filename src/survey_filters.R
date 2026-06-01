## Survey filters module ##

# Survey filters UI ----
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
          p(em(
            "Select which year or years of survey data you want to see. After changing your year selections, grid point selections may have changed on the map."
          )),
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
            em(
              "Click on individual grid cell(s) to show only results from those areas. Note: some surveys are from outside Wisconsin. Click 'Select all' to see them."
            )
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
            em(
              "Filter survey data by selecting which User IDs you want to show data from. You can find your User ID in the WiBee app under Profile. Add one at a time, or separated by commas. The selected users list will show the total number of surveys submitted by that User ID."
            )
          ),
          wellPanel(
            fluidRow(
              column(
                6,
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
              column(
                6,
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
          p(
            em(
              "Filter survey data by selecting which date range you want to see data for."
            ),
            style = "margin-bottom:.5em"
          ),
          wellPanel(
            sliderInput(
              inputId = ns("date_range"),
              label = "Date range (across all selected years):",
              min = date_slider_min,
              max = date_slider_max,
              value = c(date_slider_min, date_slider_max),
              width = "100%",
              timeFormat = "%b %d"
            ),
            div(
              style = "margin-top:15px",
              actionButton(ns("set_date_spring"), "Spring"),
              actionButton(ns("set_date_summer"), "Summer"),
              actionButton(ns("set_date_fall"), "Fall"),
              actionButton(ns("reset_date"), "Reset")
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
            em(
              "Filter survey data by selecting which habitats or management types you want to see surveys for. Number of matching surveys for each habitat or reported management practice is shown in parentheses."
            )
          ),
          wellPanel(
            fixedRow(
              column(
                6,
                checkboxGroupInput(
                  inputId = ns("which_habitat"),
                  label = "Survey habitat:",
                  choiceNames = levels(habitats$label),
                  choiceValues = habitats$type,
                  selected = habitats$type
                ),
                div(
                  actionButton(ns("which_habitat_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_habitat_none"), "None"),
                  style = "display:inline-block"
                )
              ),
              column(
                6,
                checkboxGroupInput(
                  inputId = ns("which_mgmt"),
                  label = "Management type:",
                  choiceNames = levels(managements$label),
                  choiceValues = managements$type,
                  selected = managements$type
                ),
                div(
                  actionButton(ns("which_mgmt_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_mgmt_none"), "None"),
                  style = "display:inline-block"
                )
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
            em(
              "Filter survey data by selecting which crops, focal plants (featured plants for surveys shown in the app), or other non-crop flowering plants you want to see data for. Number of matching surveys for each plant is shown in parentheses."
            )
          ),
          wellPanel(
            fixedRow(
              column(
                3,
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
              column(
                4,
                checkboxGroupInput(
                  inputId = ns("which_focal_noncrops"),
                  label = "Focal non-crop plants:",
                  choiceNames = lapply(
                    as.list(levels(focal_noncrops$label)),
                    HTML
                  ),
                  choiceValues = focal_noncrops$type,
                  selected = focal_noncrops$type
                ),
                div(
                  actionButton(ns("which_focal_noncrops_all"), "All"),
                  style = "display:inline-block"
                ),
                div(
                  actionButton(ns("which_focal_noncrops_none"), "None"),
                  style = "display:inline-block"
                )
              ),
              column(
                5,
                checkboxGroupInput(
                  inputId = ns("which_noncrops"),
                  label = "Other non-crop plant:",
                  choiceNames = lapply(
                    as.list(levels(select_noncrops$label)),
                    HTML
                  ),
                  choiceValues = select_noncrops$type,
                  selected = select_noncrops$type
                ),
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
                actionButton(
                  ns("select_no_plants"),
                  "Clear all plant selections"
                ),
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


# Survey filters server ----
#' requires global vars:
#' - surveys
#' - map_pts_wi
#' @return the filtered surveys data frame
surveyFiltersServer <- function(data) {
  moduleServer(
    id = "surveyFilters",
    function(input, output, session) {
      ns <- session$ns

      # Reactive values ----

      map_selection <- reactiveVal(map_pts_wi)
      selected_users <- reactiveVal()

      # Filter by year ----

      surveys_by_year <- reactive({
        surveys |>
          filter(year %in% input$years)
      })

      observeEvent(input$years, {
        resetMap()
      })

      output$year_survey_count <- renderText({
        paste(
          nrow(surveys_by_year()),
          "out of",
          nrow(surveys),
          "surveys selected."
        )
      })

      resetYears <- function() {
        updateCheckboxGroupInput(
          inputId = "years",
          selected = year_summary$year
        )
      }

      # Filter by map grid ----

      surveys_by_loc <- reactive({
        surveys_by_year() |>
          filter(grid_pt %in% map_selection())
      })

      ## Initialize map ----

      map_grids <- reactive({
        surveys_by_year() |>
          mutate(lat = lat_rnd, lng = lng_rnd) |>
          summarise(
            n_surveys = n(),
            n_users = n_distinct(user_id),
            total_visits = sum(total_visits),
            hb = round(sum(honeybee) / total_visits * 100),
            wb = round(sum(wild_bee) / total_visits * 100),
            nb = round(sum(non_bee) / total_visits * 100),
            .by = c(lat, lng, grid_pt, inwi)
          ) |>
          mutate(
            label = str_glue(
              "
              <strong>Grid point [{grid_pt}]</strong><br>
              {n_surveys} surveys by {n_users} users<br>
              {total_visits} total pollinators
            "
            ),
            label = if_else(
              total_visits > 0,
              str_glue(
                "
                {label}<br>
                {hb}% honeybees<br>
                {wb}% wild bees<br>
                {nb}% non-bees
              "
              ),
              label
            ),
            label = lapply(label, shiny::HTML)
          )
      })

      # Initial map condition
      output$map <- renderLeaflet({
        leaflet() |>
          addTiles() |>
          addMapPane("base_grids", zIndex = 410) |>
          addMapPane("selected_grids", zIndex = 420) |>
          setView(lng = -89.7, lat = 44.8, zoom = 7) |>
          addEasyButtonBar(
            easyButton(
              position = "topleft",
              icon = "fa-crosshairs",
              title = "Get my location",
              onClick = JS(
                "
                function(btn, map) {
                  map.locate({
                    setView: true,
                    enableHighAccuracy: false,
                    maxZoom: 12
                  })
                }
              "
              )
            ),
            easyButton(
              position = "topleft",
              icon = "fa-globe",
              title = "Reset map view",
              onClick = JS(
                "function(btn, map) { map.setView([44.8, -89.7], 7) }"
              )
            )
          )
      })

      observeEvent(map_grids(), {
        leafletProxy("map") |>
          clearGroup("base_grids") |>
          addRectangles(
            data = map_grids(),
            lng1 = ~ lng - .05,
            lng2 = ~ lng + .05,
            lat1 = ~ lat - .05,
            lat2 = ~ lat + .05,
            layerId = ~grid_pt,
            group = "base_grids",
            label = ~label,
            color = "orange",
            weight = 1,
            opacity = 1,
            fillColor = "yellow",
            fillOpacity = .25,
            highlight = highlightOptions(
              weight = 3,
              color = "red",
              fillColor = "orange",
              fillOpacity = 0.7
            ),
            options = pathOptions(pane = "base_grids")
          )
      })

      # deselect grids that are no longer available
      observeEvent(map_grids(), {
        new_selection <- intersect(map_selection(), map_grids()$grid_pt)
        map_selection(new_selection)
      })

      # reactive portion of map showing selected grids
      observeEvent(map_selection(), {
        leafletProxy("map") |>
          clearGroup("selected_grids") |>
          addRectangles(
            data = filter(map_grids(), grid_pt %in% map_selection()),
            lng1 = ~ lng - .05,
            lng2 = ~ lng + .05,
            lat1 = ~ lat - .05,
            lat2 = ~ lat + .05,
            layerId = ~ paste(grid_pt, "selected"),
            group = "selected_grids",
            label = ~label,
            color = "red",
            weight = 1,
            opacity = 1,
            fillColor = "orange",
            fillOpacity = .25,
            highlight = highlightOptions(
              color = "red",
              weight = 3,
              fillColor = "orange",
              fillOpacity = 0.7
            ),
            options = pathOptions(pane = "selected_grids")
          )
      })

      ## Handle map interactions ----

      # handle adding and subtracting grids from selection
      observeEvent(input$map_shape_click, {
        click <- input$map_shape_click
        grid_pt <- str_remove(click$id, " selected")

        # clicked on a selected grid
        if (grepl("selected", click$id, fixed = T)) {
          # on first click, select only that grid
          if (setequal(map_selection(), map_pts_wi)) {
            map_selection(grid_pt)
          } else {
            leafletProxy("map") |>
              removeShape(click$id)
            old_sel <- map_selection()
            new_sel <- old_sel[old_sel != grid_pt]
            map_selection(new_sel)
          }
        } else {
          new_sel <- c(map_selection(), grid_pt)
          map_selection(new_sel)
        }
      })

      ## Handle map buttons ----

      # zoom to show all data
      observeEvent(input$map_zoom_all, {
        leafletProxy("map") |>
          fitBounds(
            lng1 = min(map_grids()$lng),
            lat1 = min(map_grids()$lat),
            lng2 = max(map_grids()$lng),
            lat2 = max(map_grids()$lat)
          )
        map_selection(map_grids()$grid_pt)
      })

      # select grids visible in map window
      observeEvent(input$map_select_visible, {
        bounds <- input$map_bounds
        new_pts <- map_grids() |>
          filter(
            lng > bounds$west,
            lng < bounds$east,
            lat > bounds$south,
            lat < bounds$north
          ) |>
          pull(grid_pt)
        map_selection(new_pts)
      })

      # clear selection
      observeEvent(input$map_clear_selection, {
        leafletProxy("map") |>
          clearGroup("selected_grids")
        map_selection(NULL)
      })

      observeEvent(input$map_reset, resetMap())

      # reset view to show and select Wisconsin points
      resetMap <- function() {
        resetMapGrids()
        resetMapView()
      }

      resetMapGrids <- function() {
        new_sel <- filter(map_grids(), inwi)$grid_pt
        map_selection(new_sel)
      }

      resetMapView <- function() {
        leafletProxy("map") |>
          setView(lng = -89.7, lat = 44.8, zoom = 7)
      }

      ## Survey count text ----

      output$survey_count_loc <- renderText({
        if (is.null(map_selection())) {
          "0 zones and 0 surveys selected on the map."
        } else {
          paste(
            length(map_selection()),
            "zones and ",
            nrow(surveys_by_loc()),
            " surveys selected on the map."
          )
        }
      })

      # Filter by user id ----

      surveys_by_user <- reactive({
        if (is.null(selected_users())) {
          surveys_by_loc()
        } else {
          filter(surveys_by_loc(), user_id %in% selected_users())
        }
      })

      ## UI ----

      output$selected_users_display <- renderUI({
        if (is.null(selected_users())) {
          p(em("No users selected, showing surveys by all users."))
        } else {
          p(
            lapply(selected_users(), function(i) {
              list(
                strong(i),
                paste(":", sum(surveys$user_id == i), "surveys"),
                br()
              )
            })
          )
        }
      })

      observeEvent(input$add_user_id, {
        tryCatch(
          {
            ids <- suppressWarnings(parse_number(unlist(strsplit(
              input$user_id,
              ","
            ))))
          },
          error = function(cond) {
            updateTextInput(inputId = "user_id", value = "")
            return()
          }
        )
        valid_ids <- sort(intersect(ids, user_ids))
        if (length(valid_ids) > 0) {
          if (is.null(selected_users())) {
            selected_users(valid_ids)
          } else {
            selected_users(
              sort(union(selected_users(), valid_ids))
            )
          }
        }
        updateTextInput(inputId = "user_id", value = "")
      })

      ## Reset ----

      observeEvent(input$reset_user_ids, resetUserIds())

      resetUserIds <- function() {
        updateTextInput(inputId = "user_id", value = "")
        selected_users(NULL)
      }

      output$survey_count_users <- renderText({
        paste(
          nrow(surveys_by_user()),
          "out of",
          nrow(surveys_by_loc()),
          "surveys match your user selections."
        )
      })

      # Filter by date of survey ----

      resetDate <- function(min = date_slider_min, max = date_slider_max) {
        updateSliderInput(
          inputId = "date_range",
          value = c(min, max),
          timeFormat = "%b %d"
        )
      }

      surveys_by_date <- reactive({
        surveys_by_user() |>
          filter(
            year %in% input$years,
            between(doy, yday(input$date_range[1]), yday(input$date_range[2]))
          )
      })

      ## Action buttons ----

      observeEvent(
        input$set_date_spring,
        resetDate(
          max = as.Date(format(Sys.Date(), "%Y-05-31"))
        )
      )

      observeEvent(
        input$set_date_summer,
        resetDate(
          min = as.Date(format(Sys.Date(), "%Y-06-01")),
          max = as.Date(format(Sys.Date(), "%Y-08-31"))
        )
      )

      observeEvent(
        input$set_date_fall,
        resetDate(
          min = as.Date(format(Sys.Date(), "%Y-09-01"))
        )
      )

      observeEvent(input$reset_date, resetDate())

      ## Survey count text ----

      output$survey_count_date <- renderText({
        paste(
          nrow(surveys_by_date()),
          "out of",
          nrow(surveys_by_user()),
          "surveys match your date selections."
        )
      })

      # Filter by survey attributes ----

      surveys_by_attr <- reactive({
        surveys_by_date() |>
          filter(
            habitat %in% input$which_habitat,
            management %in% input$which_mgmt
          ) |>
          droplevels()
      })

      ## Habitats ----

      # Generate labels
      habitat_labels <- reactive({
        habitats |>
          left_join(
            count(surveys_by_user(), habitat, .drop = F),
            by = c("type" = "habitat")
          ) |>
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")")
          )
      })

      # Update labels on checkbox
      observeEvent(habitat_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_habitat",
          choiceNames = habitat_labels()$box_label,
          choiceValues = habitat_labels()$type,
          selected = input$which_habitat
        )
      })

      # Buttons
      observeEvent(input$which_habitat_all, selectAllHabitats())
      observeEvent(input$which_habitat_none, selectNoHabitats())

      selectAllHabitats <- function() {
        updateCheckboxGroupInput(
          inputId = "which_habitat",
          selected = habitats$type
        )
      }

      selectNoHabitats <- function() {
        updateCheckboxGroupInput(
          inputId = "which_habitat",
          selected = ""
        )
      }

      ## Managements ----

      # Generate labels
      mgmt_labels <- reactive({
        managements |>
          left_join(
            count(surveys_by_user(), management, .drop = F),
            by = c("type" = "management")
          ) |>
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")")
          )
      })

      # Update labels
      observeEvent(mgmt_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_mgmt",
          choiceNames = mgmt_labels()$box_label,
          choiceValues = mgmt_labels()$type,
          selected = input$which_mgmt
        )
      })

      # Buttons
      observeEvent(input$which_mgmt_all, selectAllMgmts())
      observeEvent(input$which_mgmt_none, selectNoMgmts())

      selectAllMgmts <- function() {
        updateCheckboxGroupInput(
          inputId = "which_mgmt",
          selected = managements$type
        )
      }

      selectNoMgmts <- function() {
        updateCheckboxGroupInput(
          inputId = "which_mgmt",
          selected = ""
        )
      }

      ## Survey count text ----

      output$survey_count_site <- renderText({
        paste(
          nrow(surveys_by_attr()),
          "out of",
          nrow(surveys_by_date()),
          "surveys match your habitat and management selections."
        )
      })

      # Filter by crop/plant ----

      surveys_by_plant <- reactive({
        surveys_by_attr() |>
          filter(
            plant_type %in%
              c(
                input$which_crops,
                input$which_focal_noncrops,
                input$which_noncrops
              )
          ) |>
          droplevels()
      })

      ## Checkboxes ----

      # Generate crop labels
      crop_labels <- reactive({
        select_crops |>
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")
          ) |>
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")")
          )
      })

      # Update labels
      observeEvent(crop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_crops",
          choiceNames = crop_labels()$box_label,
          choiceValues = crop_labels()$type,
          selected = input$which_crops
        )
      })

      # Generate focal plant labels
      focal_noncrop_labels <- reactive({
        focal_noncrops |>
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")
          ) |>
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")")
          )
      })

      # Update labels
      observeEvent(focal_noncrop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          choiceNames = lapply(as.list(focal_noncrop_labels()$box_label), HTML),
          choiceValues = focal_noncrop_labels()$type,
          selected = input$which_focal_noncrops
        )
      })

      # Generate non-crop plant labels
      noncrop_labels <- reactive({
        select_noncrops |>
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")
          ) |>
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")")
          )
      })

      # Update labels
      observeEvent(noncrop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          choiceNames = lapply(as.list(noncrop_labels()$box_label), HTML),
          choiceValues = noncrop_labels()$type,
          selected = input$which_noncrops
        )
      })

      ## Action buttons ----

      observeEvent(
        input$which_crops_all,
        updateCheckboxGroupInput(
          inputId = "which_crops",
          selected = select_crops$type
        )
      )

      observeEvent(
        input$which_crops_none,
        updateCheckboxGroupInput(
          inputId = "which_crops",
          selected = ""
        )
      )

      observeEvent(
        input$which_focal_noncrops_all,
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          selected = focal_noncrops$type
        )
      )

      observeEvent(
        input$which_focal_noncrops_none,
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          selected = ""
        )
      )

      observeEvent(
        input$which_noncrops_all,
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          selected = select_noncrops$type
        )
      )

      observeEvent(
        input$which_noncrops_none,
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          selected = ""
        )
      )

      observeEvent(
        input$select_all_plants,
        selectAllPlants()
      )

      observeEvent(
        input$select_no_plants,
        selectNoPlants()
      )

      selectAllPlants <- function() {
        updateCheckboxGroupInput(
          inputId = "which_crops",
          selected = select_crops$type
        )
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          selected = focal_noncrops$type
        )
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          selected = select_noncrops$type
        )
      }

      selectNoPlants <- function() {
        updateCheckboxGroupInput(
          inputId = "which_crops",
          selected = ""
        )
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          selected = ""
        )
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          selected = ""
        )
      }

      ## Survey count text ----

      output$survey_count_plant <- renderText({
        paste(
          nrow(surveys_by_plant()),
          "out of",
          nrow(surveys_by_attr()),
          "surveys match your plant selections."
        )
      })

      # Final survey count ----

      filtered_surveys <- reactive({
        surveys_by_plant()
      })

      filtered_surveys_long <- reactive({
        filtered_surveys() |>
          pivot_longer(
            cols = bees$type,
            names_to = "bee",
            values_to = "count"
          ) |>
          left_join(bee_join, by = "bee")
      })

      output$survey_count_final <- renderText({
        paste(
          nrow(filtered_surveys()),
          "out of",
          nrow(surveys),
          "total surveys match all of your criteria."
        )
      })

      # Master reset button ----

      observeEvent(input$reset, {
        selectAllPlants()
        selectAllHabitats()
        selectAllMgmts()
        resetDate()
        resetUserIds()
        resetMap()
        resetYears()
      })

      # Final return values ----

      return(
        list(
          wide = reactive({
            filtered_surveys()
          }),
          long = reactive({
            filtered_surveys_long()
          })
        )
      )
    }
  )
}
