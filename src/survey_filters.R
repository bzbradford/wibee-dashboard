## Survey filters module ##

# Filter type identifiers, shared between the bsCollapsePanel `value`s (so panel
# styling can target the right panel) and the names of the row-mask reactives.
filter_names <- c("years", "map", "users", "dates", "surveys", "plants")

# Survey filters UI ----
surveyFiltersUI <- function() {
  ns <- NS("surveyFilters")

  # A collapse panel title with a reactive "(selected/total)" suffix that shows
  # how much of the filter is currently in use.
  panel_title <- function(text, suffix_id, icon = NULL) {
    span(
      if (!is.null(icon)) {
        span(
          # class = "panel-title-icon",
          style = "display: inline-block; margin-right: 5px;",
          shiny::icon(icon),
        )
      },
      text,
      " ",
      span(
        class = "filter-suffix",
        textOutput(ns(suffix_id), inline = TRUE)
      )
    )
  }

  tagList(
    bsCollapse(
      id = ns("collapse"),
      multiple = TRUE,
      open = "map",

      ## By map grid ----

      bsCollapsePanel(
        value = "map",
        title = panel_title(
          "Filter by survey location",
          "suffix_map",
          icon = "map"
        ),
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
        title = panel_title(
          "Filter by user ID",
          "suffix_users",
          icon = "person"
        ),
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

      ## By year ----

      bsCollapsePanel(
        value = "years",
        title = panel_title(
          "Filter by year",
          "suffix_years",
          icon = "clock"
        ),
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

      ## By date range ----

      bsCollapsePanel(
        value = "dates",
        title = panel_title(
          "Filter by survey date range",
          "suffix_dates",
          icon = "calendar"
        ),
        tagList(
          p(
            style = "margin-bottom: 0.5em;",
            em(
              "Filter survey data by selecting which date range you want to see data for."
            )
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
              style = "margin-top: 15px;",
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
        title = panel_title(
          "Filter by habitat or management type",
          "suffix_surveys",
          icon = "tree-city"
        ),
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
        title = panel_title(
          "Filter by crop or flowering plant observed",
          "suffix_plants",
          icon = "seedling"
        ),
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
#' - filter_names
#' @return the filtered surveys data frame
surveyFiltersServer <- function(data) {
  moduleServer(
    id = "surveyFilters",
    function(input, output, session) {
      ns <- session$ns

      # Reactive values ----

      rv <- reactiveValues(
        sel_grids = map_pts_wi,
        sel_users = NULL
      )

      # Static geometry for every grid point in the data. Used to draw selected
      # grids that are currently precluded by other filters (so they can show as
      # grey placeholders and stay in the selection until they reappear).
      all_grid_geo <- distinct(
        surveys,
        grid_pt,
        lat = lat_rnd,
        lng = lng_rnd,
        inwi
      )

      # Filter row masks ----
      #
      # Each filter is a logical vector aligned to the rows of the full `surveys`
      # table. Because every mask spans the same rows, combining them (and
      # leaving one out for per-option counts) is just `&`-ing the vectors. All
      # filters always apply at once; "how restrictive" a filter is drives its
      # panel style and title suffix, not whether it applies.

      mask_years <- reactive({
        surveys$year %in% input$years
      })

      mask_map <- reactive({
        surveys$grid_pt %in% rv$sel_grids
      })

      mask_users <- reactive({
        if (is.null(rv$sel_users)) {
          rep(TRUE, nrow(surveys))
        } else {
          surveys$user_id %in% rv$sel_users
        }
      })

      mask_dates <- reactive({
        between(
          surveys$doy,
          yday(input$date_range[1]),
          yday(input$date_range[2])
        )
      })

      mask_surveys <- reactive({
        surveys$habitat %in%
          input$which_habitat &
          surveys$management %in% input$which_mgmt
      })

      mask_plants <- reactive({
        surveys$plant_type %in%
          c(
            input$which_crops,
            input$which_focal_noncrops,
            input$which_noncrops
          )
      })

      all_masks <- list(
        years = mask_years,
        map = mask_map,
        users = mask_users,
        dates = mask_dates,
        surveys = mask_surveys,
        plants = mask_plants
      )

      # Combine all masks, optionally leaving one filter out (used to compute the
      # per-option "if I changed only this filter" counts). Reads reactives, so
      # must be called inside a reactive context.
      combined_mask <- function(exclude = NULL) {
        use <- setdiff(filter_names, exclude)
        masks <- lapply(use, function(f) all_masks[[f]]())
        Reduce(`&`, masks)
      }

      # Final filtered surveys (all filters applied at once)
      filtered_surveys <- reactive({
        droplevels(surveys[combined_mask(), ])
      })

      # Leave-one-out bases: surveys matching every filter EXCEPT the named one.
      # Used as the denominator and per-option counts for that filter's panel.
      # Kept un-droplevels'd so count(.drop = FALSE) still shows zero counts.
      base_years <- reactive(surveys[combined_mask(exclude = "years"), ])
      base_map <- reactive(surveys[combined_mask(exclude = "map"), ])
      base_users <- reactive(surveys[combined_mask(exclude = "users"), ])
      base_dates <- reactive(surveys[combined_mask(exclude = "dates"), ])
      base_surveys <- reactive(surveys[combined_mask(exclude = "surveys"), ])
      base_plants <- reactive(surveys[combined_mask(exclude = "plants"), ])

      # Panel styling + title suffixes ----
      #
      # A panel turns "primary" (highlighted) when its filter is actually
      # narrowing the data, and shows a "(selected/total)" suffix in its title.

      n_sel <- function(vals, valid) {
        length(intersect(vals, as.character(valid)))
      }

      output$suffix_years <- renderText({
        sprintf("[%d/%d selected]", length(input$years), nrow(year_summary))
      })

      output$suffix_map <- renderText({
        grids <- map_grids()
        sprintf(
          "[%d/%d zones selected]",
          length(intersect(rv$sel_grids, grids$grid_pt)),
          nrow(grids)
        )
      })

      output$suffix_users <- renderText({
        n <- length(rv$sel_users)
        if (n == 0) {
          "[all users selected]"
        } else {
          sprintf("[%d user%s selected]", n, if (n == 1) "" else "s")
        }
      })

      output$suffix_dates <- renderText({
        if (dates_full()) {
          "[all dates selected]"
        } else {
          sprintf(
            "[%s - %s]",
            format(input$date_range[1], "%b %d"),
            format(input$date_range[2], "%b %d")
          )
        }
      })

      output$suffix_surveys <- renderText({
        sel <- n_sel(input$which_habitat, habitats$type) +
          n_sel(input$which_mgmt, managements$type)
        sprintf("[%d/%d selected]", sel, nrow(habitats) + nrow(managements))
      })

      output$suffix_plants <- renderText({
        sel <- n_sel(input$which_crops, select_crops$type) +
          n_sel(input$which_focal_noncrops, focal_noncrops$type) +
          n_sel(input$which_noncrops, select_noncrops$type)
        tot <- nrow(select_crops) + nrow(focal_noncrops) + nrow(select_noncrops)
        sprintf("[%d/%d selected]", sel, tot)
      })

      dates_full <- reactive({
        input$date_range[1] <= date_slider_min &&
          input$date_range[2] >= date_slider_max
      })

      # Whether each filter is actively restricting results -> panel style
      panel_styles <- reactive({
        grids <- map_grids()
        applied <- c(
          years = length(input$years) < nrow(year_summary),
          map = length(intersect(rv$sel_grids, grids$grid_pt)) < nrow(grids),
          users = length(rv$sel_users) > 0,
          dates = !dates_full(),
          surveys = n_sel(input$which_habitat, habitats$type) <
            nrow(habitats) ||
            n_sel(input$which_mgmt, managements$type) < nrow(managements),
          plants = (n_sel(input$which_crops, select_crops$type) +
            n_sel(input$which_focal_noncrops, focal_noncrops$type) +
            n_sel(input$which_noncrops, select_noncrops$type)) <
            (nrow(select_crops) + nrow(focal_noncrops) + nrow(select_noncrops))
        )
        lapply(applied, function(a) if (isTRUE(a)) "primary" else "default")
      })

      observeEvent(panel_styles(), {
        updateCollapse(session, "collapse", style = panel_styles())
      })

      # Filter by year ----

      output$survey_count <- renderText({
        paste(
          nrow(filtered_surveys()),
          "out of",
          nrow(base_years()),
          "surveys match your year selections."
        )
      })

      resetYears <- function() {
        updateCheckboxGroupInput(
          inputId = "years",
          selected = year_summary$year
        )
      }

      # Filter by map grid ----

      ## Initialize map ----

      # Map grid stats reflect every other filter (leave-one-out), debounced so
      # the rectangles don't redraw on every keystroke/slider tick.
      base_map_d <- debounce(base_map, 300)

      map_grids <- reactive({
        base_map_d() |>
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
          # one pane per grid state so z-order is fixed
          addMapPane("grids_s1", zIndex = 413) |>
          addMapPane("grids_s2", zIndex = 412) |>
          addMapPane("grids_s3", zIndex = 411) |>
          addMapPane("grids_s4", zIndex = 410) |>
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

      # Per-grid render table: one row for every grid in the data, carrying the
      # fill/border/label that reflect its current availability and selection.
      #   available + selected -> orange fill, red border
      #   available            -> yellow fill, orange border
      #   precluded + selected -> grey fill, grey border
      #   precluded            -> light grey fill, grey border
      grid_render <- reactive({
        grids <- map_grids()
        sel <- rv$sel_grids

        available <- grids |>
          mutate(
            grid_pt,
            lat,
            lng,
            label,
            available = TRUE,
            selected = grid_pt %in% sel,
            .keep = "none"
          )

        precluded <- all_grid_geo |>
          filter(!grid_pt %in% grids$grid_pt) |>
          mutate(
            grid_pt,
            lat,
            lng,
            selected = grid_pt %in% sel,
            label = lapply(
              paste(
                sprintf("Grid point [%s]<br>", grid_pt),
                if_else(
                  selected,
                  "Selected, but no surveys<br>match the current filters",
                  "No surveys match the current filters"
                )
              ),
              shiny::HTML
            ),
            available = FALSE,
            .keep = "none"
          )

        bind_rows(available, precluded) |>
          mutate(
            #' draw priority / pane:
            #'  1 = available + selected (top)
            #'  2 = available
            #'  3 = precluded + selected
            #'  4 = precluded + unselected (bottom)
            state = case_when(
              available & selected ~ 1L,
              available ~ 2L,
              selected ~ 3L,
              TRUE ~ 4L
            ),
            fill = c("orange", "yellow", "grey", "lightgrey")[state],
            fill_opacity = c(0.35, 0.3, 0.25, 0.25)[state],
            border = c("red", "orange", "grey", "grey")[state],
            weight = c(1.25, 1, 0.75, 0.5)[state],
            # a per-grid appearance signature, to detect what needs redrawing
            sig = paste(
              state,
              fill,
              border,
              weight,
              fill_opacity,
              vapply(
                label,
                function(x) paste0(as.character(x), collapse = ""),
                character(1)
              )
            )
          )
      })

      # Incremental draw: only (re)draw grids whose appearance changed since the
      # last render. grid_pt is the layerId, and re-adding a layer with an
      # existing layerId replaces it in place, so map clicks redraw one grid.
      prev_grid_sig <- reactiveVal(NULL)

      observe({
        gr <- grid_render()
        old <- prev_grid_sig()
        new <- setNames(gr$sig, gr$grid_pt)
        prev_grid_sig(new)

        if (is.null(old)) {
          changed <- gr$grid_pt
        } else {
          prev <- old[gr$grid_pt]
          changed <- gr$grid_pt[is.na(prev) | prev != new]
        }
        if (length(changed) == 0) {
          return()
        }

        to_draw <- filter(gr, grid_pt %in% changed)
        proxy <- leafletProxy("map")

        # one addRectangles call per state, each into its own pane so the
        # z-order is fixed regardless of which grids were redrawn
        for (s in sort(unique(to_draw$state))) {
          proxy <- proxy |>
            addRectangles(
              data = filter(to_draw, state == s),
              lng1 = ~ lng - 0.05,
              lng2 = ~ lng + 0.05,
              lat1 = ~ lat - 0.05,
              lat2 = ~ lat + 0.05,
              layerId = ~grid_pt,
              group = "grids",
              label = ~label,
              color = ~border,
              weight = ~weight,
              opacity = 1,
              fillColor = ~fill,
              fillOpacity = ~fill_opacity,
              highlight = highlightOptions(
                weight = 3,
                color = "red",
                fillColor = "red",
                fillOpacity = 0.5
                # bringToFront = TRUE # this introduced visual bugs in leaflet
              ),
              options = pathOptions(pane = paste0("grids_s", s))
            )
        }
      })

      ## Handle map interactions ----

      # toggle a grid in/out of the selection (works for available and grey grids
      # alike, since every grid shares its grid_pt as its layerId)
      observeEvent(input$map_shape_click, {
        gp <- input$map_shape_click$id
        cur_grids <- rv$sel_grids

        rv$sel_grids <- if (gp %in% cur_grids) {
          # first click from the all-Wisconsin default narrows to just this grid,
          # otherwise toggle it off
          if (setequal(cur_grids, map_pts_wi)) {
            gp
          } else {
            setdiff(cur_grids, gp)
          }
        } else {
          union(cur_grids, gp)
        }
      })

      ## Handle map buttons ----

      # zoom to show all data
      observeEvent(input$map_zoom_all, {
        grids <- map_grids()
        if (nrow(grids) == 0) {
          return()
        }
        leafletProxy("map") |>
          fitBounds(
            lng1 = min(grids$lng),
            lat1 = min(grids$lat),
            lng2 = max(grids$lng),
            lat2 = max(grids$lat)
          )
        rv$sel_grids <- grids$grid_pt
      })

      # select every grid visible in the map window, including precluded (grey)
      # ones, so they are staged to reactivate when other filters are relaxed
      observeEvent(input$map_select_visible, {
        bounds <- input$map_bounds
        new_pts <- all_grid_geo |>
          filter(
            lng > bounds$west,
            lng < bounds$east,
            lat > bounds$south,
            lat < bounds$north
          ) |>
          pull(grid_pt)
        rv$sel_grids <- new_pts
      })

      # clear selection (overlay observer redraws the empty selection)
      observeEvent(input$map_clear_selection, {
        rv$sel_grids <- NULL
      })

      observeEvent(input$map_reset, resetMap())

      # reset view to show and select Wisconsin points
      resetMap <- function() {
        resetMapGrids()
        resetMapView()
      }

      resetMapGrids <- function() {
        # restore the default Wisconsin selection; precluded grids stay selected
        # (shown grey) and reactivate as other filters are relaxed
        rv$sel_grids <- map_pts_wi
      }

      resetMapView <- function() {
        leafletProxy("map") |>
          setView(lng = -89.7, lat = 44.8, zoom = 7)
      }

      ## Survey count text ----

      output$survey_count_loc <- renderText({
        paste0(
          length(rv$sel_grids),
          " zones and ",
          nrow(filtered_surveys()),
          " surveys selected on the map."
        )
      })

      # Filter by user id ----

      ## UI ----

      output$selected_users_display <- renderUI({
        if (is.null(rv$sel_users)) {
          p(em("No users selected, showing surveys by all users."))
        } else {
          p(
            lapply(rv$sel_users, function(i) {
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
          rv$sel_users <- if (is.null(rv$sel_users)) {
            valid_ids
          } else {
            sort(union(rv$sel_users, valid_ids))
          }
        }
        updateTextInput(inputId = "user_id", value = "")
      })

      ## Reset ----

      observeEvent(input$reset_user_ids, resetUserIds())

      resetUserIds <- function() {
        updateTextInput(inputId = "user_id", value = "")
        rv$sel_users <- NULL
      }

      output$survey_count_users <- renderText({
        paste(
          nrow(filtered_surveys()),
          "out of",
          nrow(base_users()),
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
          nrow(filtered_surveys()),
          "out of",
          nrow(base_dates()),
          "surveys match your date selections."
        )
      })

      # Filter by survey attributes ----

      ## Habitats ----

      # Generate labels
      habitat_labels <- reactive({
        habitats |>
          left_join(
            count(base_surveys(), habitat, .drop = F),
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
            count(base_surveys(), management, .drop = F),
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
          nrow(filtered_surveys()),
          "out of",
          nrow(base_surveys()),
          "surveys match your habitat and management selections."
        )
      })

      # Filter by crop/plant ----

      ## Checkboxes ----

      # Generate crop labels
      crop_labels <- reactive({
        select_crops |>
          left_join(
            count(base_plants(), plant_type, .drop = F),
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
            count(base_plants(), plant_type, .drop = F),
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
            count(base_plants(), plant_type, .drop = F),
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
          nrow(filtered_surveys()),
          "out of",
          nrow(base_plants()),
          "surveys match your plant selections."
        )
      })

      # Final survey count ----

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
