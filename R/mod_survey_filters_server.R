## Survey filters module server ##

#' requires global vars:
#' - surveys
#' - map_pts_wi
#' 
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
        surveys %>%
          filter(year %in% input$years)
      })
      
      observeEvent(input$years, {
        resetMap()
      })
      
      output$year_survey_count <- renderText({
        paste(
          nrow(surveys_by_year()), "out of",
          nrow(surveys), "surveys selected."
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
        surveys_by_year() %>%
          filter(grid_pt %in% map_selection())
      })
      
      
      ## Initialize map ----
      
      map_grids <- reactive({
        map_pts %>%
          filter(grid_pt %in% surveys_by_year()$grid_pt)
      })
      
      # Initial map condition
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMapPane("base_grids", zIndex = 410) %>%
          addMapPane("selected_grids", zIndex = 420) %>%
          setView(lng = -89.7, lat = 44.8, zoom = 7) %>%
          addEasyButtonBar(
            easyButton(
              position = "topleft",
              icon = "fa-crosshairs",
              title = "Get my location",
              onClick = JS("
            function(btn, map) {
              map.locate({
                setView: true,
                enableHighAccuracy: false,
                maxZoom: 12
              })
            }
          ")
            ),
            easyButton(
              position = "topleft",
              icon = "fa-globe",
              title = "Reset map view",
              onClick = JS("function(btn, map) { map.setView([44.8, -89.7], 7) }")
            )
          )
      })
      
      observeEvent(map_grids(), {
        leafletProxy("map") %>%
          clearGroup("base_grids") %>%
          addRectangles(
            data = map_grids(),
            lng1 = ~ lng - .05, lng2 = ~ lng + .05,
            lat1 = ~ lat - .05, lat2 = ~ lat + .05,
            layerId = ~ grid_pt,
            group = "base_grids",
            label = ~ paste(n_surveys, "surveys by", n_users, "users"),
            weight = 1,
            opacity = 1,
            color = "orange",
            fillOpacity = .25,
            fillColor = "yellow",
            highlight = highlightOptions(
              weight = 3,
              color = "red",
              fillColor = "orange",
              fillOpacity = 0.7),
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
        leafletProxy("map") %>%
          clearGroup("selected_grids") %>%
          addRectangles(
            data = filter(map_pts, grid_pt %in% map_selection()),
            layerId = ~ paste(grid_pt, "selected"),
            group = "selected_grids",
            lng1 = ~ lng - .05, lng2 = ~ lng + .05,
            lat1 = ~ lat - .05, lat2 = ~ lat + .05,
            label = ~ paste(n_surveys, "surveys by", n_users, "users"),
            weight = 1, opacity = 1, color = "red",
            fillOpacity = .25, fillColor = "orange",
            options = pathOptions(pane = "selected_grids"),
            highlight = highlightOptions(
              weight = 3,
              color = "red",
              fillColor = "orange",
              fillOpacity = 0.7)
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
            leafletProxy("map") %>%
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
        leafletProxy("map") %>% 
          fitBounds(
            lng1 = min(map_grids()$lng),
            lat1 = min(map_grids()$lat),
            lng2 = max(map_grids()$lng),
            lat2 = max(map_grids()$lat))
        map_selection(map_grids()$grid_pt)
      })
      
      # select grids visible in map window
      observeEvent(input$map_select_visible, {
        bounds <- input$map_bounds
        new_pts <- map_grids() %>%
          filter(
            lng > bounds$west,
            lng < bounds$east,
            lat > bounds$south,
            lat < bounds$north
          ) %>%
          pull(grid_pt)
        leafletProxy("map") %>%
          clearGroup("selected_grids")
        map_selection(new_pts)
      })
      
      # clear selection
      observeEvent(input$map_clear_selection, {
        leafletProxy("map") %>%
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
        leafletProxy("map") %>%
          setView(lng = -89.7, lat = 44.8, zoom = 7)
      }
      
      
      ## Survey count text ----
      
      output$survey_count_loc <- renderText({
        if (is.null(map_selection())) {
          "0 zones and 0 surveys selected on the map."
        } else {
          paste(
            length(map_selection()), "zones and ",
            nrow(surveys_by_loc()), " surveys selected on the map.")
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
          {ids <- suppressWarnings(parse_number(unlist(strsplit(input$user_id, ","))))},
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
          nrow(surveys_by_user()), "out of",
          nrow(surveys_by_loc()), "surveys match your user selections.")
      })
      
      
    # Filter by date of survey ----
      
      surveys_by_date <- reactive({
        surveys_by_user() %>%
          filter(
            year %in% input$years,
            between(doy, yday(input$date_range[1]), yday(input$date_range[2]))
          )
      })
      
      
      ## Reset ----
      
      observeEvent(input$reset_date, resetDate())
      
      resetDate <- function() {
        updateSliderInput(
          inputId = "date_range",
          value = c(date_slider_min, date_slider_max),
          timeFormat = "%b %d"
        )
      }
      
      
      ## Survey count text ----
      
      output$survey_count_date <- renderText({
        paste(
          nrow(surveys_by_date()), "out of",
          nrow(surveys_by_user()), "surveys match your date selections.")
      })
      
      
      
      
    # Filter by survey attributes ----
      
      surveys_by_attr <- reactive({
        surveys_by_date() %>%
          filter(
            habitat %in% input$which_habitat,
            management %in% input$which_mgmt
          ) %>%
          droplevels()
      })
      
      
      ## Habitats ----
      
      # Generate labels
      habitat_labels <- reactive({
        habitats %>%
          left_join(
            count(surveys_by_user(), habitat, .drop = F),
            by = c("type" = "habitat")) %>%
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")"))
      })
      
      # Update labels on checkbox
      observeEvent(habitat_labels(),{
        updateCheckboxGroupInput(
          inputId = "which_habitat",
          choiceNames = habitat_labels()$box_label,
          choiceValues = habitat_labels()$type,
          selected = input$which_habitat)
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
        managements %>%
          left_join(
            count(surveys_by_user(), management, .drop = F),
            by = c("type" = "management")) %>%
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")"))
      })
      
      # Update labels
      observeEvent(mgmt_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_mgmt",
          choiceNames = mgmt_labels()$box_label,
          choiceValues = mgmt_labels()$type,
          selected = input$which_mgmt)
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
          nrow(surveys_by_attr()), "out of",
          nrow(surveys_by_date()), "surveys match your habitat and management selections.")
      })
      
      
      
    # Filter by crop/plant ----
      
      surveys_by_plant <- reactive({
        surveys_by_attr() %>%
          filter(plant_type %in% c(input$which_crops, input$which_focal_noncrops, input$which_noncrops)) %>%
          droplevels()
      })
      
      
      ## Checkboxes ----
      
      # Generate crop labels
      crop_labels <- reactive({
        select_crops %>%
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")) %>%
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")"))
      })
      
      # Update labels
      observeEvent(crop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_crops",
          choiceNames = crop_labels()$box_label,
          choiceValues = crop_labels()$type,
          selected = input$which_crops)
      })
      
      # Generate focal plant labels
      focal_noncrop_labels <- reactive({
        focal_noncrops %>%
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")) %>%
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")"))
      })
      
      # Update labels
      observeEvent(focal_noncrop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_focal_noncrops",
          choiceNames = lapply(as.list(focal_noncrop_labels()$box_label), HTML),
          choiceValues = focal_noncrop_labels()$type,
          selected = input$which_focal_noncrops)
      })
      
      # Generate non-crop plant labels
      noncrop_labels <- reactive({
        select_noncrops %>%
          left_join(
            count(surveys_by_attr(), plant_type, .drop = F),
            by = c("type" = "plant_type")) %>%
          mutate(
            n = replace_na(n, 0),
            box_label = paste0(label, " (", n, ")"))
      })
      
      # Update labels
      observeEvent(noncrop_labels(), {
        updateCheckboxGroupInput(
          inputId = "which_noncrops",
          choiceNames = lapply(as.list(noncrop_labels()$box_label), HTML),
          choiceValues = noncrop_labels()$type,
          selected = input$which_noncrops)
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
          nrow(surveys_by_plant()), "out of",
          nrow(surveys_by_attr()), "surveys match your plant selections.")
      })
      
      
    # Final survey count ----
      
      filtered_surveys <- reactive({
        surveys_by_plant()
      })
      
      filtered_surveys_long <- reactive({
        filtered_surveys() %>%
          pivot_longer(cols = bees$type, names_to = "bee", values_to = "count") %>%
          left_join(bee_join, by = "bee")
      })
      
      output$survey_count_final <- renderText({
        paste(
          nrow(filtered_surveys()), "out of",
          nrow(surveys), "total surveys match all of your criteria.")
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
          wide = reactive({filtered_surveys()}),
          long = reactive({filtered_surveys_long()})
        )
      )
    }
  )
}
