# server.R

server <- function(input, output, session) {
  
# Reactive values ---------------------------------------------------------
  
  rv <- reactiveValues(
    map_selection = map_pts_wi,
    selected_users = NULL
  )
  
  surveys_by_year <- reactive({
    surveys %>%
      filter(year %in% input$years)
  })
  
  surveys_by_loc <- reactive({
    surveys_by_year() %>%
      filter(grid_pt %in% rv$map_selection)
    })
  
  surveys_by_user <- reactive({
    if (is.null(rv$selected_users)) {
      surveys_by_loc()
    } else {
      filter(surveys_by_loc(), user_id %in% rv$selected_users)
    }
  })
  
  surveys_by_date <- reactive({
    surveys_by_user() %>%
      filter(
        year %in% input$years,
        between(doy, yday(input$date_range[1]), yday(input$date_range[2]))
      )
    })
  
  surveys_by_site <- reactive({
    surveys_by_date() %>%
      filter(
        habitat %in% input$which_habitat,
        management %in% input$which_mgmt
      ) %>%
    droplevels()
    })
  
  filtered_surveys <- reactive({
    surveys_by_site() %>%
      filter(plant_type %in% c(input$which_crops, input$which_focal_noncrops, input$which_noncrops)) %>%
      droplevels()
  })

  # filter the long survey list
  # filtered_surveys_long <- reactive({
  #   df <- surveys_long %>%
  #     filter(
  #       grid_pt %in% rv$map_selection,
  #       year %in% input$years,
  #       between(doy, yday(input$date_range[1]), yday(input$date_range[2])),
  #       habitat %in% input$which_habitat,
  #       management %in% input$which_mgmt,
  #       plant_type %in% c(input$which_crops, input$which_focal_noncrops, input$which_noncrops),
  #       bee_name %in% input$which_bees
  #     )
  #   if (!is.null(rv$selected_users)) {
  #     df <- filter(df, user_id %in% rv$selected_users)
  #   }
  #   droplevels(df)
  # })
  
  filtered_surveys_long <- reactive({
    filtered_surveys() %>%
      pivot_longer(cols = bees$type, names_to = "bee", values_to = "count") %>%
      left_join(bee_join, by = "bee") %>%
      filter(bee_name %in% input$which_bees) %>%
      droplevels()
  })
  
  

# Year select -------------------------------------------------------------

  resetYears <- function() {
    updateCheckboxGroupInput(session, "years", selected = year_summary$year)
  }
  
  output$survey_count_years <- renderText({
    paste(
      nrow(surveys_by_year()), "out of",
      nrow(surveys), "surveys selected."
    )
  })
  
  
# Map ----------------------------------------------------------------
  
  ## Initialize map ----
  
  # initialize map selection list, with all in WI selected
  # map_selection <- reactiveVal(value = map_pts_wi)
  
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
      setView(lng = -89.7, lat = 44.8, zoom = 7)
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
    rv$map_selection <- intersect(rv$map_selection, map_grids()$grid_pt)
  })
  
  # reactive portion of map showing selected grids
  observeEvent(rv$map_selection, {
    leafletProxy("map") %>%
      clearGroup("selected_grids") %>%
      addRectangles(
        data = filter(map_pts, grid_pt %in% rv$map_selection),
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
    proxy <- leafletProxy("map")
    
    # on first click deselect all other grids
    if (grepl("selected", click$id, fixed = T)) {
      if (length(rv$map_selection) == 1) {return()}
      proxy %>% removeShape(click$id)
      old_sel <- rv$map_selection
      new_sel <- old_sel[old_sel != grid_pt]
      rv$map_selection <- new_sel
    } else {
      new_sel <- c(rv$map_selection, grid_pt)
      rv$map_selection <- new_sel
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
    rv$map_selection <- map_grids()$grid_pt
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
    rv$map_selection <- new_pts
  })
  
  # clear selection
  observeEvent(input$map_clear_selection, {
    leafletProxy("map") %>%
      clearGroup("selected_grids")
    rv$map_selection <- NULL
  })
  
  observeEvent(input$map_reset, resetMap())
  
  # reset view to show and select Wisconsin points
  resetMap <- function() {
    resetMapGrids()
    resetMapView()
  }
  
  resetMapGrids <- function() {
    rv$map_selection <- filter(map_grids(), inwi)$grid_pt
  }
  
  resetMapView <- function() {
    leafletProxy("map") %>%
      setView(lng = -89.7, lat = 44.8, zoom = 7)
  }
  
  
  ## Survey count text ----
  
  output$survey_count_loc <- renderText({
    if (is.null(rv$map_selection)) {
      "0 zones and 0 surveys selected on the map."
    } else {
      paste(
        length(rv$map_selection), "zones and ",
        nrow(surveys_by_loc()), " surveys selected on the map.")
    }
  })
  
  

# User select -------------------------------------------------------------
  
  ## UI ----
  
  output$selected_users_display <- renderUI({
    if (is.null(rv$selected_users)) {
      p(em("No users selected, showing surveys by all users."))
    } else {
      p(
        lapply(rv$selected_users, function(i) {
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
      if (is.null(rv$selected_users)) {
        rv$selected_users <- valid_ids
      } else {
        rv$selected_users <- sort(union(rv$selected_users, valid_ids))
      }
    }
    updateTextInput(inputId = "user_id", value = "")
  })
  
  
  ## Reset ----
  
  observeEvent(input$reset_user_ids, resetUserIds())
  
  resetUserIds <- function() {
    updateTextInput(inputId = "user_id", value = "")
    rv$selected_users <- NULL
  }
  
  
  # Survey count text ----
  
  output$survey_count_users <- renderText({
    paste(
      nrow(surveys_by_user()), "out of",
      nrow(surveys_by_loc()), "surveys match your user selections.")
  })



# Date select -------------------------------------------------------------

  ## Reset ----
  
  observeEvent(input$reset_date, resetDate())
  
  resetDate <- function() {
    updateCheckboxGroupButtons(session, "years", selected = years)
    updateSliderInput(session, "date_range", value = c(date_slider_min, date_slider_max))
  }
  
  
  ## Survey count text ----
  
  output$survey_count_date <- renderText({
    paste(
      nrow(surveys_by_date()), "out of",
      nrow(surveys_by_user()), "surveys match your date selections.")
  })
  
  
  
# Survey attribs select ---------------------------------------------------
  
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
    updateCheckboxGroupInput(session, "which_habitat",
      choiceNames = habitat_labels()$box_label,
      choiceValues = habitat_labels()$type,
      selected = input$which_habitat)
  })
  
  # Buttons
  observeEvent(input$which_habitat_all, selectAllHabitats())
  observeEvent(input$which_habitat_none, selectNoHabitats())
  
  selectAllHabitats <- function() {
    updateCheckboxGroupInput(session, "which_habitat", selected = habitats$type)
  }
  selectNoHabitats <- function() {
    updateCheckboxGroupInput(session, "which_habitat", selected = "")
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
    updateCheckboxGroupInput(session, "which_mgmt",
      choiceNames = mgmt_labels()$box_label,
      choiceValues = mgmt_labels()$type,
      selected = input$which_mgmt)
  })
  
  # Buttons
  observeEvent(input$which_mgmt_all, selectAllMgmts())
  observeEvent(input$which_mgmt_none, selectNoMgmts())
  
  selectAllMgmts <- function() {
    updateCheckboxGroupInput(session, "which_mgmt", selected = managements$type)
  }
  selectNoMgmts <- function() {
    updateCheckboxGroupInput(session, "which_mgmt", selected = "")
  }
  
  
  ## Bee groups ----
  
  # Buttons
  observeEvent(input$which_bees_all, resetBees())
  observeEvent(input$which_bees_none, updateCheckboxGroupInput(session, "which_bees", selected = ""))
  
  # Reset bee selections on grouping
  observeEvent(input$group_wild, resetBees())
  
  # Refresh bee selection checkbox, depending on yes/no wild bee grouping selection
  resetBees <- function() {
    if (input$group_wild) {
      updateCheckboxGroupInput(
        session,
        "which_bees",
        choiceNames = wildbee_names,
        choiceValues = wildbee_names,
        selected = wildbee_names)
    } else {
      updateCheckboxGroupInput(
        session,
        "which_bees",
        choiceNames = bee_names,
        choiceValues = bee_names,
        selected = bee_names)
    }
  }
  
  resetBeeGrouping <- function() {
    updateCheckboxInput(session, "group_wild", value = F)
  }
  
  
  ## Survey count text ----
  
  output$survey_count_site <- renderText({
    paste(
      nrow(surveys_by_site()), "out of",
      nrow(surveys_by_date()), "surveys match your habitat and management selections.")
  })
  
  

# Plant select ------------------------------------------------------------
  
  ## Checkboxes ----
  
  # Generate crop labels
  crop_labels <- reactive({
    select_crops %>%
      left_join(
        count(surveys_by_site(), plant_type, .drop = F),
        by = c("type" = "plant_type")) %>%
      mutate(
        n = replace_na(n, 0),
        box_label = paste0(label, " (", n, ")"))
  })
  
  # Update labels
  observeEvent(crop_labels(), {
    updateCheckboxGroupInput(session, "which_crops",
      choiceNames = crop_labels()$box_label,
      choiceValues = crop_labels()$type,
      selected = input$which_crops)
  })
  
  # Generate focal plant labels
  focal_noncrop_labels <- reactive({
    focal_noncrops %>%
      left_join(
        count(surveys_by_site(), plant_type, .drop = F),
        by = c("type" = "plant_type")) %>%
      mutate(
        n = replace_na(n, 0),
        box_label = paste0(label, " (", n, ")"))
  })
  
  # Update labels
  observeEvent(focal_noncrop_labels(), {
    updateCheckboxGroupInput(session, "which_focal_noncrops",
      choiceNames = lapply(as.list(focal_noncrop_labels()$box_label), HTML),
      choiceValues = focal_noncrop_labels()$type,
      selected = input$which_focal_noncrops)
  })
  
  # Generate non-crop plant labels
  noncrop_labels <- reactive({
    select_noncrops %>%
      left_join(
        count(surveys_by_site(), plant_type, .drop = F),
        by = c("type" = "plant_type")) %>%
      mutate(
        n = replace_na(n, 0),
        box_label = paste0(label, " (", n, ")"))
  })
  
  # Update labels
  observeEvent(noncrop_labels(), {
    updateCheckboxGroupInput(session, "which_noncrops",
      choiceNames = lapply(as.list(noncrop_labels()$box_label), HTML),
      choiceValues = noncrop_labels()$type,
      selected = input$which_noncrops)
  })
  
  
  ## Action buttons ----
  
  observeEvent(input$which_crops_all, updateCheckboxGroupInput(session, "which_crops", selected = select_crops$type))
  observeEvent(input$which_crops_none, updateCheckboxGroupInput(session, "which_crops", selected = ""))
  
  observeEvent(input$which_focal_noncrops_all, updateCheckboxGroupInput(session, "which_focal_noncrops", selected = focal_noncrops$type))
  observeEvent(input$which_focal_noncrops_none, updateCheckboxGroupInput(session, "which_focal_noncrops", selected = ""))
  
  observeEvent(input$which_noncrops_all, updateCheckboxGroupInput(session, "which_noncrops", selected = select_noncrops$type))
  observeEvent(input$which_noncrops_none, updateCheckboxGroupInput(session, "which_noncrops", selected = ""))
  
  observeEvent(input$select_all_plants, selectAllPlants())
  observeEvent(input$select_no_plants, selectNoPlants())
  
  selectAllPlants <- function() {
    updateCheckboxGroupInput(session, "which_crops", selected = select_crops$type)
    updateCheckboxGroupInput(session, "which_focal_noncrops", selected = focal_noncrops$type)
    updateCheckboxGroupInput(session, "which_noncrops", selected = select_noncrops$type)
  }
  
  selectNoPlants <- function() {
    updateCheckboxGroupInput(session, "which_crops", selected = "")
    updateCheckboxGroupInput(session, "which_focal_noncrops", selected = "")
    updateCheckboxGroupInput(session, "which_noncrops", selected = "")
  }
  
  
  ## Survey count text ----
  
  output$survey_count_plant <- renderText({
    paste(
      nrow(filtered_surveys()), "out of",
      nrow(surveys_by_site()), "surveys match your plant selections.")
  })
  
  

# Master reset button -----------------------------------------------------

  observeEvent(input$reset, {
    selectAllPlants()
    selectAllHabitats()
    selectAllMgmts()
    resetDate()
    resetUserIds()
    resetYears()
    rv$map_selection <- map_pts_wi
    resetMapView()
    resetBeeGrouping()
    resetBees()
  })
  

# Final survey count text -------------------------------------------------
  
  output$survey_count_final <- renderText({
    paste(
      nrow(filtered_surveys()), "out of",
      nrow(surveys), "total surveys match all of your criteria.")
  })
  
  

# Module servers ----
  
  # species composition pie charts
  speciesCompServer(
    data = reactive(filtered_surveys()),
    data_long = reactive(filtered_surveys_long()),
    which_bees = reactive(input$which_bees)
  )

  # bee activity by date
  activityByDateServer(
    data = reactive(filtered_surveys()),
    data_long = reactive(filtered_surveys_long())
  )
  
  # number of surveys by date
  surveysByDateServer(
    data = reactive(filtered_surveys())
  )
  
  # bee activity by date
  activityByHabitatServer(
    data_long = reactive(filtered_surveys_long())
  )
  
  # bee activity by management type
  activityByMgmtServer(
    data_long = reactive(filtered_surveys_long())
  )
  
  # bee activity by crop type
  activityByCropServer(
    data_long = reactive(filtered_surveys_long())
  )
  
  # map showing surveys or bee activity
  activityMapServer(
    data = reactive(filtered_surveys()),
    data_long = reactive(filtered_surveys_long())
  )
  
  # data table
  dataTableServer(
    data_long = reactive(filtered_surveys_long())
  )
  
  # user stats
  userStatsServer(
    data = reactive(filtered_surveys())
  )
  
}
