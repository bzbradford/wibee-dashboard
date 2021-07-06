#---- SERVER ----#

library(tidyverse)
library(shiny)
library(leaflet)
library(DT)
library(plotly)



server <- function(input, output, session) {
  
# Reactive values ---------------------------------------------------------
  
  # first filter - by map grid cell
  surveys_by_loc <- reactive({
    filter(surveys, grid_pt %in% map_selection())
    })
  
  
  # filter survey data by date slider
  surveys_by_date <- reactive({
    surveys_by_loc() %>%
      filter(year %in% input$years) %>%
      filter(between(date, input$date_range[1], input$date_range[2]))
    })
  
  
  # filter survey data by site characteristics
  surveys_by_site <- reactive({
    surveys_by_date() %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(management %in% input$which_mgmt) %>%
    droplevels()
    })
  
  # filter survey data by site characteristics
  filtered_surveys <- reactive({
    surveys_by_site() %>%
      filter(plant_type %in% c(input$which_crops, input$which_focal_noncrops, input$which_noncrops)) %>%
      droplevels()
  })

  # filter the long survey list
  filtered_surveys_long <- reactive({
    surveys_long %>%
      filter(grid_pt %in% map_selection()) %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(management %in% input$which_mgmt) %>%
      filter(plant_type %in% c(input$which_crops, input$which_focal_noncrops, input$which_noncrops)) %>%
      filter(bee_name %in% input$which_bees) %>%
      droplevels()
    })
  

# Filter checkboxes -------------------------------------------------------

  ## Dates ----
  
  # Update date slider on year checkbox actions
  observeEvent(input$years, {
    df <- surveys %>%
      select(year, date) %>%
      filter(year %in% input$years)
    updateSliderInput(
      session,
      "date_range",
      min = min(df$date),
      max = max(df$date),
      value = c(min(df$date), max(df$date)))
    })
  
  
  ## Habitats ----
  
  # Generate labels
  habitat_labels <- reactive({
    habitats %>%
      left_join(
        count(surveys_by_date(), habitat, .drop = F),
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
  
  
  ## Managements ----
  
  # Generate labels
  mgmt_labels <- reactive({
    managements %>%
      left_join(
        count(surveys_by_date(), management, .drop = F),
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
  

  ## Plants ----
  
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
      choiceNames = focal_noncrop_labels()$box_label,
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
      choiceNames = noncrop_labels()$box_label,
      choiceValues = noncrop_labels()$type,
      selected = input$which_noncrops)
    })
  
  
  # Date slider
  # observeEvent(surveys_by_loc(), {
  #   df <- surveys_by_loc()
  #   if (nrow(df) > 0) {
  #     updateSliderInput(
  #       session,
  #       "date_range",
  #       min = min(df$date),
  #       max = max(df$date),
  #       value = c(min(df$date), max(df$date)))
  #     }
  #   })

  

  
  
  
# Reset buttons -----------------------------------------------------------
  
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
  
  resetDate <- function() {
    updateCheckboxGroupButtons(session, "years", selected = years)
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
  }
  
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
  
  
  ## Reset date slider ----
  observeEvent(input$reset_date, resetDate())
  
  
  ## All/None buttons ----
  observeEvent(input$which_bees_all, resetBees())
  observeEvent(input$which_bees_none, updateCheckboxGroupInput(session, "which_bees", selected = ""))
  
  observeEvent(input$which_habitat_all, updateCheckboxGroupInput(session, "which_habitat", selected = habitats$type))
  observeEvent(input$which_habitat_none, updateCheckboxGroupInput(session, "which_habitat", selected = ""))
  
  observeEvent(input$which_mgmt_all, updateCheckboxGroupInput(session, "which_mgmt", selected = managements$type))
  observeEvent(input$which_mgmt_none, updateCheckboxGroupInput(session, "which_mgmt", selected = ""))
  
  observeEvent(input$which_crops_all, updateCheckboxGroupInput(session, "which_crops", selected = select_crops$type))
  observeEvent(input$which_crops_none, updateCheckboxGroupInput(session, "which_crops", selected = ""))
  
  observeEvent(input$which_focal_noncrops_all, updateCheckboxGroupInput(session, "which_focal_noncrops", selected = focal_noncrops$type))
  observeEvent(input$which_focal_noncrops_none, updateCheckboxGroupInput(session, "which_focal_noncrops", selected = ""))
  
  observeEvent(input$which_noncrops_all, updateCheckboxGroupInput(session, "which_noncrops", selected = select_noncrops$type))
  observeEvent(input$which_noncrops_none, updateCheckboxGroupInput(session, "which_noncrops", selected = ""))
  
  observeEvent(input$select_all_plants, selectAllPlants())
  observeEvent(input$select_no_plants, selectNoPlants())
  

  ## Reset bee selections on grouping ----
  observeEvent(input$group_wild, resetBees())
  
  ## Master reset button ----
  observeEvent(input$reset, {
    resetDate()
    updateCheckboxGroupInput(session, "which_habitat", selected = habitats$type)
    updateCheckboxGroupInput(session, "which_mgmt", selected = managements$type)
    selectAllPlants()
    updateCheckboxInput(session, "group_wild", value = F)
    resetBees()
  })
  
  

# Text outputs ------------------------------------------------------------
  
  # number of matching surveys after map filter
  output$survey_count_loc <- renderText({
    if (is.null(map_selection())) {
      "0 zones and 0 surveys selected on the map."
    } else {
      paste(
        length(map_selection()), "zones and ",
        nrow(surveys_by_loc()), " surveys selected on the map.")
    }
  })
  
  # Number of matching surveys after date filter
  output$survey_count_date <- renderText({
    paste(
      nrow(surveys_by_date()), "out of",
      nrow(surveys_by_loc()), "surveys match your date selections.")
  })
  
  # Number of matching surveys after date filter
  output$survey_count_site <- renderText({
    paste(
      nrow(surveys_by_site()), "out of",
      nrow(surveys_by_date()), "surveys match your habitat and management selections.")
    })
  
  # Number of matching surveys after date filter
  output$survey_count_plant <- renderText({
    paste(
      nrow(filtered_surveys()), "out of",
      nrow(surveys_by_site()), "surveys match your plant selections.")
  })
  
  # Number of matching surveys after site characteristic filters
  output$survey_count_final <- renderText({
    paste(
      nrow(filtered_surveys()), "out of",
      nrow(surveys), "total surveys match all of your criteria.")
    })
  
  

# Map and map filtering ---------------------------------------------------
  
  ## Initialize map ----
  
  # initialize map selection list, with all in WI selected
  map_selection <- reactiveVal(value = map_pts_wi)
  
  # Initial map condition
  output$map <- renderLeaflet({
    leaflet(map_pts) %>%
      addTiles() %>%
      addRectangles(
        lng1 = ~ lng - .05, lng2 = ~ lng + .05,
        lat1 = ~ lat - .05, lat2 = ~ lat + .05,
        layerId = ~ grid_pt,
        group = "All points",
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
          fillOpacity = 0.7))
    })
  
  # zoom to WI on initial selection
  observeEvent(map_selection(), {
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
  }, once = T)
  
  
  ## Handle map interactions ----
  
  # handle adding and subtracting grids from selection
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    grid_pt <- str_remove(click$id, " selected")
    proxy <- leafletProxy("map")
    
    # on first click deselect all other grids
    if (setequal(map_selection(), map_pts_all) | setequal(map_selection(), map_pts_wi)) {
      proxy %>% clearGroup("Selected points")
      map_selection(grid_pt)
    } else if (grepl("selected", click$id, fixed = T)) {
      if (length(map_selection()) == 1) {return()}
      proxy %>% removeShape(click$id)
      old_sel <- map_selection()
      new_sel <- old_sel[old_sel != grid_pt]
      map_selection(new_sel)
    } else {
      new_sel <- c(map_selection(), grid_pt)
      map_selection(new_sel)}
    })
  
  # reactive portion of map showing selected grids
  observeEvent(map_selection(), {
    leafletProxy("map") %>%
      addRectangles(
        layerId = ~ paste(grid_pt, "selected"),
        group = "Selected points",
        lng1 = ~ lng - .05, lng2 = ~ lng + .05,
        lat1 = ~ lat - .05, lat2 = ~ lat + .05,
        label = ~ paste(n_surveys, "surveys by", n_users, "users"),
        weight = 1, opacity = 1, color = "red",
        fillOpacity = .25, fillColor = "orange",
        highlight = highlightOptions(
          weight = 3,
          color = "red",
          fillColor = "orange",
          fillOpacity = 0.7),
        data = filter(map_pts, grid_pt %in% map_selection()))
    })
  
  
  ## Handle map buttons ----
  
  # zoom to show all data
  observeEvent(input$map_zoom_all, {
    leafletProxy("map") %>% 
      fitBounds(
        lng1 = min(surveys$lng),
        lat1 = min(surveys$lat),
        lng2 = max(surveys$lng),
        lat2 = max(surveys$lat))
    map_selection(map_pts_all)
    })
  
  # select grids visible in map window
  observeEvent(input$map_select_visible, {
    bounds <- input$map_bounds
    new_pts <- map_pts %>%
      filter(
        lng > bounds$west,
        lng < bounds$east,
        lat > bounds$south,
        lat < bounds$north
      ) %>%
      pull(grid_pt)
    leafletProxy("map") %>% clearGroup("Selected points")
    map_selection(new_pts)
    })
  
  # clear selection
  observeEvent(input$map_clear_selection, {
    leafletProxy("map") %>% clearGroup("Selected points")
    map_selection(NULL)
    })
  
  # reset view to show and select Wisconsin points
  observeEvent(input$map_reset, {
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
    map_selection(map_pts_wi)
    })



# Outputs -----------------------------------------------------------------
  
  ## Bee activity pie charts ----
  
  # left pie chart: whole dataset
  output$map_chart_all <- renderPlotly({
    df <- surveys_long %>%
      filter(bee_name %in% input$which_bees) %>%
      droplevels() %>%
      group_by(bee_name, bee_color) %>%
      summarise(mean_count = round(mean(count), 1), .groups = "drop")
    
    df %>% 
      plot_ly(labels = ~ bee_name, values = ~ mean_count, type = "pie",
        textposition = "inside",
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~ paste(mean_count, bee_name, "per survey"),
        marker = list(
          colors = levels(df$bee_color),
          line = list(color = "#ffffff", width = 1)),
        sort = F,
        direction = "clockwise",
        showlegend = F
        ) %>%
      add_annotations(
        y = 1.075,
        x = 0.5, 
        text = paste0("<b>All surveys (", nrow(surveys), ")</b>"), 
        showarrow = F,
        font = list(size = 15))
  })
  
  # right pie chart: selected data
  output$map_chart_selected <- renderPlotly({
    df <- filtered_surveys_long() %>%
      group_by(bee_name, bee_color) %>%
      summarise(mean_count = round(mean(count), 1), .groups = "drop")
    
    df %>% 
      plot_ly(labels = ~ bee_name, values = ~ mean_count, type = "pie",
        textposition = "inside",
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~ paste(mean_count, bee_name, "per survey"),
        marker = list(
          colors = levels(df$bee_color),
          line = list(color = "#ffffff", width = 1)),
        sort = F,
        direction = "clockwise",
        showlegend = F
      ) %>%
      add_annotations(
        y = 1.075, 
        x = 0.5, 
        text = paste0("<b>Selected surveys (", nrow(filtered_surveys()), ")</b>"), 
        showarrow = F,
        font = list(size = 15))
  })

  
  
  ## Activity by date plot ----

  output$plotByDate <- renderPlotly({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      df %>%
        group_by(date, bee_name, bee_color) %>%
        summarise(visit_rate = round(mean(count / 5), 1), .groups = "drop") %>%
        droplevels() %>%
        plot_ly(
          x = ~ date,
          y = ~ visit_rate,
          type = "bar",
          color = ~ bee_name,
          colors = ~ levels(.$bee_color),
          marker = list(line = list(color = "#ffffff", width = .25))) %>%
        plotly::layout(
          barmode = "stack",
          title = list(text = "<b>Daily average pollinator visitation rates</b>", font = list(size = 15)),
          xaxis = list(title = "", type = "date", tickformat = "%b %d<br>%Y", fixedrange = T),
          yaxis = list(title = "Number of visits per survey", fixedrange = T),
          hovermode = "compare",
          legend = list(orientation = "h"),
          bargap = 0
          )
      }
    })
  
  
  
  ## Plot by habitat ----
  
  output$plotByHabitat <- renderPlotly({
    filtered_surveys_long() %>%
      group_by(habitat_name, bee_name, bee_color) %>%
      summarise(
        visit_rate = round(mean(count), 2),
        n = n(),
        .groups = "drop") %>%
      droplevels() %>%
      mutate(x = fct_inorder(paste0("(", n, ") ", habitat_name))) %>%
      plot_ly(
        type = "bar",
        x = ~ x,
        y = ~ visit_rate,
        color = ~ bee_name,
        colors = ~ levels(.$bee_color),
        marker = list(line = list(color = "#ffffff", width = .25))) %>%
      plotly::layout(
        barmode = "stack",
        title = list(text = "<b>Pollinator visitation rates by habitat type</b>", font = list(size = 15)),
        xaxis = list(title = "", fixedrange = T),
        yaxis = list(title = "Number of visits per survey", fixedrange = T),
        hovermode = "compare"
      )
    })
  
  
  
  ## Plot by management ----
  
  output$plotByMgmt <- renderPlotly({
    filtered_surveys_long() %>%
      group_by(management_name, bee_name, bee_color) %>%
      summarise(
        visit_rate = round(mean(count), 2),
        n = n(),
        .groups = "drop") %>%
      droplevels() %>%
      mutate(x = fct_inorder(paste0("(", n, ") ", management_name))) %>%
      plot_ly(
        type = "bar",
        x = ~ x,
        y = ~ visit_rate,
        color = ~ bee_name,
        colors = ~ levels(.$bee_color),
        marker = list(line = list(color = "#ffffff", width = .25))) %>%
      plotly::layout(
        barmode = "stack",
        title = list(text = "<b>Pollinator visitation rates by management type</b>", font = list(size = 15)),
        xaxis = list(title = "", fixedrange = T),
        yaxis = list(title = "Number of visits per survey", fixedrange = T),
        hovermode = "compare"
      )
    })
  
  
  
  ## Plot by plant ----
  
  output$plotByCrop <- renderPlotly({
    
    # estimate label lengths to increase plot margin
    bmargin <- max(40, 10 + 4 * max(nchar(filtered_surveys()$plant_label)))
    
    # plot
    filtered_surveys_long() %>%
      group_by(plant_label, bee_name, bee_color) %>%
      summarise(
        visit_rate = round(mean(count), 2),
        n = n(),
        .groups = "drop") %>%
      droplevels() %>%
      mutate(x = fct_inorder(paste0("(", n, ") ", plant_label))) %>%
      plot_ly(
        type = "bar",
        x = ~ x,
        y = ~ visit_rate,
        color = ~ bee_name,
        colors = ~ levels(.$bee_color),
        marker = list(line = list(color = "#ffffff", width = .25))) %>%
      plotly::layout(
        barmode = "stack",
        title = list(text = "<b>Pollinator visitation rates by plant type</b>", font = list(size = 15)),
        xaxis = list(title = "", fixedrange = T, tickangle = 45),
        yaxis = list(title = "Number of visits per survey", fixedrange = T),
        hovermode = "compare",
        margin = list(b = bmargin)
      )
    })
  
  

  ## Data table ----

  filteredTable <- reactive({
    filtered_surveys_long() %>%
      mutate(date = as.character(date)) %>%
      group_by_at(input$dtGroups) %>%
      group_by(bee_name, .add = T) %>%
      summarise(
        n = n(),
        visit_rate = round(mean(count) / 5, 1),
        .groups = "drop_last") %>%
      mutate("Total rate" = sum(visit_rate)) %>%
      ungroup() %>%
      pivot_wider(names_from = bee_name, values_from = visit_rate) %>%
      mutate("row" = row_number()) %>%
      select("row", everything())
    })
  
  output$summaryTable <- renderDT(
    filteredTable(),
    rownames = F,
    options = list(pageLength = 25)
    )
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("WiBee data ", format(Sys.time(), "%Y-%m-%d %H%M%S"), ".csv") },
    content = function(file) { write_csv(filteredTable(), file) }
    )
  
  
  
  ## Plot of user statistics ----
  
  output$plotUserStats <- renderPlotly({
    df <- 
      filtered_surveys() %>%
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
    
    df2 <- df %>%
      group_by(year) %>%
      summarise(
        total_surveys = sum(total_surveys),
        total_users = sum(n_users)) %>%
      mutate(
        parent = "All surveys",
        label = paste0("<B>", year, ": ", total_surveys, " surveys by ", total_users, " users</B>"),
        id = year,
        value = total_surveys)
    
    df3 <- df %>%
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
