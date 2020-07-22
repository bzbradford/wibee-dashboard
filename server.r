#### SERVER ####

library(tidyverse)
library(shiny)
library(leaflet)
library(DT)

server <- function(input, output, session) {
  
  filtered_by_loc <- reactive({
    surveys %>% filter(grid_pt %in% map_selection())
  })
  
  
  output$survey_count_loc <- renderText({
    paste(nrow(filtered_by_loc()), " surveys selected on the map.")
  })
  
  
  # filter survey data by date slider
  filtered_by_date <- reactive({
    filtered_by_loc() %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  
  # Number of matching surveys
  output$survey_count_date <- renderText({
    paste(nrow(filtered_by_date()), "surveys selected from this date range.")
  })
  
  
  # filter survey data by checkboxes
  filtered_surveys <- reactive({
    filtered_by_date() %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt)
  })
  
  
  # Number of matching surveys
  output$survey_count_final <- renderText({
    paste(nrow(filtered_surveys()), "surveys match your filter selections.")
  })
  
  
  filtered_surveys_long <- reactive({
    filtered_surveys() %>%
      pivot_longer(
        bee_ref$bee_type,
        names_to = "bee_type",
        values_to = "count") %>%
      left_join(bee_ref, by = "bee_type") %>%
      filter(bee_name %in% input$which_bees)
  })
  
  
  map_selection <- reactiveVal(value = map_pts_all)
  
  # update checkbox labels when date slider is moved
  observeEvent(list(map_selection(), input$date_range), {
    habitat_labels <- filtered_by_date() %>%
      count(habitat, .drop = F) %>%
      mutate(label = paste0(habitat, " (", n, ")")) %>%
      .$label
    crop_labels <- filtered_by_date() %>%
      count(crop, .drop = F) %>%
      mutate(label = paste0(crop, " (", n, ")")) %>%
      .$label
    mgmt_labels <- filtered_by_date() %>%
      count(management, .drop = F) %>%
      mutate(label = paste0(management, " (", n, ")")) %>%
      .$label
    
    updateCheckboxGroupInput(
      session,
      "which_habitat",
      choiceNames = habitat_labels,
      choiceValues = habitat_types,
      selected = input$which_habitat
    )
    updateCheckboxGroupInput(
      session,
      "which_crop",
      choiceNames = crop_labels,
      choiceValues = crop_types,
      selected = input$which_crop
    )
    updateCheckboxGroupInput(
      session,
      "which_mgmt",
      choiceNames = mgmt_labels,
      choiceValues = mgmt_types,
      selected = input$which_mgmt
    )
  })
  
  
  # Refresh bee selection checkbox, depending on yes/no wild bee grouping selection
  reset_bees <- function() {
    if(input$group_wild) {
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
  
  # update date slider
  observeEvent(filtered_by_loc(), {
    df <- filtered_by_loc()
    updateSliderInput(session, "date_range", min = min(df$date), max = max(df$date))
  })
  
  
  # reset date slider
  observeEvent(input$reset_date, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
  })

  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
    updateCheckboxGroupInput(session, "which_habitat", selected = habitat_types)
    updateCheckboxGroupInput(session, "which_crop", selected = crop_types)
    updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)
    updateCheckboxInput(session, "group_wild", value = F)
    reset_bees()
  })
  
  # Group wild bees together
  observeEvent(input$group_wild, reset_bees())
  
  
  ## All/None buttons ##
  observeEvent(input$which_bees_all, reset_bees())
  observeEvent(input$which_bees_none,
    {updateCheckboxGroupInput(session, "which_bees", selected = "")})
  observeEvent(input$which_habitat_all,
    {updateCheckboxGroupInput(session, "which_habitat", selected = habitat_types)})
  observeEvent(input$which_habitat_none,
    {updateCheckboxGroupInput(session, "which_habitat", selected = "")})
  observeEvent(input$which_crop_all,
    {updateCheckboxGroupInput(session, "which_crop", selected = crop_types)})
  observeEvent(input$which_crop_none,
    {updateCheckboxGroupInput(session, "which_crop", selected = "")})
  observeEvent(input$which_mgmt_all,
    {updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)})
  observeEvent(input$which_mgmt_none,
    {updateCheckboxGroupInput(session, "which_mgmt", selected = "")})
  
  
  # Initial map condition
  output$map <- renderLeaflet({
    leaflet(map_pts) %>%
      addTiles() %>%
      addRectangles(
        lng1 = ~ lng_rnd - .05, lng2 = ~ lng_rnd + .05,
        lat1 = ~ lat_rnd - .05, lat2 = ~ lat_rnd + .05,
        layerId = ~ grid_pt,
        group = "All points",
        label = ~ paste(n_surveys, "surveys"),
        weight = 1,
        opacity = 1,
        color = "orange",
        fillOpacity = .25,
        fillColor = "yellow",
        highlight = highlightOptions(
          weight = 3,
          color = "red",
          fillColor = "orange",
          fillOpacity = 0.7)) %>%
      setView(lng = -89.7, lat = 44.8, zoom = 7)
  })
  
  # handle adding and subtracting grids from selection
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    grid_pt <- str_remove(click$id, " selected")
    proxy <- leafletProxy("map")
    
    # on first click deselect all other grids
    if(setequal(map_selection(), map_pts_all)) {
      proxy %>% clearGroup("Select points")
      map_selection(grid_pt)
    } else if (grepl("selected", click$id, fixed = T)) {
      proxy %>% removeShape(click$id)
      old_sel <- map_selection()
      new_sel <- old_sel[old_sel != grid_pt]
      map_selection(new_sel)
    } else {
      new_sel <- c(map_selection(), grid_pt)
      map_selection(new_sel)
    }
  })   
  
  # reactive portion of map showing selected grids
  observeEvent(map_selection(), {
    leafletProxy("map") %>%
      addRectangles(
        layerId = ~ paste(grid_pt, "selected"),
        group = "Select points",
        lng1 = ~ lng_rnd - .05, lng2 = ~ lng_rnd + .05,
        lat1 = ~ lat_rnd - .05, lat2 = ~ lat_rnd + .05,
        label = ~ paste(n_surveys, "surveys"),
        weight = 1, opacity = 1, color = "red",
        fillOpacity = .25, fillColor = "orange",
        highlight = highlightOptions(
          weight = 3,
          color = "red",
          fillColor = "orange",
          fillOpacity = 0.7),
        data = filter(map_pts, grid_pt %in% map_selection()))
  })

  # map buttons
  observeEvent(input$map_zoom_all, {
    leafletProxy("map") %>% 
      fitBounds(
        lng1 = min(surveys$lng),
        lat1 = min(surveys$lat),
        lng2 = max(surveys$lng),
        lat2 = max(surveys$lat))
  })
  observeEvent(input$map_zoom_wi, {
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
  })
  observeEvent(input$reset_map, {
    map_selection(map_pts_all)
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
  })
  

  
  

  
  
  
  # observeEvent(input$map_shape_click, {
  #   click <- input$map_shape_click
  #   lng <- round(click$lng, 1)
  #   lat <- round(click$lat, 1)
  #   proxy <- leafletProxy("map")
  #
  #   if(grepl("selected", click$id, fixed = T)) {
  #     proxy %>% removeShape(layerId = click$id)
  #     print("removing")
  #   } else {
  #     proxy %>%
  #       addRectangles(
  #         lng1 = ~ round(lng, 1) - .05, lng2 = ~ round(lng, 1) + .05,
  #         lat1 = ~ round(lat, 1) - .05, lat2 = ~ round(lat, 1) + .05,
  #         layerId = paste(lng, lat, "selected"),
  #         label = ~ paste(n_surveys, "surveys"),
  #         weight = 1,
  #         opacity = 1,
  #         color = "red",
  #         fillOpacity = .25,
  #         fillColor = "orange",
  #         highlight = highlightOptions(
  #           weight = 3,
  #           color = "red",
  #           fillColor = "orange",
  #           fillOpacity = 0.7),
  #         data = click)
  #     print("selecting")
  #   }
  #
  # })
  
  # output$surveyMap <- renderLeaflet({
  #   leaflet(map_pts) %>%
  #     addTiles() %>%
  #     addRectangles(
  #       lng1 = ~ lng - .05, lng2 = ~ lng + .05,
  #       lat1 = ~ lat - .05, lat2 = ~ lat + .05,
  #       label = ~ paste(n_surveys, "surveys"),
  #       popup = ~ paste0(
  #         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
  #         "<strong>Mean visits per minute:</strong><br/>",
  #         "Honey bees: ", hb, "<br/>",
  #         "Wild bees: ", wb, "<br/>",
  #         "Non-bees: ", nb),
  #       weight = 1,
  #       opacity = 1,
  #       color = "orange",
  #       fillOpacity = .25,
  #       fillColor = "yellow",
  #       highlight = highlightOptions(
  #         weight = 5,
  #         color = "red",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE)) %>%
  #     addMarkers(~lng, ~lat, icon = bee_icon,
  #       label = ~ paste(n_surveys, "surveys"),
  #       popup = ~ paste0(
  #         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
  #         "<strong>Mean visits per minute:</strong><br/>",
  #         "Honey bees: ", hb, "<br/>",
  #         "Wild bees: ", wb, "<br/>",
  #         "Non-bees: ", nb),
  #       clusterOptions = markerClusterOptions(showCoverageOnHover = F)
  #     )
  # })
  
  
  # Plot of daily bee averages
  output$plotByDate <- renderPlot({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      df2 <- df %>%
        group_by(date, bee_name, bee_color) %>%
        summarise(visit_rate = mean(count / 5), .groups = "drop") %>%
        droplevels()
      df2 %>%
        ggplot(aes(x = date, y = visit_rate, fill = bee_name)) +
        geom_col() +
        scale_fill_manual(values = levels(df2$bee_color)) +
        labs(x = "Survey date", y = "Average visits per minute", fill = "") +
        theme_classic() +
        theme(
          text = element_text(size = 14),
          plot.title = element_text(face = "bold", hjust = .5))
    }
  })
  
  
  # Plot of bee activity averages by site characteristics
  output$plotByCat <- renderPlot({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      # create working dataset
      df2 <- df %>%
        rename(
          `By crop` = crop,
          `By habitat` = habitat,
          `By management` = management) %>%
        pivot_longer(
          cols = c("By habitat", "By crop", "By management"),
          names_to = "category") %>%
        mutate(category = factor(category, c("By habitat", "By crop", "By management"))) %>%
        group_by(category, value, bee_name, bee_color) %>%
        summarise(visit_rate = mean(count) / 5, n = n(), .groups = "drop") %>%
        droplevels()
      
      # get matching surveys counts by category
      survey_count <-
        df2 %>%
        group_by(category, value) %>%
        summarise(n = mean(n), .groups = "drop")
      
      # plot
      df2 %>%
        ggplot() +
        geom_col(aes(x = value, y = visit_rate, fill = bee_name)) +
        geom_text(
          data = survey_count,
          aes(x = value, y = 0, label = paste0("(", n, ")")),
          size = 3, vjust = 1) +
        scale_fill_manual(values = levels(df2$bee_color)) +
        labs(
          title = "Pollinator visitation rate by site characteristics",
          x = "", y = "Average visits per minute", fill = "") +
        facet_grid(. ~ category, scales = "free_x") +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
          text = element_text(size = 14),
          plot.title = element_text(face = "bold", hjust = .5))
    }
  })
  
  
  
  ## histograms of total insect counts per survey
  # output$beePlot3 <- renderPlot({
  #   df <- filtered_surveys_long()
  #   if (nrow(df) > 0) {
  #     # exclude zero-counts from histograms
  #     df2 <- df %>%
  #       filter(count > 0) %>%
  #       droplevels()
  #     df2 %>%
  #       ggplot(aes(x = count, fill = bee_name)) +
  #       geom_histogram(bins = 20) +
  #       facet_wrap( ~ bee_name) +
  #       scale_fill_manual(values = levels(df2$bee_color)) +
  #       labs(x = "Number of flower visits per survey", y = "Number of surveys", fill = "")
  #   }
  # })
  
  output$summaryTable <- renderDT({
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
      select("row", everything())},
    rownames = F,
    options = list(pageLength = 25)
  )
  
  output$plotUserStats <- renderPlot({
    filtered_surveys() %>%
      group_by(user_id) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      mutate(rank = row_number()) %>%
      mutate(rank = factor(rank, levels = rank)) %>%
      ggplot(aes(x = rank, y = n, fill = as.integer(n))) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +
      scale_fill_gradient(low = "gold", high = "darkgreen") +
      labs(
        title = "Number of matching surveys by user",
        x = "User rank",
        y = "Number of surveys"
      ) +
      guides(fill = "none") +
      theme_classic() +
      theme(
        text = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = .5))
  })
  
}