#### SERVER ####

library(tidyverse)
library(shiny)
library(leaflet)
library(DT)
library(plotly)


server <- function(input, output, session) {
  
# Reactive values ---------------------------------------------------------
  
  # first filter - by map grid cell
  surveysByLoc <- reactive({
    surveys %>% filter(grid_pt %in% map_selection())})
  
  # filter survey data by date slider
  surveysByLocDate <- reactive({
    surveysByLoc() %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])})
  
  # filter survey data by site characteristics
  filtered_surveys <- reactive({
    surveysByLocDate() %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt)})
  
  # pivot filtered survey list long
  filtered_surveys_long <- reactive({
    filtered_surveys() %>%
      pivot_longer(
        bees$bee_type,
        names_to = "bee_type",
        values_to = "count") %>%
      left_join(bees, by = "bee_type") %>%
      filter(bee_name %in% input$which_bees) %>%
      droplevels()})
  
  
  

# Filter checkboxes -------------------------------------------------------

  # Habitat checkbox labels and values
  habitat_labels <- reactive({
    left_join(habitats,
      count(surveysByLocDate(), habitat, .drop = F),
      by = c("type" = "habitat")) %>%
      mutate(box_label = paste0(label, " (", n, ")"))})
  
  observeEvent(habitat_labels(),{
    updateCheckboxGroupInput(session, "which_habitat",
      choiceNames = habitat_labels()$box_label,
      choiceValues = habitat_labels()$type,
      selected = input$which_habitat)})
  
  
  # Crop checkbox labels and values
  crop_labels <- reactive({
    left_join(crops,
      count(surveysByLocDate(), crop, .drop = F),
      by = c("type" = "crop")) %>%
      mutate(box_label = paste0(label, " (", n, ")"))})
  
  observeEvent(crop_labels(), {
    updateCheckboxGroupInput(session, "which_crop",
      choiceNames = crop_labels()$box_label,
      choiceValues = crop_labels()$type,
      selected = input$which_crop)})
  
  
  # Management checkbox labels and values
  mgmt_labels <- reactive({
    left_join(mgmt,
      count(surveysByLocDate(), management, .drop = F),
      by = c("type" = "management")) %>%
      mutate(box_label = paste0(label, " (", n, ")"))})
  
  observeEvent(mgmt_labels(), {
    updateCheckboxGroupInput(session, "which_mgmt",
      choiceNames = mgmt_labels()$box_label,
      choiceValues = mgmt_labels()$type,
      selected = input$which_mgmt)})
  
  
  # Date slider
  observeEvent(surveysByLoc(), {
    df = surveysByLoc()
    updateSliderInput(
      session,
      "date_range",
      min = min(df$date),
      max = max(df$date),
      value = c(min(df$date), max(df$date)))
  })
  
  # reset date slider button
  observeEvent(input$reset_date, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
  })
  
  
  
  
# Reset buttons -----------------------------------------------------------
  
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
  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
    updateCheckboxGroupInput(session, "which_habitat", selected = habitats$type)
    updateCheckboxGroupInput(session, "which_crop", selected = crops$type)
    updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt$type)
    updateCheckboxInput(session, "group_wild", value = F)
    reset_bees()
  })
  
  # Group wild bees together
  observeEvent(input$group_wild, reset_bees())
  
  # All/None buttons
  observeEvent(input$which_bees_all, reset_bees())
  observeEvent(input$which_bees_none,
    {updateCheckboxGroupInput(session, "which_bees", selected = "")})
  observeEvent(input$which_habitat_all,
    {updateCheckboxGroupInput(session, "which_habitat", selected = habitats$type)})
  observeEvent(input$which_habitat_none,
    {updateCheckboxGroupInput(session, "which_habitat", selected = "")})
  observeEvent(input$which_crop_all,
    {updateCheckboxGroupInput(session, "which_crop", selected = crops$type)})
  observeEvent(input$which_crop_none,
    {updateCheckboxGroupInput(session, "which_crop", selected = "")})
  observeEvent(input$which_mgmt_all,
    {updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt$type)})
  observeEvent(input$which_mgmt_none,
    {updateCheckboxGroupInput(session, "which_mgmt", selected = "")})
  
  
  

# Text outputs ------------------------------------------------------------
  
  # number of matching surveys after map filter
  output$survey_count_loc <- renderText({
    paste(
      length(map_selection()), "zones and ",
      nrow(surveysByLoc()), " surveys selected on the map.")})
  
  # Number of matching surveys after date filter
  output$survey_count_filters <- renderText({
    paste(nrow(surveysByLocDate()), "out of",
      nrow(surveysByLoc()), "surveys match your filter selections.")})  
  
  # Number of matching surveys after site characteristic filters
  output$survey_count_final <- renderText({
    paste(
      nrow(filtered_surveys()), "out of", nrow(surveys), "total surveys match all of your criteria.")})
  
  
  

# Map and map filtering ---------------------------------------------------
  
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
          fillOpacity = 0.7))
    })
  
  # zoom to WI on initial selection
  observeEvent(map_selection(), {
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
  }, once = T)
  
  # handle adding and subtracting grids from selection
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    grid_pt <- str_remove(click$id, " selected")
    proxy <- leafletProxy("map")
    
    # on first click deselect all other grids
    if(setequal(map_selection(), map_pts_all) | setequal(map_selection(), map_pts_wi)) {
      proxy %>% clearGroup("Select points")
      map_selection(grid_pt)
    } else if (grepl("selected", click$id, fixed = T)) {
      if(length(map_selection()) == 1) {return()}
      proxy %>% removeShape(click$id)
      old_sel <- map_selection()
      new_sel <- old_sel[old_sel != grid_pt]
      map_selection(new_sel)
    } else {
      new_sel <- c(map_selection(), grid_pt)
      map_selection(new_sel)}})
  
  
  # reactive portion of map showing selected grids
  observeEvent(map_selection(), {
    leafletProxy("map") %>%
      addRectangles(
        layerId = ~ paste(grid_pt, "selected"),
        group = "Selected points",
        lng1 = ~ lng - .05, lng2 = ~ lng + .05,
        lat1 = ~ lat - .05, lat2 = ~ lat + .05,
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
    map_selection(c(""))
    })
  
  
  # reset view to show and select Wisconsin points
  observeEvent(input$map_reset, {
    leafletProxy("map") %>% setView(lng = -89.7, lat = 44.8, zoom = 7)
    map_selection(map_pts_wi)
    })



# Pie charts --------------------------------------------------------------
  
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

  
  

# Plot by date ------------------------------------------------------------

  # Plot of daily bee counts
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
        layout(
          barmode = "stack",
          title = "Daily average pollinator visitation rates",
          xaxis = list(title = "", type = "date", tickformat = "%b %d<br>%Y", fixedrange = T),
          yaxis = list(title = "Number of visits per survey", fixedrange = T),
          hovermode = "compare",
          legend = list(orientation = "h"),
          bargap = 0
          )
    }
  })
  
  
  

# Plot by site characteristics --------------------------------------------

  ## Convert this plot to plotly! ##
  
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
  
  
  

# Histogram plot (not used) ----------------------------------------------------------
  
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
  
  

# Data table --------------------------------------------------------------

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
    filename = "wibee_data.csv",
    content = function(file) {
      write_csv(filteredTable(), file)
    }
  )
  
  
# Plot of user statistics -------------------------------------------------

  output$plotUserStats <- renderPlot({
    df <- filtered_surveys() %>%
      group_by(user_id) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      mutate(rank = row_number()) %>%
      mutate(label = case_when(
        n != lag(n, default = 0) ~ as.character(n),
        T ~ ""))
    
    df %>%
      ggplot(aes(x = rank, y = n, fill = as.integer(n))) +
      geom_col(color = "black", size = .5, width = 1) +
      geom_text(aes(label = label), vjust = -0.5, hjust = 0) +
      scale_fill_gradient(low = "gold", high = "darkgreen") +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      scale_x_continuous(expand = c(0, 0)) +
      labs(
        title = "Number of matching surveys by user",
        x = "User",
        y = "Number of surveys"
      ) +
      guides(fill = "none") +
      theme_classic() +
      theme(
        text = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = .5))
  })
  

  
}