### WiBee Shiny App ###

## Notes / To Do ##
# consider renaming variables in camelCase
# add number of surveys matching each filter category for each category item
# map shows larger grid cells at wider zoom levels with aggregated data for those levels
# update wibee icon for map
# group bees by honey bee/wild bee/non-bee
# move honeybee before bumblebee


library(shiny)
library(tidyverse)
library(leaflet)


# Script ------------------------------------------------------------------

# read csv
wibee_in <- suppressMessages(read_csv("wibee-surveys.csv"))


### Data wrangling ###

# which columns to convert from char to factor
fct_cols <- c(
    "habitat",
    "cloud_cover",
    "wind_intensity",
    "temperature",
    "management",
    "crop"
)

# useless cols
drop_cols <- c(
  "picture_id",
  "picture_url",
  "honeybee_amended",
  "bumble_bee_amended",
  "large_dark_bee_amended",
  "small_dark_bee_amended",
  "greenbee_amended",
  "non_bee_amended"
)

# bee count data
bee_cols = c(
  "honeybee",  
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
)


# crossref for bee name aliases
bee_ref <-
  tibble(
    bee_num = 1:6,
    species = bee_cols,
    bee_name = c(
      "Honey bee",
      "Bumble bee",
      "Large dark bee",
      "Small dark bee",
      "Green bee",
      "Non-bee"
    ),
    bee_class = c(
      "Honey bees",
      "Wild bees",
      "Wild bees",
      "Wild bees",
      "Wild bees",
      "Non-bees"
    )
  )


# color scheme
bee_palette <- function(bees = 1:6) {
  bees = as.integer(bees)
#  pal = c("red","orange","yellow","green","blue","purple")
  pal = c("#FFA400", "#972D07", "#758BFD", "#AEB8FE", "#8CB369", "#D664BE")
  return(pal[bees])
}


## For stripping out user-entered options in some fields
# valid management types
mgmt_types <- 
  c("organic",
    "conventional",
    "other",
    "unknown")

# valid crop types
crop_types <- 
  c("apple",
    "berry",
    "cucumber",
    "melon",
    "squash",
    "other",
    "none")

# generate main dataset
surveys <- wibee_in %>%
  select(-drop_cols) %>%
  rename(habitat = site_type, management = management_type) %>%
  filter(duration == "5 minutes") %>%
  mutate(
    management =
      case_when(
        management %in% mgmt_types ~ management,
        T ~ "other"),
    crop =
      case_when(
        is.na(crop) ~ "none",
        crop %in% crop_types ~ crop,
        T ~ "other"),
    date = as.Date(ended_at)) %>%  
  mutate_at(fct_cols, as.factor) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  filter(date >= "2020-01-01")

# valid site types
habitat_types <- levels(surveys$habitat)

# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_cols, names_to = "species", values_to = "count") %>%
  left_join(bee_ref, by = "species") %>%
  mutate(bee_name = factor(bee_name, levels = bee_ref$bee_name))


# generate grid points and summary statistics
survey_pts <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng_rnd = round(lng, 1),
    lat_rnd = round(lat, 1),
    wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  group_by(lng_rnd, lat_rnd) %>%
  summarise(
    n_surveys = n(),
    lng = mean(lng_rnd),
    lat = mean(lat_rnd),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1)) %>%
  ungroup() %>%
  select(-c(lng_rnd, lat_rnd))


# set icon for leaflet
bee_icon <- makeIcon(
  iconUrl = "map-icon.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15)


# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)


# total counts for project summary
bee_totals <- surveys_long %>%
  group_by(bee_class) %>%
  summarise(tot_count = sum(count)) %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))



# define ui ---------------------------------------------------------------

ui <- fixedPage(
  fluidRow(
    column(1, h2(img(src = "wibee-logo.png", height = 60))),
    column(10,
      h2("WiBee Data Dashboard"),
      h4("View and explore pollinator data collected with the WiBee app"),
      align = "center"),
    column(1, h2(img(src = "uw-crest.png", height = 60)))
  ),
  hr(style = "margin-top:0px"),
  br(),
  
  sidebarLayout(sidebarPanel(
    p(strong("Project summary")),
    p("Unique users: ", length(unique(surveys$user_id))),
    p("Total completed surveys: ", nrow(surveys)),
    p("Most recent survey: ", max(surveys$date)),
    p("Total insect observations: ", sum(surveys_long$count)),
    tags$ul(tags$li({
      x = filter(bee_totals, bee_class == "Honey bees")
      paste0("Honey bees: ", x$tot_count, " (", x$pct_count, ")")
    }),
      tags$li({
        x = filter(bee_totals, bee_class == "Wild bees")
        paste0("Wild bees: ", x$tot_count, " (", x$pct_count, ")")
      }),
      tags$li({
        x = filter(bee_totals, bee_class == "Non-bees")
        paste0("Non-bees: ", x$tot_count, " (", x$pct_count, ")")
      })),
  ),
    mainPanel(
      h3("What is the WiBee app?", style = "margin-top:0px"),
      p(
        "WiBee (pronounced Wee-bee) is a new smartphone app developed by the",
        a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/"),
        "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms."
      ),
      p(
        "WiBee is a citizen science project, so all the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to this Wisconsin-wide data to help you make decisions about managing your local pollinator community or track any change over time."
      ),
      p(
        "To join the project and help collect data, download the WiBee app today or visit",
        a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee"),
        "to learn more. Thank you for participating!"
      )
    ),
    position = "right"),
  
  
  h2("Survey locations", style = "border-bottom:1px grey"),
  p(em("Click on each bee icon or rectangle to show average counts for all surveys conducted within that area."), style = "margin-bottom:.5em"),
  leafletOutput("surveyMap"),
  
  
  br(),
  h2("Data explorer"),
  tabsetPanel(
    tabPanel("View and filter data",
      br()),
    tabPanel(
      "How to use this dashboard",
      br(),
      p(
        strong("Step 1: Choose a habitat type(s)."),
        "If you run an orchard and you just want to look at the collective data from other orchards in Wisconsin, filter the data by checking the “orchard” box."
      ),
      p(
        strong("Step 2: Choose a crop type(s)."),
        "If you want to compare your apple bloom wild bee visit rate to other apple orchards in Wisconsin, check the apple box to filter the data. Keep in mind that crops bloom at different times of year and have different florescences, so the bee visit rate and bee group composition will likely be different between crops."
      ),
      p(
        strong("Step 3: Choose a management type(s)."),
        "These categories are subjective (chosen by the survey taker) and very broad, so take any variation between conventional, organic or “other” management styles with a grain of salt. "
      ),
      p(
        strong("Step 4: Explore the bee groups."),
        "We recommend looking at the bee data in three different combinations:"
      ),
      tags$ul(
        tags$li("Visits by all the bees combined (honey bees and wild bees)"),
        tags$li("Visits by honey bees compared to wild bees"),
        tags$li(
          "Visits by each individual bee group (bumble bee, honey bee, large dark bee, small dark bee, green bee)"
        )
      ),
      p(
        strong("Step 5: Making sense of the data."),
        "Look at the average flower visits per minute and the composition of the bee visitors."
      ),
      tags$ul(
        tags$li(
          "How do your overall bee visits per minute and wild bee visits per minute compare to the Wisconsin average?"
        ),
        tags$li(
          "What does your flower visit composition look like compared to the Wisconsin average? Do you have a lower, similar or higher percentage of wild bees compared to the Wisconsin average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?"
        )
      ),
      hr()
    )
  ), 
  fluidRow(column(
    8,
    sliderInput(
      "date_range",
      label = "Date range:",
      min = min_date,
      max = max_date,
      value = c(min_date, max_date),
      width = "100%"
    )
  ),
    column(4, actionButton("reset", "Reset filters"), align = "center")),
  fluidRow(
    column(
      3,
      uiOutput("which_bees"),
      div(actionButton("which_bees_all", "All"), style = "display:inline-block"),
      div(actionButton("which_bees_none", "None"), style = "display:inline-block")
    ),
    column(
      3,
      uiOutput("which_habitat"),
      div(actionButton("which_habitat_all", "All"), style = "display:inline-block"),
      div(actionButton("which_habitat_none", "None"), style = "display:inline-block")
    ),
    column(
      3,
      uiOutput("which_crop"),
      div(actionButton("which_crop_all", "All"), style = "display:inline-block"),
      div(actionButton("which_crop_none", "None"), style = "display:inline-block")
    ),
    column(
      3,
      uiOutput("which_mgmt"),
      div(actionButton("which_mgmt_all", "All"), style = "display:inline-block"),
      div(actionButton("which_mgmt_none", "None"), style = "display:inline-block")
    ),
  ),
  br(),
  div(strong(textOutput("n_surveys")), style = "font-size:larger; text-align:center"),
  hr(),
  br(),
  h4("Pollinator activity per minute by survey date", align = "center"),
  plotOutput("beePlot1", height = "300px"),
  hr(),
  br(),
  h4("Pollinator activity by site characteristics", align = "center"),
  plotOutput("beePlot2"),
  

  
  hr(),
  
  br(),
  br(), 
  p(strong("©2020 University of Wisconsin Board of Regents"), align = "center", style = "font-size:small; color:grey"),
  p("developed by tanuki.tech", align = "center", style = "font-size:small; color:grey")
  
)



# define server -----------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_surveys <- reactive({
    surveys %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(habitat %in% input$which_habitats) %>%
      filter(management %in% input$which_mgmt) %>%
      filter(crop %in% input$which_crop)
  })

  filtered_surveys_long <- reactive({
    surveys_long %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(bee_num %in% input$which_bees) %>%
      filter(habitat %in% input$which_habitats) %>%
      filter(management %in% input$which_mgmt) %>%
      filter(crop %in% input$which_crop)
  })
  
  output$n_surveys <- renderText({
    paste(nrow(filtered_surveys()), "surveys match your filter selections.")})
  
  # reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
    updateCheckboxGroupInput(session, "which_bees", selected = 1:6)
    updateCheckboxGroupInput(session, "which_habitats", selected = habitat_types)
    updateCheckboxGroupInput(session, "which_crop", selected = crop_types)
    updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)
  })
  
  # bee buttons
  observeEvent(input$which_bees_all, {updateCheckboxGroupInput(session, "which_bees", selected = 1:6)})
  observeEvent(input$which_bees_none, {updateCheckboxGroupInput(session, "which_bees", selected = 0)})
  output$which_bees <- renderUI({
    checkboxGroupInput(
      "which_bees",
      label = "Bee group:",
      choiceNames = bee_ref$bee_name,
      choiceValues = 1:6,
      selected = 1:6)
  })
  
  # habitat buttons
  observeEvent(input$which_habitat_all, {updateCheckboxGroupInput(session, "which_habitats", selected = habitat_types)})
  observeEvent(input$which_habitat_none, {updateCheckboxGroupInput(session, "which_habitats", selected = "")})
  output$which_habitat <- renderUI({
    checkboxGroupInput(
      "which_habitats",
      label = "Habitat type:",
      choiceNames = habitat_types,
      choiceValues = habitat_types,
      selected = habitat_types)
  })

  # crop buttons
  observeEvent(input$which_crop_all, {updateCheckboxGroupInput(session, "which_crop", selected = crop_types)})
  observeEvent(input$which_crop_none, {updateCheckboxGroupInput(session, "which_crop", selected = "")})
    output$which_crop <- renderUI({
    checkboxGroupInput(
      "which_crop",
      label = "Crop type:",
      choiceNames = crop_types,
      choiceValues = crop_types,
      selected = crop_types)
  })
  
  # management buttons
  observeEvent(input$which_mgmt_all, {updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)})
  observeEvent(input$which_mgmt_none, {updateCheckboxGroupInput(session, "which_mgmt", selected = "")})
  output$which_mgmt <- renderUI({
    checkboxGroupInput(
      "which_mgmt",
      label = "Management type:",
      choiceNames = mgmt_types,
      choiceValues = mgmt_types,
      selected = mgmt_types)
  })
  
  bee_totals <- reactive({
    filtered_surveys_long() %>%
    group_by(bee_class) %>%
    summarise(tot_count = sum(count)) %>%
    mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))
    })
  
  ## survey site map ##
  output$surveyMap <- renderLeaflet({
    leaflet(survey_pts) %>%
      addTiles() %>%
      addRectangles(
        lng1 = ~ lng - .05, lng2 = ~ lng + .05,
        lat1 = ~ lat - .05, lat2 = ~ lat + .05,
        label = ~ paste(n_surveys, "surveys"),
        popup = ~ paste0(
          "<strong>Total surveys: </strong>", n_surveys, "<br/>",
          "<strong>Mean visits per minute:</strong><br/>",
          "Honey bees: ", hb, "<br/>",
          "Wild bees: ", wb, "<br/>",
          "Non-bees: ", nb),
        weight = 1,
        opacity = 1,
        color = "orange",
        fillOpacity = .25,
        fillColor = "yellow") %>%
      addMarkers(~lng, ~lat, icon = bee_icon,
        label = ~ paste(n_surveys, "surveys"),
        popup = ~ paste0(
          "<strong>Total surveys: </strong>", n_surveys, "<br/>",
          "<strong>Mean visits per minute:</strong><br/>",
          "Honey bees: ", hb, "<br/>",
          "Wild bees: ", wb, "<br/>",
          "Non-bees: ", nb),
        clusterOptions = markerClusterOptions(showCoverageOnHover = F)
      )
  })


  # simple survey data explorer
  output$beePlot1 <- renderPlot({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      df %>%
        group_by(date, bee_name) %>%
        summarise(mean_count = mean(count) / 5) %>%
        ggplot(aes(x = date, y = mean_count, fill = bee_name)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = bee_palette(input$which_bees)) +
        labs(x = "Survey date", y = "Average visits per minute", fill = "") +
        theme(text = element_text(size = 16))
      }
  })
  
  output$beePlot2 <- renderPlot({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      
      # this should all be possible with a pivot_longer call
      df_crop <- df %>%
        mutate(type = as.character(crop)) %>%
        group_by(type, bee_name) %>%
        summarise(
          mean_count = mean(count),
          n = n()) %>%
        mutate(type_label = "By crop")
      
      df_site <- df %>%
        mutate(type = as.character(habitat)) %>%
        group_by(type, bee_name) %>%
        summarise(
          mean_count = mean(count),
          n = n()) %>%
        mutate(type_label = "By habitat")
      
      df_mgmt <- df %>%
        mutate(type = as.character(management)) %>%
        group_by(type, bee_name) %>%
        summarise(
          mean_count = mean(count),
          n = n()) %>%
        mutate(type_label = "By management")
      
      df_bind <- bind_rows(df_crop, df_site, df_mgmt) %>%
        mutate(
          mean_count = mean_count / 5,
          type_label = factor(type_label, levels = c("By habitat", "By crop", "By management"))) %>%
        ungroup()
      
      df_labels <- df_bind %>%
        group_by(type_label, type) %>%
        summarise(n = mean(n))
      
      df_bind %>%
        ggplot() +
        geom_col(aes(x = type, y = mean_count, fill = bee_name)) +
        geom_text(
          data = df_labels,
          aes(x = type, y = -.1, label = paste0("(", n, ")")),
          size = 3) +
        scale_fill_manual(values = bee_palette(input$which_bees)) +
        labs(
          x = "",
          y = "Average visits per minute",
          fill = "") +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = .5,
          hjust = 1
        )) +
        facet_grid(. ~ type_label, scales = "free_x") +
        theme(text = element_text(size = 16))
    }
  })
}


# run app -----------------------------------------------------------------

shinyApp(ui, server)



# dustbin -----------------------------------------------------------------

# df_crop <- surveys_long %>%
#   mutate(type = as.character(crop)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By crop")
# df_site <- surveys_long %>%
#   mutate(type = as.character(habitat_type)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By site")
# df_mgmt <- surveys_long %>%
#   mutate(type = as.character(management_type)) %>%
#   group_by(type, bee_name) %>%
#   summarise(mean_count = mean(count), n = n()) %>%
#   mutate(type_label = "By management")
# df <- bind_rows(df_crop, df_site, df_mgmt) %>%
#   mutate(mean_count = mean_count / 5) %>%
#   group_by(type_label, type)
# df_labels <- df %>%
#   summarise(n = mean(n))
# 
# 
# 
# 
# df %>%
#   ggplot() +
#   geom_col(aes(x = type, y = mean_count, fill = bee_name)) +
#   geom_text(
#     data = df_labels,
#     aes(x = type, y = -.1, label = paste0("(", n, ")")),
#     size = 3) +
#   scale_fill_manual(values = bee_palette(1:6)) +
#   labs(x = "", y = "Average visits per minute", fill = "") +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
#   facet_grid(. ~ type_label, scales = "free_x")


# output$beePlotSiteType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(habitat_type, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       ggplot(aes(x = habitat_type, y = mean_count, fill = bee_name)) +
#       geom_bar(stat = "identity") +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Site type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })
# 
# output$beePlotCropType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(crop, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       arrange(desc(mean_count)) %>%
#       ggplot(aes(x = crop, y = mean_count, fill = bee_name)) +
#       geom_col() +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Crop type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })
# 
# output$beePlotMgmtType <- renderPlot({
#   df <- filtered_surveys_long()
#   if (nrow(df) > 0) {
#     df %>%
#       group_by(management_type, bee_name) %>%
#       summarise(mean_count = mean(count)) %>%
#       ggplot(aes(x = management_type, y = mean_count, fill = bee_name)) +
#       geom_col() +
#       scale_fill_manual(values = bee_palette(input$which_bees)) +
#       labs(x = "Management type", y = "Mean insect count", fill = "") +
#       coord_flip()}
# })

# # survey points for map, aggregated
# survey_pts1 <- surveys %>%
#   drop_na(lng, lat) %>%
#   mutate(
#     lng_rnd = round(lng, 1),
#     lat_rnd = round(lat, 1),
#     wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
#   group_by(lng_rnd, lat_rnd) %>%
#   summarise(
#     n_surveys = n(),
#     lng = mean(lng),
#     lat = mean(lat),
#     hb = round(mean(honeybee)/5,1),
#     wb = round(mean(wild_bee)/5,1),
#     nb = round(mean(non_bee)/5,1)) %>%
#   ungroup() %>%
#   select(-c(lng_rnd, lat_rnd)) %>%
#   mutate(
#     lng = lng + runif(length(.$lng), -.001, .001),
#     lat = lat + runif(length(.$lat), -.001, .001)
#   )


# leaflet map of survey sites (points)
# output$surveyMap1 <- renderLeaflet({
#   leaflet(survey_pts1) %>%
#     addTiles() %>%
#     addCircles(~ lng, ~ lat, radius = 3000, opacity = 0) %>%
#     addMarkers( ~ lng, ~ lat,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),)
# })


# # leaflet map
# output$surveyMap2 <- renderLeaflet({
#   leaflet(survey_pts2) %>%
#     addTiles() %>%
#     addRectangles(
#       lng1 = ~ lng - .005, lng2 = ~ lng + .005,
#       lat1 = ~ lat - .005, lat2 = ~ lat + .005,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),
#       weight = 2,
#       opacity = 1,
#       fillOpacity = .25)
# })



# observe({
#   leafletProxy("surveyMap", data = survey_pts2) %>%
#     clearMarkers() %>%
#     addMarkers(~lng, ~lat, icon = bee_icon,
#       label = ~ paste(n_surveys, "surveys"),
#       popup = ~ paste0(
#         "<strong>Total surveys: </strong>", n_surveys, "<br/>",
#         "<strong>Mean visits per minute:</strong><br/>",
#         "Honey bees: ", hb, "<br/>",
#         "Wild bees: ", wb, "<br/>",
#         "Non-bees: ", nb),
#       # options = markerOptions(opacity = op),
#       clusterOptions = markerClusterOptions(showCoverageOnHover = F)
#       )
# })