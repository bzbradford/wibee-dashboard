### WiBee Shiny App ###

## Notes / To Do ##
# consider renaming variables in camelCase


library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(leaflet)


# Script ------------------------------------------------------------------

# read csv
wibee_in <- suppressMessages(read_csv("wibee-surveys.csv"))



### Data wrangling ###

# which columns to convert from char to factor
fct_cols <- c(
    "site_type",
    "cloud_cover",
    "wind_intensity",
    "temperature",
    "management_type",
    "crop"
)

# useless cols
drop_cols <- c(
  "picture_id",
  "picture_url",
  "bumble_bee_amended",
  "honeybee_amended",
  "large_dark_bee_amended",
  "small_dark_bee_amended",
  "greenbee_amended",
  "non_bee_amended"
)

# bee count data
bee_cols = c(
  "bumble_bee",
  "honeybee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
)

# crossref for bee name aliases
bee_ref <-
  tibble(
    bee_num = 1:6,
    species = c(
      "bumble_bee",
      "honeybee",
      "large_dark_bee",
      "small_dark_bee",
      "greenbee",
      "non_bee"
    ),
    bee_name = c(
      "Bumble bee",
      "Honey bee",
      "Large dark bee",
      "Small dark bee",
      "Green bee",
      "Non-bee"
    ),
    bee_class = c(
      "Wild bees",
      "Honey bees",
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
  pal = c("#972D07","#FFA400","#758BFD","#AEB8FE","#8CB369","#D664BE")
  return(pal[bees])
}

### For factors that have "other" ###

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

site_types <- levels(surveys$site_type)



# generate main dataset
surveys <- wibee_in %>%
  select(-drop_cols) %>%
  filter(duration == "5 minutes") %>%
  mutate(
    management_type =
      case_when(
        management_type %in% mgmt_types ~ management_type,
        T ~ "other"),
    crop =
      case_when(
        crop %in% crop_types ~ crop,
        T ~ "other"),
    date = as.Date(ended_at)) %>%  
  mutate_at(fct_cols, as.factor) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  filter(date >= "2020-01-01")


# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_cols, names_to = "species", values_to = "count") %>%
  left_join(bee_ref, by = "species") %>%
  mutate(bee_name = factor(bee_name, levels = bee_ref$bee_name))

# survey points for map, aggregated
survey_pts1 <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng_rnd = round(lng, 1),
    lat_rnd = round(lat, 1),
    wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  group_by(lng_rnd, lat_rnd) %>%
  summarise(
    n_surveys = n(),
    lng = mean(lng),
    lat = mean(lat),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1)) %>%
  ungroup() %>%
  select(-c(lng_rnd, lat_rnd)) %>%
  mutate(
    lng = lng + runif(length(.$lng), -.001, .001),
    lat = lat + runif(length(.$lat), -.001, .001)
  )

survey_pts2 <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng_rnd = round(lng, 2),
    lat_rnd = round(lat, 2),
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

# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)

# total counts for project summary
bee_totals <- surveys_long %>%
  group_by(bee_class) %>%
  summarise(tot_count = sum(count)) %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))

bee_icon <- makeIcon(
  iconUrl = "wibee-logo.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15)



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
  
  sidebarLayout(
    sidebarPanel(
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
      p("WiBee (pronounced Wee-bee) is a new smartphone app developed by the Gratton Lab at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms."),
      p("With your help, we can provide growers with better pollination management recommendations specific to individual farms and share more information about the diversity, abundance and value of Wisconsin’s wild bees."),
      p("Learn more at the ", a("Gratton lab website.", href = "https://pollinators.wisc.edu/wibee/"))
    ),
    position = "right"),
  
  br(),
  h2("Survey locations", style = "border-bottom:1px grey"),
  p(em("Click on each bee icon or rectangle to show average counts for all surveys conducted within that area."), style = "margin-bottom:.5em"),
#  leafletOutput("surveyMap"),
  
  br(),
  h2("Data explorer"),
  
  fluidRow(
    column(6,
      sliderInput(
        "date_range",
        label = h4("Date range:"),
        min = min_date,
        max = max_date,
        value = c(min_date, max_date)
        )
      ),
    column(6, actionButton("reset", "Reset selections"), align = "center")
    ),
  
  fluidRow(
    column(3,
      checkboxGroupInput(
        "which_bees",
        label = h4("Bee group:"),
        choiceNames = bee_ref$bee_name,
        choiceValues = 1:6,
        selected = 1:6
        )
      ),
    column(3,
      checkboxGroupInput(
        "which_sites",
        label = h4("Site type:"),
        choiceNames = site_types,
        choiceValues = site_types,
        selected = site_types
      )
    ),
    column(3,
      checkboxGroupInput(
        "which_mgmt",
        label = h4("Management type:"),
        choiceNames = mgmt_types,
        choiceValues = mgmt_types,
        selected = mgmt_types
      )),
    column(3,
      checkboxGroupInput(
        "which_crop",
        label = h4("Crop type:"),
        choiceNames = crop_types,
        choiceValues = crop_types,
        selected = crop_types
      )),
    ), 
  br(),
  textOutput("n_surveys"),
  br(),
  plotOutput("surveyCount"),
  
  br(),
  br(),
  p(strong("©2020 University of Wisconsin Board of Regents"), align = "center", style = "font-size:small; color:grey"),
  p("developed by tanuki.tech", align = "center", style = "font-size:small; color:grey")
  
)



# define server -----------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    print(input$date_range)
    print(input$which_bees)
    print(input$which_sites)
    print(input$which_mgmt)
    print(input$which_crop)
  })
  
  filtered_surveys <- reactive({
    surveys %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    # %>%
      # filter(site_type %in% input$which_sites) %>%
      # filter(management_type %in% input$which_mgmt) %>%
      # filter(crop %in% input$which_crop)
  })

  filtered_surveys_long <- reactive({
    surveys_long %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(bee_num %in% input$which_bees) %>%
      filter(site_type %in% input$which_sites) %>%
      filter(management_type %in% input$which_mgmt) %>%
      filter(crop %in% input$which_crop)
  })
  
  output$n_surveys <- renderText({
    paste(nrow(filtered_surveys()), "surveys match your filter selections.")})
  
  output$surveyMap <- renderLeaflet({
    leaflet(survey_pts2) %>%
      addTiles() %>%
      addRectangles(
        lng1 = ~ lng - .005, lng2 = ~ lng + .005,
        lat1 = ~ lat - .005, lat2 = ~ lat + .005,
        label = ~ paste(n_surveys, "surveys"),
        popup = ~ paste0(
          "<strong>Total surveys: </strong>", n_surveys, "<br/>",
          "<strong>Mean visits per minute:</strong><br/>",
          "Honey bees: ", hb, "<br/>",
          "Wild bees: ", wb, "<br/>",
          "Non-bees: ", nb),
        weight = 1,
        opacity = 1,
        fillOpacity = .25) %>%
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

  # reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
    updateCheckboxGroupInput(session, "which_bees", selected = 1:6)
    updateCheckboxGroupInput(session, "which_sites", selected = site_types)
    updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)
    updateCheckboxGroupInput(session, "which_crop", selected = crop_types)
  })
  
#  simple survey data explorer
  output$surveyCount <- renderPlot({
    df <- surveys_long %>%
      filter(date >= input$date_range[1] &
               date <= input$date_range[2]) %>%
      filter(bee_num %in% input$which_bees) %>%
      group_by(date, bee_name) %>%
      summarise(mean_count = mean(count, na.rm = T))
    ggplot(df, aes(x = date, y = mean_count, fill = bee_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = bee_palette(input$which_bees)) +
      labs(x = "Survey date", y = "Mean insect count", fill = "")
  })
  
  # output$surveyCount <- renderPlot({
  #   df <- filtered_surveys_long()
  #   df %>%
  #     group_by(date, bee_name) %>%
  #     summarise(mean_count = mean(count, na.rm = T)) %>%
  #     ggplot(aes(x = date, y = mean_count, fill = bee_name)) +
  #     geom_bar(stat = "identity") +
  #     scale_fill_manual(values = bee_palette(input$which_bees)) +
  #     labs(x = "Survey date", y = "Mean insect count", fill = "")
  # })
}


# run app -----------------------------------------------------------------

shinyApp(ui, server)




# dustbin -----------------------------------------------------------------

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

# 
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