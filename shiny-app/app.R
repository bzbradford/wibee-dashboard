### WiBee Shiny App ###

## Notes / To Do ##
# consider renaming variables in camelCase
# add number of surveys matching each filter category for each category item
# map shows larger grid cells at wider zoom levels with aggregated data for those levels
# update wibee icon for map
# group bees by honey bee/wild bee/non-bee
# move honeybee before bumblebee


library(shiny)
library(httr)
library(tidyverse)
library(leaflet)


# Script ------------------------------------------------------------------

# read csv
# wibee_in <- suppressMessages(read_csv("wibee-surveys.csv"))

#apiKey <- Sys.getenv('survey_summaries_token')

# load current surveys
get_surveys <-
  content(
    GET(
      url = "https://wibee.caracal.tech/api/data/survey-summaries",
      config = add_headers(Authorization = print(Sys.getenv('survey_summaries_token')))
    )
  )
      #config = add_headers(Authorization = print(apiKey))
      #config = add_headers(Authorization = "9cd665b8-95d2-4bc2-be7d-587210a5666d")

# check if GET returned valid data frame
if(is.data.frame(get_surveys)) {
  wibee_in <- get_surveys
  wibee_in %>% write_csv("wibee-surveys-downloaded.csv")
}

unique(wibee_in$site_type)


# explanatory cols to keep
keep_cols <- c(
  "id",
  "user_id",
  "lat",
  "lng",
  "ended_at",
  "duration",
  "site_type",
  "crop",
  "management_type")

bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
)

bee_names <- c(
  "Honey bees",
  "Bumble bees",
  "Large dark bees",
  "Small dark bees",
  "Green bees",
  "Non-bees")

wildbee_names <- c(
  "Honey bees",
  "Wild bees",
  "Non-bees"
)


# bee crossref
bee_ref <-
  tibble(
    bee_type = c(
      "honeybee",
      "bumble_bee",
      "large_dark_bee",
      "small_dark_bee",
      "greenbee",
      "wild_bee",
      "non_bee"),
    bee_name = fct_inorder(c(
      "Honey bees",
      "Bumble bees",
      "Large dark bees",
      "Small dark bees",
      "Green bees",
      "Wild bees",
      "Non-bees")
      ),
    bee_color = fct_inorder(c(
      "#eca500",
      "#d86d27",
      "#758BFD",
      "#AEB8FE",
      "#99b5aa",
      "#5f8475",
      "#949494")
      )
    )

# Valid habitat types
habitat_types <- 
  c("corn-soybeans-alfalfa",
    "fruit-vegetable-field",
    "orchard",
    "road-field-edge",
    "lawn-and-garden",
    "prairie",
    "woodland")

# valid crop types
crop_types <- 
  c("apple",
    "cherry",
    "berry",
    "cucumber",
    "melon",
    "squash",
    "other",
    "none")

# valid management types
mgmt_types <- 
  c("organic",
    "conventional",
    "other",
    "unknown")


# generate main dataset
surveys <- wibee_in %>%
  select(all_of(c(keep_cols, bee_cols))) %>%
  mutate(ended_at = as.Date(ended_at)) %>%
  rename(habitat = site_type, management = management_type, date = ended_at) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  mutate(wild_bee = bumble_bee + large_dark_bee + small_dark_bee + greenbee) %>%
  mutate(habitat = factor(habitat, levels = habitat_types)) %>%
  mutate(crop = tolower(crop)) %>%
  mutate(crop = factor(
    case_when(
      is.na(crop) ~ "none",
      grepl("berry", crop) ~ "berry",
      crop %in% crop_types ~ crop,
      T ~ "other"), levels = crop_types)) %>%
  mutate(management = factor(
    case_when(
      management %in% mgmt_types ~ management,
      T ~ "other"), levels = mgmt_types)) %>%
  filter(duration == "5 minutes") %>%
  filter(date >= "2020-04-01") %>%
  drop_na(c(habitat, crop, management))

# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_ref$bee_type, names_to = "bee_type", values_to = "count") %>%
  left_join(bee_ref, by = "bee_type")


# generate grid points and summary statistics
survey_pts <- surveys %>%
  drop_na(lng, lat) %>%
  mutate(
    lng2 = round(lng, 1),
    lat2 = round(lat, 1)) %>%
  group_by(lng2, lat2) %>%
  summarise(
    n_surveys = n(),
    lng = mean(lng2),
    lat = mean(lat2),
    hb = round(mean(honeybee)/5,1),
    wb = round(mean(wild_bee)/5,1),
    nb = round(mean(non_bee)/5,1)) %>%
  ungroup() %>%
  select(-c(lng2, lat2))


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
  filter(bee_name %in% wildbee_names) %>%
  group_by(bee_name) %>%
  summarise(tot_count = sum(count)) %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))


## generate initial filter labels ##
habitat_labels <- {
  surveys %>%
    count(habitat, .drop = F) %>%
    mutate(label = paste0(habitat, " (", n, ")")) %>%
    .$label
}
crop_labels <- {
  surveys %>%
    count(crop, .drop = F) %>%
    mutate(label = paste0(crop, " (", n, ")")) %>%
    .$label
}
mgmt_labels <- {
  surveys %>%
    count(management, .drop = F) %>%
    mutate(label = paste0(management, " (", n, ")")) %>%
    .$label
}



# Define ui ---------------------------------------------------------------

ui <- fixedPage(
  
  ## page heading ##
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
  
  ## Intro and project survey ##
  sidebarLayout(
    sidebarPanel(
      p(strong("Project summary")),
      p("Unique users: ", length(unique(surveys$user_id))),
      p("Total completed surveys: ", nrow(surveys)),
      p("Most recent survey: ", max(surveys$date)),
      p("Total insect observations: ", sum(surveys_long$count)),
      tags$ul(tags$li({
        x = filter(bee_totals, bee_name == "Honey bees")
        paste0("Honey bees: ", x$tot_count, " (", x$pct_count, ")")
      }),
        tags$li({
          x = filter(bee_totals, bee_name == "Wild bees")
          paste0("Wild bees: ", x$tot_count, " (", x$pct_count, ")")
        }),
        tags$li({
          x = filter(bee_totals, bee_name == "Non-bees")
          paste0("Non-bees: ", x$tot_count, " (", x$pct_count, ")")
        }))
    ),
      mainPanel(
        h3("What is the WiBee app?", style = "margin-top:0px"),
        p("WiBee (pronounced Wee-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms."),
        p("WiBee is a citizen science project, so all the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to this Wisconsin-wide data to help you make decisions about managing your local pollinator community or track any change over time."),
        p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee"), "to learn more. Thank you for participating!")),
    position = "right"),
  
  ## Survey location map and summary data ##
  h2("Survey locations", style = "border-bottom:1px grey"),
  p(em("Click on each bee icon or rectangle to show average counts for all surveys conducted within that area."), style = "margin-bottom:.5em"),
  leafletOutput("surveyMap"),
  br(),
  
  ## Tabset with How-To on page 2 ##
  h2("Data explorer"),
  tabsetPanel(
    tabPanel("View and filter data", br()),
    tabPanel("How to use this dashboard",
      br(),
      p(strong("Step 1: Choose a habitat type(s)."), "If you run an orchard and you just want to look at the collective data from other orchards in Wisconsin, filter the data by checking the “orchard” box."),
      p(strong("Step 2: Choose a crop type(s)."), "If you want to compare your apple bloom wild bee visit rate to other apple orchards in Wisconsin, check the apple box to filter the data. Keep in mind that crops bloom at different times of year and have different inflorescences, so the bee visit rate and bee group composition will likely be different between crops."),
      p(strong("Step 3: Choose a management type(s)."), "These categories are subjective (chosen by the survey taker) and very broad, so take any variation between conventional, organic or “other” management styles with a grain of salt."),
      p(strong("Step 4: Explore the bee groups."), "We recommend looking at the bee data in three different combinations:"),
      tags$ul(
        tags$li("Visits by all the bees combined (honey bees and wild bees)"),
        tags$li("Visits by honey bees compared to wild bees"),
        tags$li("Visits by each individual bee group (bumble bee, honey bee, large dark bee, small dark bee, green bee)")),
      p(strong("Step 5: Making sense of the data."), "Look at the average flower visits per minute and the composition of the bee visitors."),
      tags$ul(
        tags$li("How do your overall bee visits per minute and wild bee visits per minute compare to the Wisconsin average?"),
        tags$li("What does your flower visit composition look like compared to the Wisconsin average? Do you have a lower, similar or higher percentage of wild bees compared to the Wisconsin average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?")),
      hr()
      )
    ),
  
  ## Filter data ##
  fluidRow(
    column(8,
      sliderInput(
        "date_range",
        label = "Date range:",
        min = min_date,
        max = max_date,
        value = c(min_date, max_date),
        width = "100%")
      ),
    column(4, actionButton("reset", "Reset filters"), align = "center")
    ),
  fluidRow(
    column(3,
      checkboxGroupInput(
        "which_bees",
        label = "Bee group:",
        choiceNames = bee_names,
        choiceValues = bee_names,
        selected = bee_names),
      div(actionButton("which_bees_all", "All"), style = "display:inline-block"),
      div(actionButton("which_bees_none", "None"), style = "display:inline-block"),
      checkboxInput("group_wild", label = "Group wild bees together")),
    column(3,
      checkboxGroupInput(
        "which_habitat",
        label = "Habitat type:",
        choiceNames = habitat_labels,
        choiceValues = habitat_types,
        selected = habitat_types),
      div(actionButton("which_habitat_all", "All"), style = "display:inline-block"),
      div(actionButton("which_habitat_none", "None"), style = "display:inline-block")),
    column(3,
      checkboxGroupInput(
        "which_crop",
        label = "Crop type:",
        choiceNames = crop_labels,
        choiceValues = crop_types,
        selected = crop_types),
      div(actionButton("which_crop_all", "All"), style = "display:inline-block"),
      div(actionButton("which_crop_none", "None"), style = "display:inline-block")),
    column(3,
      checkboxGroupInput(
        "which_mgmt",
        label = "Management type:",
        choiceNames = mgmt_labels,
        choiceValues = mgmt_types,
        selected = mgmt_types),
      div(actionButton("which_mgmt_all", "All"), style = "display:inline-block"),
      div(actionButton("which_mgmt_none", "None"), style = "display:inline-block"))
  ),
  br(),
  div(strong(textOutput("n_surveys")), style = "font-size:larger; text-align:center"),
  hr(),
  br(),
  
  ## Plots based on filtered data ##
  tabsetPanel(
    tabPanel(
      "View by date",
      h4("Pollinator activity per minute by survey date"),
      plotOutput("beePlot1", height = "300px"),
    ),
    tabPanel(
      "View by survey characteristics",
      h4("Pollinator activity by survey site characteristics"),
      plotOutput("beePlot2"),
    ),
    tabPanel(
      "Flower visit frequency",
      h4("Frequency of flower visits by pollinator group"),
      plotOutput("beePlot3"),
    )
  ),
  
  hr(),
  br(), 
  
  ## Credits ##
  br(),
  p(strong("©2020 University of Wisconsin Board of Regents"), align = "center", style = "font-size:small; color:grey"),
  p("developed by tanuki.tech", align = "center", style = "font-size:small; color:grey")
  
)


# Define server -----------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_surveys <- reactive({
    surveys %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt)
    })


  filtered_surveys_long <- reactive({
    surveys_long %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt) %>% 
      filter(bee_name %in% input$which_bees)
  })
  
  ## Refresh bee selection checkbox, depending on yes/no wild bee grouping selection ##
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

  
  ## Number of matching surveys ##
  output$n_surveys <- renderText({
    paste(nrow(filtered_surveys()), "surveys match your filter selections.")
  })
  
  
  ## Reset button ##
  observeEvent(input$reset, {
    updateSliderInput(session, "date_range", value = c(min_date, max_date))
    updateCheckboxGroupInput(session, "which_habitat", selected = habitat_types)
    updateCheckboxGroupInput(session, "which_crop", selected = crop_types)
    updateCheckboxGroupInput(session, "which_mgmt", selected = mgmt_types)
    updateCheckboxInput(session, "group_wild", value = F)
    reset_bees()
  })
  
  
  ## Group wild bees together ##
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

  
  ## Plot of daily bee averages ##
  output$beePlot1 <- renderPlot({
    df <- filtered_surveys_long()
    if (nrow(df) > 0) {
      df2 <- df %>%
        group_by(date, bee_name, bee_color) %>%
        summarise(visit_rate = mean(count) / 5) %>%
        droplevels()
      
      df2 %>%
        ggplot(aes(x = date, y = visit_rate, fill = bee_name)) +
        geom_col() +
        scale_fill_manual(values = levels(df2$bee_color)) +
        labs(x = "Survey date", y = "Average visits per minute", fill = "") +
        theme(text = element_text(size = 16))
    }
  })
  

  ## Plot of bee activity averages by site characteristics ##
  output$beePlot2 <- renderPlot({
    
    df <- filtered_surveys_long()
    
    if (nrow(df) > 0) {
      
      # create working dataset
      df2 <- df %>%
        rename(`By crop` = crop, `By habitat` = habitat, `By management` = management) %>%
        pivot_longer(cols = c("By habitat", "By crop", "By management"), names_to = "category") %>%
        mutate(category = factor(category, c("By habitat", "By crop", "By management"))) %>%
        group_by(category, value, bee_name, bee_color) %>%
        summarise(visit_rate = mean(count) / 5, n = n()) %>%
        droplevels()
      
      # get matching surveys counts by category
      survey_count <- df2 %>% group_by(category, value) %>% summarise(n = mean(n))
      
      # plot
      df2 %>%
        ggplot() +
        geom_col(aes(x = value, y = visit_rate, fill = bee_name)) +
        geom_text(
          data = survey_count,
          aes(x = value, y = -.15, label = paste0("(", n, ")")),
          size = 3) +
        scale_fill_manual(values = levels(df2$bee_color)) +
        labs(x = "", y = "Average visits per minute", fill = "") +
        theme(axis.text.x = element_text(angle = 90,vjust = .5, hjust = 1)) +
        facet_grid(. ~ category, scales = "free_x") +
        theme(text = element_text(size = 16))
    }
  })
    
  
  
  ## histograms of total insect counts per survey
  output$beePlot3 <- renderPlot({
    
    df <- filtered_surveys_long()
    
    if (nrow(df) > 0) {
      
      # exclude zero-counts from histograms
      df2 <- df %>%
        filter(count > 0) %>%
        droplevels()
      
      # plot
      df2 %>%
        ggplot(aes(x = count, fill = bee_name)) +
        geom_histogram(bins = 20) +
        facet_wrap( ~ bee_name) +
        scale_fill_manual(values = levels(df2$bee_color)) +
        labs(x = "Number of flower visits per survey", y = "Number of surveys", fill = "")
    }
  })

}
  

# run app -----------------------------------------------------------------

shinyApp(ui, server)



# dustbin -----------------------------------------------------------------

# surveys_long %>%
#   filter(count > 0) %>%
#   mutate(visit_rate = count / 5) %>%
#   ggplot(aes(x = visit_rate, fill = bee_name)) +
#   geom_histogram() +
#   facet_wrap(~ bee_name, scale = "free_y") +
#   scale_fill_manual(values = bee_palette(levels(surveys_long$bee_name))) +
#   labs(x = "Visitation rate (when present)", y = "Number of surveys")
# 
# 

# df_in <- surveys_long
# if (nrow(df) > 0) {
#   df <- df_in %>%
#     droplevels() %>%
#     rename(`By crop` = crop, `By habitat` = habitat, `By management` = management) %>%
#     pivot_longer(cols = c("By crop", "By habitat", "By management")) %>%
#     group_by(name, value, bee_class, bee_name) %>%
#     summarise(visit_rate = mean(count) / 5, n = n())
#   df_labels <- df %>% group_by(name, value) %>% summarise(n = n[1])
#   
#   if (T) {
#     plt <- df %>%
#       summarise(visit_rate = sum(visit_rate)) %>%
#       ggplot() +
#       geom_col(aes(x = value, y = visit_rate, fill = bee_class)) +
#       geom_text(
#         data = df_labels,
#         aes(x = value, y = -.1, label = paste0("(", n, ")")),
#         size = 3) +
#       scale_fill_manual(values = bee_palette(levels(df$bee_class)))
#   } else {
#     plt <- df %>%
#       ggplot() +
#       geom_col(aes(x = value, y = visit_rate, fill = bee_name)) +
#       geom_text(
#         data = {group_by(., name, value) %>% summarise(n = n[1])},
#         aes(x = value, y = -.1, label = paste0("(", n, ")")),
#         size = 3) +
#       scale_fill_manual(values = bee_palette(levels(df$bee_name)))
#   }
#   
#   plt +
#     labs(x = "", y = "Average visits per minute", fill = "") +
#     theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) +
#     facet_grid(. ~ name, scales = "free_x") +
#     theme(text = element_text(size = 16))
# }


# observeEvent(filtered_surveys(), {
#   df <- filtered_surveys()
#   which_habitat <- input$which_habitat
#   which_crop <- input$which_crop
#   which_mgmt <- input$which_mgmt
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_habitat",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(habitat, .drop = F) %>%
#         mutate(label = paste0(habitat, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = habitat_types,
#     selected = which_habitat
#   )
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_crop",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(crop, .drop = F) %>%
#         mutate(label = paste0(crop, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = crop_types,
#     selected = which_crop
#   )
#   
#   updateCheckboxGroupInput(
#     session,
#     "which_mgmt",
#     choiceNames = {
#       filtered_surveys() %>%
#         count(management, .drop = F) %>%
#         mutate(label = paste0(management, " (", n, ")")) %>%
#         .$label
#     },
#     choiceValues = mgmt_types,
#     selected = which_mgmt
#   )
# })



# left_join(
#   tibble(management = as.factor(mgmt_types)),
#   surveys %>%
#     group_by(management) %>%
#     summarise(n = n()),
#   by = "management") %>%
#   replace_na(list(n = 0)) %>%
#   mutate(label = paste0(management, " (", n, ")")) %>%
#   .$label






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
