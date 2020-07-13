### WiBee Shiny App ###
# currently built under R 3.6.3 for compatibility with data-viz.it.wisc.edu


library(shiny)
library(shinythemes)
library(httr)
library(tidyverse)
library(leaflet)
library(DT)


# run if reading in from saved csv
# wibee_in <- read_csv("wibee-surveys.csv")
# lastUpdate = max(surveys$date)
# Sys.setenv(lastUpdate = "2020-07-02")
# Sys.unsetenv("lastUpdate")
# Sys.time()


if(file.exists("./data/refresh_time")) {
  refresh_time <- readRDS("./data/refresh_time")
} else {
  refresh_time <- as.POSIXct("2020-01-01")
}

# update surveys at most once an hour
if(refresh_time < Sys.time() - 3600) {
  get_surveys <-
    content(
      GET(url = "https://wibee.caracal.tech/api/data/survey-summaries",
        config = add_headers(Authorization = Sys.getenv("caracal_token")))
      )
  if(is.data.frame(get_surveys)) {
    arrange(get_surveys, ended_at) %>% write_csv("./data/surveys.csv")
    refresh_time <- Sys.time()
    saveRDS(refresh_time, "./data/refresh_time")
    msg <- "Survey data refreshed from remote database."
  } else {
    msg <- "Unable to refresh data from server, falling back on local cache."
  }
} else {
  msg <- "Survey data refreshed recently."
}

wibee_in <- read_csv("./data/surveys.csv", col_types = cols())

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

# bee count column names
bee_cols <- c(
  "honeybee",
  "bumble_bee",
  "large_dark_bee",
  "small_dark_bee",
  "greenbee",
  "non_bee"
)

# formatted bee names for ungrouped
bee_names <- c(
  "Honey bees",
  "Bumble bees",
  "Large dark bees",
  "Small dark bees",
  "Green bees",
  "Non-bees")

# formatted names for wild bee grouping
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
    "cranberry",
    "other berry",
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
      crop %in% crop_types ~ crop,
      grepl("berry", crop) ~ "other berry",
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
    nb = round(mean(non_bee)/5,1),
    .groups = "drop") %>%
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
  summarise(tot_count = sum(count), .groups = "drop") %>%
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



###############
## DEFINE UI ##
###############

ui <- fixedPage(
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  HTML('<meta name="viewport" content="width=1024">'),
  
  # page heading ##
  fixedRow(
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
        p("WiBee (pronounced Wee-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
        p("WiBee is a citizen science project, so all the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
        p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"), "to learn more. Questions?", a("Email us.", href = "mailto:pollinators@wisc.edu"), "Comments?", a("Send feedback.", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank"), "Want to stay in the loop?", a("Sign up for our newsletter.", href = "http://eepurl.com/gMqRdr", target = "_blank"), "Thank you for participating!")),
    position = "right"),

  
  # Survey location map and summary data
  h3("Survey locations", style = "border-bottom:1px grey"),
  p(em("Click on each bee icon or rectangle to show average counts for all surveys conducted within that area."), style = "margin-bottom:.5em"),
  leafletOutput("surveyMap"),
  br(),
  
  
  hr(),
  h3("Data explorer"),
  p(em("View and filter survey data submitted by you and other users of the app."), style = "margin-bottom:.5em"),
  br(),
  
  # Tabset with blank 1st tab and How-To on second tab
  tabsetPanel(
    tabPanel("Jump right in", br()),
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
        tags$li("What does your flower visit composition look like compared to the Wisconsin average? Do you have a lower, similar or higher percentage of wild bees compared to the Wisconsin average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?")
        )
      )
    ),

  
  # Apply filters
  h4("Filter survey data:"),
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
    column(4, br(), actionButton("reset", "Reset filters"), align = "center")
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
  br(),
  
 
  tabsetPanel(
    
     # Plots based on filtered data
    tabPanel("View summary charts",
      br(),
      plotOutput("plotByDate", height = "400px"),
      br(),
      br(),
      br(),
      plotOutput("plotByCat", height = "400px")
    ),
    
    # Tabular survey data
    tabPanel("View as data table",
      br(),
      p("The table below shows the average visitation rate per minute for the surveys and insect categories selected by the filters above. Check or uncheck the grouping variables to simplify or expand the summary table."),
      br(),
      checkboxGroupInput(
        "dtGroups",
        label = "Select which variables to include in table:",
        choiceNames = c("Date", "Habitat", "Crop", "Management"),
        choiceValues = c("date", "habitat", "crop", "management"),
        selected = c("habitat", "crop", "management"),
        inline = T
      ),
      DTOutput("summaryTable")
    ),
    
    # user stats
    tabPanel("User statistics",
      br(),
      plotOutput("plotUserStats", height = "400px")
    )
  ),
  
  
  ## Credits ##
  
  br(),
  hr(),
  br(),
  div(
    align = "center",
    style = "font-size:small; color:grey; border-top:2px darkgrey",    
    p(strong("©2020 University of Wisconsin Board of Regents"), style = "font-size:small; color:grey"),
    p(
      a("More information", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"),
        " - ", a("Email us", href = "mailto:pollinators@wisc.edu"),
        " - ", a("Sign up for our newsletter", href = "http://eepurl.com/gMqRdr", target = "_blank"),
        " - ", a("Send feedback", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank")
      ),
    br(),
    p("developed by", a("tanuki.tech", href = "https://github.com/bzbradford", target = "_blank"), style = "font-size:small; color:grey"),
    br(),
    p(em(paste(msg, "Last update:", as.character(refresh_time, format="%Y-%m-%d %H:%M:%S %Z"))))
    )
  
)



###################
## DEFINE SERVER ##
###################

server <- function(input, output, session) {
  
  # filter survey data by date slider
  surveys_datefilter <- reactive({
    surveys %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  # filter survey data by checkboxes
  filtered_surveys <- reactive({
    surveys_datefilter() %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt)
  })
  
  # filter long dataset by date slider and checkboxes
  filtered_surveys_long <- reactive({
    surveys_long %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(habitat %in% input$which_habitat) %>%
      filter(crop %in% input$which_crop) %>%
      filter(management %in% input$which_mgmt) %>% 
      filter(bee_name %in% input$which_bees)
  })
  
  # update checkbox labels when date slider is moved
  observeEvent(input$date_range, {
    habitat_labels <- surveys_datefilter() %>%
      count(habitat, .drop = F) %>%
      mutate(label = paste0(habitat, " (", n, ")")) %>%
      .$label
    crop_labels <- surveys_datefilter() %>%
      count(crop, .drop = F) %>%
      mutate(label = paste0(crop, " (", n, ")")) %>%
      .$label
    mgmt_labels <- surveys_datefilter() %>%
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

  # Number of matching surveys
  output$n_surveys <- renderText({
    paste(nrow(filtered_surveys()), "surveys match your filter selections.")
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
  
  
  # Survey site map - not currently reactive to data filters
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
        labs(
          title = "Pollinator visitation rate by survey date",
          x = "Survey date", y = "Average visits per minute", fill = "") +
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
  

# run app -----------------------------------------------------------------

shinyApp(ui, server)


