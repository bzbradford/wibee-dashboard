### WiBee Shiny App ###

library(shiny)
library(tidyverse)
library(leaflet)


# Script ------------------------------------------------------------------

# read csv
wibee_in <- read_csv("wibee-surveys.csv")

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
drop_cols <-c(
    "picture_id",
    "picture_url"
)
bee_cols_amended <- 
  c("bumble_bee_amended",
    "honeybee_amended",
    "large_dark_bee_amended",
    "small_dark_bee_amended",
    "greenbee_amended",
    "non_bee_amended")

# color scheme
bee_palette <- function(bees = 1:6) {
  bees = as.integer(bees)
#  pal = c("red","orange","yellow","green","blue","purple")
  pal = c("#972D07","#FFA400","#758BFD","#AEB8FE","#8CB369","#D664BE")
  return(pal[bees])
}

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


# generate main dataset
surveys <- wibee_in %>%
  select(-drop_cols) %>%
  select(-bee_cols_amended) %>%
  filter(duration == "5 minutes") %>%
  mutate_at(fct_cols, as.factor) %>%
  mutate_at(bee_cols, replace_na, 0) %>%
  mutate(date = as.Date(ended_at)) %>%
  filter(date >= "2020-01-01")

# pivot longer for plotting etc
surveys_long <- surveys %>%
  pivot_longer(bee_cols, names_to = "species", values_to = "count") %>%
  left_join(bee_ref, by = "species") %>%
  mutate(bee_name = factor(bee_name, levels = bee_ref$bee_name))

# get date range of data
min_date <- min(surveys$date)
max_date <- max(surveys$date)

# survey points for map
survey_pts <- surveys %>%
  select(id, lng, lat) %>%
  group_by(lng, lat) %>%
  summarise(n = n())

bee_totals <- surveys_long %>%
  group_by(bee_class) %>%
  summarise(tot_count = sum(count)) %>%
  mutate(pct_count = sprintf("%1.1f%%", tot_count / sum(.$tot_count) * 100))

filter(bee_totals, bee_class == "Wild bees")$tot_count

# App ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(
    fluidRow(
      column(11, "WiBee Data Dashboard"),
      column(1, img(src = "wibee-logo.png", align = "right"))
      )
    ),
  h4("View and explore pollinator data collected with the WiBee app"),
  br(),
  sidebarLayout(
    sidebarPanel(
      p(strong("Project summary")),
      p("Unique users: ", length(unique(surveys$user_id))),
      p("Total completed surveys: ", nrow(surveys)),
      p("Total insect observations: ", sum(surveys_long$count)),
      p("Most recent observation: ", max(surveys$date)),
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
#      p("Spring is here and Wisconsin’s native, wild bees are about to emerge and begin foraging for pollen and nectar on blooming flowers. We invite you to use The WiBee App to survey bees on your farm or property as soon as the flowers bloom and the weather is appropriate for bee activity: about 60 degrees or warmer and sunny/partly cloudy."),
      h3("What is the WiBee app?"),
      p("WiBee (pronounced Wee-bee) is a new smartphone app developed by the Gratton Lab at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms."),
      p("With your help, we can provide growers with better pollination management recommendations specific to individual farms and share more information about the diversity, abundance and value of Wisconsin’s wild bees.")
    )),
  br(),
  
    h2("Survey locations"),
    leafletOutput("surveyMap"),
    br(),
    h2("Daily bee counts across all surveys"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
            "date_range",
            label = h4("Date range:"),
            min = min_date,
            max = max_date,
            value = c(min_date, max_date)
        ),
        checkboxGroupInput(
            "which_bees",
            label = h4("Filter by bee group:"),
            choiceNames = bee_ref$bee_name,
            choiceValues = 1:6,
            selected = 1:6
            )
    ),
    mainPanel(
        plotOutput("surveyCount"))
    )
    
)

server <- function(input, output) {
  
  # leaflet map of survey sites
  output$surveyMap <- renderLeaflet({
    leaflet(survey_pts) %>%
      addTiles() %>%
      addMarkers( ~ lng, ~ lat)
  })
  
  # simple survey data explorer
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
      labs(x = "Survey date", y = "Mean insect count")
  })
}

shinyApp(ui, server)