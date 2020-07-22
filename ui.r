#### UI ####

library(tidyverse)
library(shiny)
library(shinythemes)
library(leaflet)
library(DT)



ui <- fixedPage(
  
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  # forces fixed width page
  HTML('<meta name="viewport" content="width=1024">'),
  
  
  # Page header
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
  
  
  # Introduction and project summary
  sidebarLayout(
    mainPanel = mainPanel(
      h3("What is the WiBee app?", style = "margin-top:0px"),
      p("WiBee (pronounced Wee-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
      p("WiBee is a citizen science project, so all the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
      p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"), "to learn more. Questions?", a("Email us.", href = "mailto:pollinators@wisc.edu"), "Comments?", a("Send feedback.", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank"), "Want to stay in the loop?", a("Sign up for our newsletter.", href = "http://eepurl.com/gMqRdr", target = "_blank"), "Thank you for participating!")),
    
    sidebarPanel = sidebarPanel(
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
    
    position = "right"),
  
  
  # How-to use
  br(),
  h3("How to use this dashboard"),
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
  br(),
  
  
  # Survey location map and summary data
  hr(),
  h3("View and filter by survey location:"),
  p(em("Click on individual grid cell(s) to show only results from those areas. Note: some surveys may be from outside Wisconsin. Click 'Zoom extents' to see them."), style = "margin-bottom:.5em"),
  leafletOutput("map", height = 600),
  div(style = "padding-top:10px",
    div(actionButton("map_zoom_all", "Zoom extents"), style = "padding-right:10px; display:inline-block"),
    div(actionButton("map_zoom_wi", "Zoom Wisconsin"), style = "padding-right:10px; display:inline-block"),
    div(actionButton("reset_map", "Reset map selections"), style = "padding-right:10px; display:inline-block"),
    div(strong(textOutput("survey_count_loc")), style = "display:inline-block")),
  br(),
  div("here I want a simple chart or table summarising the surveys picked on the map", style = "border:2px solid red; padding:5px; text-align:center"),
  br(),
  hr(),
  h3("Filter selected data by date:"),
  p(em("Move the slider to select only surveys selected on the map above and also from a specific date range."), style = "margin-bottom:.5em"),
  br(),
  fluidRow(style = "border:1px solid #ddd; background-color:#f1f1f1; border-radius:5px; padding:5px",
    column(8,
      sliderInput(
        "date_range",
        label = "Date range:",
        min = min_date,
        max = max_date,
        value = c(min_date, max_date),
        width = "100%")
    ),
    column(4, br(), actionButton("reset_date", "Reset dates"), align = "center")
  ),
  textOutput("survey_count_date"),
  h4("Pollinator visiation rate by day", align = "center"),
  plotOutput("plotByDate", height = "300px"),
  br(),
  hr(),
  h3("Select desired survey characteristics:"),
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
  actionButton("reset", "Reset filters"),
  br(),
  div(strong(textOutput("survey_count_final")), style = "font-size:larger; text-align:center"),
  br(),
  
  
  tabsetPanel(
    
    tabPanel("Summary data"),
    
    # Plots based on filtered data
    tabPanel("View summary charts",
      br(),
      # plotOutput("plotByDate", height = "400px"),
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