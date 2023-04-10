# ui.R

# Define UI ----

ui <- fixedPage(
  
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing pollinator activity surveys recorded by the WiBee app"),
    tags$meta(name = "keywords", content = "insect, pollinator, bee, dashboard, wisconsin, wibee, wisconsin"),
    tags$meta(name = "viewport", content = "width=1024"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("google-analytics.html")
  ),
  
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
  
  

  # Introduction and summary ----

  sidebarLayout(
    mainPanel = mainPanel(
      h3("What is the WiBee app?", style = "margin-top:0px"),
      p("WiBee (pronounced We-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
      p("WiBee is a citizen science project where participants use the ", a("WiBee app", href = "http://www.pollinators.wisc.edu/wibee"), " to conduct 5-minute pollinator surveys of a 1 meter square section of flowering plants. Each time a pollinator (such as a honey bee, bumble bee, solitary wild bee, or other non-bee insect) lands on a flower, that's a visit! All the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
      p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"), "to learn more. Questions?", a("Email us.", href = "mailto:pollinators@wisc.edu"), "Comments?", a("Send feedback.", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank"), "Want to stay in the loop?", a("Sign up for our newsletter.", href = "http://eepurl.com/gMqRdr", target = "_blank"), "Thank you for participating!")),
    sidebarPanel = projectSummaryUI(),
    position = "right"),
  br(),
  

  # How to use ----
  
  bsCollapse(
    bsCollapsePanel(
      title = "Click for help on how to use this dashboard",
      p("This dashboard has two main sections: ", strong("selecting surveys"), " and ", strong("viewing survey data.")),
      p(strong("Step 1: Select geographic zones on the map."), "If you want to look only at surveys taken in a specific area, select those areas on the map tab."),
      p(strong("Step 2: Select date range."), "If you want to look at surveys from a specific year, or over a specific date range, specify those in the date tab."),
      p(strong("Step 3: Select survey characteristics."), "Use the checkboxes to narrow down what kind of habitat, crop, flower or management type you want to look at. Numbers in parentheses show the number of surveys that match each characteristic. You can also group wild bees together, or select specific kinds of bees to show in the plots."),
      tags$ul(
        tags$li(strong("Choose a habitat type(s)."), "If you run an orchard and you just want to look at the collective data from other orchards, filter the data by checking the 'orchard' box."),
        tags$li(strong("Choose a crop or flower type(s)."), "If you want to compare your apple bloom wild bee visit rate to other apple orchards, check the apple box to filter the data. Keep in mind that crops bloom at different times of year and have different inflorescences, so the bee visit rate and bee group composition will likely be different between crops."),
        tags$li(strong("Choose a management type(s)."), "These categories are subjective (chosen by the survey taker) and very broad, so take any variation between management types with a grain of salt."),
        tags$li(strong("Select or group bee types."), " This is optional, but you can group wild bees together (bumble bees, dark bees, and green bees), or select specific bee groups to show on the plots."),
      ),
      p(strong("Step 4: Making sense of the data."), "Look at the average flower visits per minute and the composition of the bee visitors."),
      tags$ul(
        tags$li("How do your overall bee visits per minute and wild bee visits per minute compare to the overall average?"),
        tags$li("What does your flower visit composition look like compared to the overall average? Do you have a lower, similar or higher percentage of wild bees compared to the overall average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?"),
        tags$li("You can make comparisons within this dashboard between select surveys and the overall averages, or you can look at your WiBee app survey data and compare individual surveys to the data you see here in the dashboard."))
    )
  ),
  br(),
  
  
  
  # Survey filters ----
  
  h4("Select and filter bee surveys:", style = "text-decoration: underline;"),
  
  surveyFiltersUI(),
  br(),
  
  

# Tabs with plot and data table displays ----

  h4("View or download data from selected surveys:", style = "text-decoration: underline;"),
  
  tabsetPanel(
    tabPanel("Species composition", speciesCompUI()),
    tabPanel("Activity by date", activityByDateUI()),
    tabPanel("Surveys by date", surveysByDateUI()),
    tabPanel("Compare habitats", activityByHabitatUI()),
    tabPanel("Compare managements", activityByMgmtUI()),
    tabPanel("Compare crops/flowers", activityByCropUI()),
    tabPanel("View on a map", activityMapUI()),
    tabPanel("View as data table", dataTableUI()),
    tabPanel("User statistics", userStatsUI())
  ),
  
  br(),
  div(actionButton("surveyFilters-reset", "Reset all filters"), style = "margin-top:15px", align = "center"),
  
  

# Credits ----
  
  br(),
  hr(),
  br(),
  div(
    align = "center",
    style = "font-size:small; color:grey; border-top:2px darkgrey",
    p(strong(paste0("©", format(Sys.Date(), "%Y"), " University of Wisconsin Board of Regents")), style = "font-size:small; color:grey"),
    p(
      a("More information", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"),
      " - ", a("Email us", href = "mailto:pollinators@wisc.edu"),
      " - ", a("Sign up for our newsletter", href = "http://eepurl.com/gMqRdr", target = "_blank"),
      " - ", a("Send feedback", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank")
    ),
    br(),
    p("Dashboard developed by", a("Ben Bradford", href = "https://github.com/bzbradford", target = "_blank"), style = "font-size:small; color:grey"),
    p("WiBee app developed by", a("Dan Imhoff", href = "https://caracal.tech/", target = "_blank"), style = "font-size:small; color:grey"),
    br(),
    p(em(status), br(), em(paste("Data last updated:", as.character(refresh_time, format = "%Y-%m-%d %H:%M:%S %Z"))))
  )
  
)
