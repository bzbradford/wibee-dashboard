# ui.R

# Define UI ----

ui <- fixedPage(
  
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  
  ## Metadata ----
  
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing pollinator activity surveys recorded by the WiBee app"),
    tags$meta(name = "keywords", content = "insect, pollinator, bee, dashboard, wisconsin, wibee, wisconsin"),
    tags$meta(name = "viewport", content = "width=1024"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("google-analytics.html")
  ),
  useShinyjs(),
  
  
  ## Page header ----
  
  fixedRow(
    column(
      width = 1,
      h2(img(src = "wibee-logo.png", height = 60))
    ),
    column(
      width = 10,
      align = "center",
      h2("WiBee Data Dashboard"),
      h4("View and explore pollinator data collected with the WiBee app")
    ),
    column(
      width = 1,
      h2(img(src = "uw-crest.png", height = 60))
    )
  ),
  hr(style = "margin-top:0px"),
  br(),
  
  
  ## Introduction, summary, and instructions ----
  
  introUI(),
  br(),
  
  
  ## Survey filters ----
  
  h3(class = "section-heading", "Select and filter pollinator surveys"),
  surveyFiltersUI(),
  br(),
  
  
  ## Data tabs ----

  h3(class = "section-heading", "View or download data from selected surveys"),
  
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
  div(
    align = "center",
    style = "margin-top:15px",
    actionButton("surveyFilters-reset", "Reset all filters")
  ),
  

  ## Credits ----
  
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
