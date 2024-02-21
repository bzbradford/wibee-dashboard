# ui.R

# Define UI ----

lnk <- function(text, href) {
  a(text, href = href, target = "_blank")
}

ui <- fixedPage(
  
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  
  ## Metadata ----
  
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing pollinator activity surveys recorded by the WiBee app"),
    tags$meta(name = "keywords", content = "insect, pollinator, bee, dashboard, wisconsin, wibee, wisconsin"),
    tags$meta(id = "vp", name = "viewport", content = "width=device-width"),
    
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("www/google-analytics.html"),
    tags$script(src = "setViewport.js")
  ),
  useShinyjs(),
  
  
  ## Page header ----
  
  div(
    class = "page-title",
    div(img(src = "wibee-logo.png", height = 60)),
    div(
      align = "center",
      h2("WiBee Data Dashboard"),
      h4("View and explore pollinator data collected with the WiBee app")
    ),
    div(img(src = "uw-crest.png", height = 60))
  ),
  hr(style = "margin-top:0px"),
  br(),
  
  
  ## Introduction, summary, and instructions ----
  
  introUI(),
  br(),
  
  
  ## Survey filters ----
  
  h3(class = "section-heading", "Select and filter pollinator surveys"),
  surveyFiltersUI(),
  div(
    align = "center",
    style = "margin-top: 15px;",
    actionButton("surveyFilters-reset", "Reset all filters")
  ),
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
  materialSwitch(
    inputId = "group_wild",
    label = "Group wild bees together?",
    status = "success"
  ),
  

  ## Credits ----

  br(),
  hr(),
  br(),
  div(
    align = "center", style = "font-size: small; color: grey; border-top: 2px darkgrey;",
    p(
      style = "line-height: 2em;",
      "WiBee is a project of the", lnk("UW-Madison Department of Entomology", "https://entomology.wisc.edu/"), "and the", lnk("Gratton Lab", "https://gratton.entomology.wisc.edu/"),
      br(),
      lnk("More information", "http://www.pollinators.wisc.edu/wibee"),
      " - ", a("Email us", href = "mailto:pollinators@wisc.edu"),
      " - ", lnk("Sign up for our newsletter", "http://eepurl.com/gMqRdr"),
      " - ", lnk("Send feedback", "https://forms.gle/6qy9qJLwCxSTTPNT8"),
      " - ", lnk("View source code", "https://github.com/bzbradford/wibee-dashboard"),
      br(),
      "Dashboard developed by", lnk("Ben Bradford", "https://entomology.wisc.edu/directory/ben-bradford/"),
      " - ",
      "WiBee app developed by", lnk("Dan Imhoff", "https://caracal.tech/"),
    ),
    br(),
    p(em(status), br(), em(paste("Data last updated:", format(refresh_time, "%Y-%m-%d %H:%M:%S %Z"))))
  )
)
