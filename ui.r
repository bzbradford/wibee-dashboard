#---- UI ----#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(DT)
library(plotly)



# Define UI ---------------------------------------------------------------

ui <- fixedPage(
  
  # Google analytics
  tags$head(includeHTML(("google-analytics.html"))),
  
  # forces fixed width page
  HTML('<meta name="viewport" content="width=1024">'),  
  
  title = "WiBee Dashboard",
  theme = shinytheme("flatly"),
  
  
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
  
  

## Introduction and project summary ----------------------------------------

  sidebarLayout(
    mainPanel = mainPanel(
      h3("What is the WiBee app?", style = "margin-top:0px"),
      p("WiBee (pronounced We-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
      p("WiBee is a citizen science project, so all the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
      p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"), "to learn more. Questions?", a("Email us.", href = "mailto:pollinators@wisc.edu"), "Comments?", a("Send feedback.", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank"), "Want to stay in the loop?", a("Sign up for our newsletter.", href = "http://eepurl.com/gMqRdr", target = "_blank"), "Thank you for participating!")),
    
    sidebarPanel = sidebarPanel(
      p(strong("Project summary")),
      p("Unique users: ", format(length(unique(surveys$user_id)), big.mark = ",")),
      p("Total completed surveys: ", format(nrow(surveys), big.mark = ",")),
      p("Most recent survey: ", max(surveys$date)),
      p("Total insect observations: ", format(sum(surveys_long$count), big.mark = ",")),
      tags$ul(tags$li({
        x = filter(bee_totals, bee_name == "Honey bees")
        paste0("Honey bees: ", format(x$tot_count, big.mark = ","), " (", x$pct_count, ")")
      }),
        tags$li({
          x = filter(bee_totals, bee_name == "Wild bees")
          paste0("Wild bees: ", format(x$tot_count, big.mark = ","), " (", x$pct_count, ")")
        }),
        tags$li({
          x = filter(bee_totals, bee_name == "Non-bees")
          paste0("Non-bees: ", format(x$tot_count, big.mark = ","), " (", x$pct_count, ")")
        }))
    ),
    
    position = "right"),
  br(),
  
  br(),
  

  
## Tabs with map, filters, and how-to --------------------------------------

  h4(em("Browse and filter surveys by location or attribute:")),
  tabsetPanel(
    tabPanel(
      title = "Filter by survey location",
      p(em("Click on individual grid cell(s) to show only results from those areas. Note: some surveys may be from outside Wisconsin. Click 'Zoom extents' to see them."), style = "margin-top:.5em; margin-bottom:.5em"),
      leafletOutput("map", height = 600),
      div(style = "margin-top: 5px;",
        div(actionButton("map_select_visible", "Select visible"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_zoom_all", "Select all"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_clear_selection", "Clear selection"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_reset", "Reset map"), style = "padding-right:20px; display:inline-block"),
        div(strong(textOutput("survey_count_loc")), style = "display:inline-block"))
      ),
    
    tabPanel(
      title = "Filter by survey characteristics",
      p(em("Filter survey data by selecting which habitat(s), crop(s), and management type(s) you want to see data for."), style = "margin-top:.5em; margin-bottom:.5em"),
      div(
        style = "border:1px solid #ddd; background-color:#f1f1f1; border-radius:5px; padding:15px",
          sliderInput(
          "date_range",
          "Date range:",
          min = min_date,
          max = max_date,
          value = c(min_date, max_date),
          width = "100%"),
        fixedRow(
          column(width = 3,
            checkboxGroupInput(
              "which_bees",
              "Bee group:",
              choiceNames = bee_names,
              choiceValues = bee_names,
              selected = bee_names),
            materialSwitch("group_wild", label = "Group wild bees together", status = "success"),
            div(actionButton("which_bees_all", "All"), style = "display:inline-block"),
            div(actionButton("which_bees_none", "None"), style = "display:inline-block"),
            ),
          column(width = 3,
            checkboxGroupInput(
              "which_habitat",
              "Habitat:",
              choiceNames = levels(habitats$label),
              choiceValues = habitats$type,
              selected = habitats$type),
            div(actionButton("which_habitat_all", "All"), style = "display:inline-block"),
            div(actionButton("which_habitat_none", "None"), style = "display:inline-block"),
            ),
          column(width = 3,
            checkboxGroupInput(
              "which_crop",
              "Crop/Wildflower:",
              choiceNames = levels(crops$label),
              choiceValues = crops$type,
              selected = crops$type),
            div(actionButton("which_crop_all", "All"), style = "display:inline-block"),
            div(actionButton("which_crop_none", "None"), style = "display:inline-block"),
            ),
          column(width = 3,
            checkboxGroupInput(
              "which_mgmt",
              "Management:",
              choiceNames = levels(managements$label),
              choiceValues = managements$type,
              selected = managements$type),
            div(actionButton("which_mgmt_all", "All"), style = "display:inline-block"),
            div(actionButton("which_mgmt_none", "None"), style = "display:inline-block"),
            div(actionButton("reset", "Reset filters"), style = "margin-top:15px"),
            ),
          ),
        )
      ),
    
    tabPanel(
      title = "How to use this dashboard",
      br(),
      p(strong("Step 1: Select geographic zones on the map."), "If you want to look only at surveys taken in a specific area, select those areas on the map tab."),
      p(strong("Step 2: Select survey characteristics."), "Use the checkboxes to narrow down what kind of habitat, crop, or management type you want to look at. Numbers in parentheses show the number of surveys that match each characteristic."),
      tags$ul(
        tags$li(strong("Choose a habitat type(s)."), "If you run an orchard and you just want to look at the collective data from other orchards in Wisconsin, filter the data by checking the 'orchard' box."),
        tags$li(strong("Choose a crop type(s)."), "If you want to compare your apple bloom wild bee visit rate to other apple orchards in Wisconsin, check the apple box to filter the data. Keep in mind that crops bloom at different times of year and have different inflorescences, so the bee visit rate and bee group composition will likely be different between crops."),
        tags$li(strong("Choose a management type(s)."), "These categories are subjective (chosen by the survey taker) and very broad, so take any variation between conventional, organic or 'other' management styles with a grain of salt.")
      ),
      p(strong("Step 3: Explore the bee groups."), "We recommend looking at the bee data in three different combinations:"),
      tags$ul(
        tags$li("Visits by all the bees combined (honey bees and wild bees)"),
        tags$li("Visits by honey bees compared to wild bees"),
        tags$li("Visits by each individual bee group (bumble bee, honey bee, large dark bee, small dark bee, green bee)")),
      p(strong("Step 4: Making sense of the data."), "Look at the average flower visits per minute and the composition of the bee visitors."),
      tags$ul(
        tags$li("How do your overall bee visits per minute and wild bee visits per minute compare to the Wisconsin average?"),
        tags$li("What does your flower visit composition look like compared to the Wisconsin average? Do you have a lower, similar or higher percentage of wild bees compared to the Wisconsin average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?"))
      )
    ),
  
  
  
## Number of selected surveys ----------------------------------------------

  br(),
  br(),
  div(strong(textOutput('survey_count_final')), style = "font-size:larger; text-align:center; border:1px solid #ddd; background-color:#f1f1f1; border-radius:5px; padding:15px;"),
  br(),
  br(),
  
  

## Tabs with plot and data table displays ----------------------------------------

  h4(em("View or download survey data:")),
  tabsetPanel(
    
    tabPanel("Species composition",
      br(),
      plotlyOutput("map_chart_all", width = "45%", inline = T),
      plotlyOutput("map_chart_selected", width = "45%", inline = T),
      br()),
    
    tabPanel("Activity by date",
      br(),
      plotlyOutput("plotByDate"),
      br()),
    
    tabPanel("Compare habitats",
      br(),
      plotlyOutput("plotByHabitat"),
      br()),
    
    tabPanel("Compare crops/flowers",
      br(),
      plotlyOutput("plotByCrop"),
      br()),
    
    tabPanel("Compare managements",
      br(),
      plotlyOutput("plotByMgmt"),
      br()),
    
    # Tabular survey data
    tabPanel("View as data table",
      br(),
      p("The table below shows the average visitation rate per minute for the surveys and insect categories selected by the filters above. Check or uncheck the grouping variables to simplify or expand the summary table. Click the download button to save a copy of the data you have selected.", em("Note: this data is for personal or educational use only. Other use or use in a publication is not permitted without the consent of the team. ", a("Email us with any inquiries.", href = "mailto:pollinators@wisc.edu"))),
      br(),
      fixedRow(
        column(width = 8,
          checkboxGroupInput(
        "dtGroups",
        label = "Select which variables to include in table:",
        choiceNames = c("Survey ID", "Date", "Location", "Habitat", "Crop", "Management"),
        choiceValues = c("id", "date", "grid_pt", "habitat", "crop", "management"),
        selected = c("habitat", "crop", "management"),
        inline = T)
          ),
        column(width = 4, align = "right",
          downloadButton("download_data", "Download data"))
      ),
      DTOutput("summaryTable")),
    
    # user stats
    tabPanel("User statistics",
      br(),
      plotlyOutput("plotUserStats", height = "600px"))
  ),
  
  

## Credits -----------------------------------------------------------------
  
  br(),
  hr(),
  br(),
  div(
    align = "center",
    style = "font-size:small; color:grey; border-top:2px darkgrey",    
    p(strong("©2021 University of Wisconsin Board of Regents"), style = "font-size:small; color:grey"),
    p(
      a("More information", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"),
      " - ", a("Email us", href = "mailto:pollinators@wisc.edu"),
      " - ", a("Sign up for our newsletter", href = "http://eepurl.com/gMqRdr", target = "_blank"),
      " - ", a("Send feedback", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank")
    ),
    br(),
    p("dashboard developed by", a("tanuki.tech", href = "https://github.com/bzbradford", target = "_blank"), style = "font-size:small; color:grey"),
    p("WiBee app developed by", a("caracal.tech", href = "https://caracal.tech/", target = "_blank"), style = "font-size:small; color:grey"),
    br(),
    p(em(paste("Data last updated:", as.character(refresh_time, format="%Y-%m-%d %H:%M:%S %Z"))))
  )
  
)
