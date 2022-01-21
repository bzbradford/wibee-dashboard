#---- UI ----#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(leaflet)
library(DT)
library(plotly)



# Define UI ----

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
  
  

  # Introduction and summary ----

  sidebarLayout(
    mainPanel = mainPanel(
      h3("What is the WiBee app?", style = "margin-top:0px"),
      p("WiBee (pronounced We-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
      p("WiBee is a citizen science project where participants use the ", a("WiBee app", href = "http://www.pollinators.wisc.edu/wibee"), " to conduct 5-minute pollinator surveys of a 1 meter square section of flowering plants. Each time a pollinator (such as a honey bee, bumble bee, solitary wild bee, or other non-bee insect) lands on a flower, that's a visit! All the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
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
  
  bsCollapse(
    open = "map",
    
    ## Map ----
    bsCollapsePanel(
      style = "primary",
      value = "map",
      title = "1) Filter by survey location",
      p(em("Click on individual grid cell(s) to show only results from those areas. Note: some surveys are from outside Wisconsin. Click 'Select all' to see them."), style = "margin-top:.5em; margin-bottom:.5em"),
      leafletOutput("map", height = 600),
      div(style = "margin-top: 5px;",
        div(actionButton("map_select_visible", "Select visible"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_zoom_all", "Select all"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_clear_selection", "Clear selection"), style = "padding-right:10px; display:inline-block"),
        div(actionButton("map_reset", "Reset map"), style = "padding-right:20px; display:inline-block"),
        div(strong(textOutput("survey_count_loc")), style = "display:inline-block")
      )
    ),
    
    ## User IDs ----
    bsCollapsePanel(
      style = "primary",
      title = "2) Show surveys from specific user(s)",
      p(em("Filter survey data by selecting which User IDs you want to show data from. You can find your User ID in the WiBee app under Profile. Add one at a time, or separated by commas. The selected users list will show the total number of surveys submitted by that User ID."), style = "margin-bottom:.5em"),
      div(
        class = "well",
        fluidRow(
          column(
            6,
            textInput(
              inputId = "user_id",
              label = "User ID:",
              value = ""
            ),
            div(
              actionButton("add_user_id", "Add ID to list"),
              actionButton("reset_user_ids", "Reset list of IDs"),
              style = "margin-top:15px"
            )
          ),
          column(
            6,
            p(strong("Selected users:")),
            uiOutput("selected_users_display")
          )
        ),
      ),
      div(style = "text-align: center; font-weight: bold;", textOutput("survey_count_users"))
    ),
    
    ## Date range ----
    bsCollapsePanel(
      style = "primary",
      title = "3) Select survey date range",
      p(em("Filter survey data by selecting which date range you want to see data for."), style = "margin-bottom:.5em"),
      div(
        class = "well",
        checkboxGroupButtons(
          "years",
          label = "Surveys from year:",
          choices = years,
          selected = years,
          individual = TRUE,
          checkIcon = list(yes = icon("check-square"), no = icon("square-o"))
        ),
        sliderInput(
          "date_range",
          label = "Date range:",
          min = min_date,
          max = max_date,
          value = c(min_date, max_date),
          width = "100%"),
        div(actionButton("reset_date", "Reset date"), style = "margin-top:15px"),
      ),
      div(style = "text-align: center; font-weight: bold;", textOutput("survey_count_date"))
    ),
  
    ## Habitat/management/pollinator ----
    bsCollapsePanel(
      style = "primary",
      title = "4) Select surveys by habitat, management type, or pollinator group",
      p(em("Filter survey data by selecting which habitats, management types, or pollinator groups you want to see data for. Number of matching surveys for each habitat or reported management practice is shown in parentheses."), style = "margin-bottom:.5em"),
      div(class = "well",
        fixedRow(
          column(4,
            checkboxGroupInput(
              "which_habitat",
              "Habitat:",
              choiceNames = levels(habitats$label),
              choiceValues = habitats$type,
              selected = habitats$type
              ),
            div(actionButton("which_habitat_all", "All"), style = "display:inline-block"),
            div(actionButton("which_habitat_none", "None"), style = "display:inline-block")
            ),
          column(4,
            checkboxGroupInput(
              "which_mgmt",
              "Management:",
              choiceNames = levels(managements$label),
              choiceValues = managements$type,
              selected = managements$type
              ),
            div(actionButton("which_mgmt_all", "All"), style = "display:inline-block"),
            div(actionButton("which_mgmt_none", "None"), style = "display:inline-block")
            ),
          column(4,
            checkboxGroupInput(
              "which_bees",
              "Bee group:",
              choiceNames = bee_names,
              choiceValues = bee_names,
              selected = bee_names
            ),
            materialSwitch("group_wild", label = "Group wild bees together", status = "success"),
            div(actionButton("which_bees_all", "All"), style = "display:inline-block"),
            div(actionButton("which_bees_none", "None"), style = "display:inline-block")
          ),
        )
      ),
      div(style = "text-align: center; font-weight: bold;", textOutput("survey_count_site"))
    ),
  
  ## Plant selections ----
    bsCollapsePanel(
      style = "primary",
      title = "5) Select crop(s) or flowering plant(s) observed during surveys",
      p(em("Filter survey data by selecting which crops, focal plants (featured plants for surveys shown in the app), or other non-crop flowering plants you want to see data for. Number of matching surveys for each plant is shown in parentheses."), style = "margin-bottom:.5em"),
      div(class = "well",
        fixedRow(
          column(3,
            checkboxGroupInput(
              "which_crops",
              "Crops:",
              choiceNames = levels(select_crops$label),
              choiceValues = select_crops$type,
              selected = select_crops$type
              ),
            div(actionButton("which_crops_all", "All"), style = "display:inline-block"),
            div(actionButton("which_crops_none", "None"), style = "display:inline-block")
            ),
          column(4,
            checkboxGroupInput(
              "which_focal_noncrops",
              "Focal non-crop plants:",
              choiceNames = lapply(as.list(levels(focal_noncrops$label)), HTML),
              choiceValues = focal_noncrops$type,
              selected = focal_noncrops$type),
            div(actionButton("which_focal_noncrops_all", "All"), style = "display:inline-block"),
            div(actionButton("which_focal_noncrops_none", "None"), style = "display:inline-block")
          ),
          column(5,
            checkboxGroupInput(
              "which_noncrops",
              "Other non-crop plant:",
              choiceNames = lapply(as.list(levels(select_noncrops$label)), HTML),
              choiceValues = select_noncrops$type,
              selected = select_noncrops$type),
            div(actionButton("which_noncrops_all", "All"), style = "display:inline-block"),
            div(actionButton("which_noncrops_none", "None"), style = "display:inline-block")
            )
          ),
        fixedRow(align = "center", style = "margin-top: 1em;",
          div(actionButton("select_all_plants", "Select all plants"), style = "display:inline-block"),
          div(actionButton("select_no_plants", "Clear all plant selections"), style = "display:inline-block")
          )
        ),
      div(style = "text-align: center; font-weight: bold;", textOutput("survey_count_plant"))
      )
    ),
  
  
  
# Text: Number of selected surveys ----

  div(
    class = "well",
    style = "text-align: center; font-size: larger;",
    strong(textOutput('survey_count_final')),
    ),
  br(),
  br(),
  
  

# Tabs with plot and data table displays ----

  h4("View or download data from selected surveys:", style = "text-decoration: underline;"),
  
  tabsetPanel(
    
    ## Species pie charts ----
    tabPanel("Species composition",
      br(),
      plotlyOutput("map_chart_all", width = "45%", inline = T),
      plotlyOutput("map_chart_selected", width = "45%", inline = T),
      br()),
    
    ## Activity by date ----
    tabPanel("Activity by date",
      br(),
      plotlyOutput("plotByDate"),
      br(),
      p(em("This chart shows daily or season trends in pollinator activity by showing the average activity by pollinator group across all surveys conducted on a given day. The date range can be adjusted in the survey filters below the map."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small")),
    
    ## Surveys by date ----
    tabPanel("Surveys by date",
      br(),
      plotlyOutput("plotSurveysByDate"),
      br(),
      p(em("This chart shows the number of surveys completed on each day within the date range specified in the data filters above."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small")),
    
    ## Compare habitat ----
    tabPanel("Compare habitats",
      br(),
      plotlyOutput("plotByHabitat"),
      br(),
      p(em("This chart compares total pollinator visitation rates across different habitat types. The number of surveys represented by each habitat is shown in parentheses in the labels."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small")),
    
    ## Compare management ----
    tabPanel("Compare managements",
      br(),
      plotlyOutput("plotByMgmt"),
      br(),
      p(em("This chart compares total pollinator visitation rates by user-reported management practices. The number of surveys represented by each practice is shown in parentheses in the labels."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small")),
    
    ## Compare plants ----
    tabPanel("Compare crops/flowers",
      br(),
      plotlyOutput("plotByCrop", height = "600px"),
      br(),
      p(em("This interactive chart compares total pollinator visitation rates across all of the different crops and non-crop plants surveyed with the app. The number of surveys represented by each plant species or group is shown in parentheses in the labels."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small;")),
    
    ## Map of filtered surveys ----
    tabPanel("View selected surveys on a map",
      br(),
      uiOutput("data_map_ui"),
      leafletOutput("data_map", height = "600px"),
      br(),
      p(em("This map displays a summary of all the surveys currently selected by your filters. Change your filter selections or choose a different summary type to change what is shown on this map."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small;")),
    
    ## Tabular survey data ----
    tabPanel("View as data table",
      br(),
      p("The table below shows the average visitation rate per minute for the surveys and insect categories selected by the filters above. Check or uncheck the grouping variables to simplify or expand the summary table. Click the download button to save a copy of the data you have selected.", em("Note: this data is for personal or educational use only. Other use or use in a publication is not permitted without the consent of the team. ", a("Email us with any inquiries.", href = "mailto:pollinators@wisc.edu"))),
      br(),
      fixedRow(
        column(8,
          checkboxGroupInput(
            "dtGroups",
            label = "Select which variables to include in table:",
            choiceNames = table_vars$names,
            choiceValues = table_vars$values,
            selected = table_vars_selected,
            inline = T)),
        column(4, align = "right",
          downloadButton("download_data", "Download data"))
      ),
      DTOutput("summaryTable")
    ),
    
    ## User stats ----
    tabPanel("User statistics",
      br(),
      plotlyOutput("plotUserStats", height = "600px"),
      p(em("This graph shows the total number of surveys conducted each year, further grouped by the number of users who have conducted the same amount of surveys."), align = "center", style = "margin-top:.5em; margin-bottom:.5em; font-size:small;")
    )
  ),
  
  br(),
  div(actionButton("reset", "Reset all filters"), style = "margin-top:15px", align = "center"),
  
  

# Credits ----
  
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
    p(em(status), br(), em(paste("Data last updated:", as.character(refresh_time, format = "%Y-%m-%d %H:%M:%S %Z"))))
  )
  
)
