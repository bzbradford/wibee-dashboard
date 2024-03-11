## INTRO & PROJECT SUMMARY ##

#' Requires global vars:
#' - surveys
#' - surveys_long
#' - bee_totals

introUI <- function() {
  main_panel <- mainPanel(
    h3("What is the WiBee app?", style = "margin-top:0px"),
    p("WiBee (pronounced We-bee) is a new smartphone app developed by the", a("Gratton Lab", href = "https://gratton.entomology.wisc.edu/", target = "_blank"), "at the University of Wisconsin-Madison. We invite growers and interested citizen scientists to use the app during the growing season to collect high quality data on wild bee abundance and diversity on Wisconsin’s fruit and vegetable farms. The app can also be used in your home garden, or at prairies, parks, woodlands, or anywhere else you see pollinator activity, whether or not you live in Wisconsin. All are welcome to contribute."),
    p("WiBee is a citizen science project where participants use the ", a("WiBee app", href = "http://www.pollinators.wisc.edu/wibee"), " to conduct 5-minute pollinator surveys of a 1 meter square section of flowering plants. Each time a pollinator (such as a honey bee, bumble bee, solitary wild bee, or other non-bee insect) lands on a flower, that's a visit! All the data here is collected by people like you going out and completing surveys with the WiBee App. We invite you to explore the data to see what wild bee populations and their flower visit rates look like across Wisconsin. You can also compare your own data in the WiBee app to the summary data presented here in this dashboard to help you make decisions about managing your local pollinator community or track any change over time. As you explore the data below, remember that this dashboard is a work in progress. If you have specific suggestions, please contact us!"),
    p("To join the project and help collect data, download the WiBee app today or visit", a("pollinators.wisc.edu/wibee", href = "http://www.pollinators.wisc.edu/wibee", target = "_blank"), "to learn more. Questions?", a("Email us.", href = "mailto:pollinators@wisc.edu"), "Comments?", a("Send feedback.", href = "https://forms.gle/6qy9qJLwCxSTTPNT8", target = "_blank"), "Want to stay in the loop?", a("Sign up for our newsletter.", href = "http://eepurl.com/gMqRdr", target = "_blank"), "Thank you for participating!")
  )
  
  sidebar <- sidebarPanel(
    p(strong("Project summary")),
    p("Contributors: ", format(length(unique(surveys$user_id)), big.mark = ",")),
    p("Total completed surveys: ", format(nrow(surveys), big.mark = ",")),
    p("Most recent survey: ", format(max(surveys$date), "%b %d, %Y")),
    p("Total insect observations: ", format(sum(surveys$total_visits), big.mark = ",")),
    tags$ul(
      tags$li(filter(bee_totals, bee_name == "Honey bees")$label),
      tags$li(filter(bee_totals, bee_name == "Wild bees")$label),
      tags$li(filter(bee_totals, bee_name == "Non-bees")$label)
    )
  )
  
  how_to_use <- bsCollapse(
    bsCollapsePanel(
      title = "Click here for help on how to use this dashboard",
      p("This dashboard has two main sections: ", strong("selecting surveys"), " and ", strong("viewing survey data.")),
      p(
        strong("Step 1: Select geographic zones on the map."),
        "If you want to look only at surveys taken in a specific area, select those areas on the map tab."
      ),
      p(
        strong("Step 2: Select date range."),
        "If you want to look at surveys from a specific year, or over a specific date range, specify those in the date tab."
      ),
      p(
        strong("Step 3: Select survey characteristics."),
        "Use the checkboxes to narrow down what kind of habitat, crop, flower or management type you want to look at. Numbers in parentheses show the number of surveys that match each characteristic. You can also group wild bees together, or select specific kinds of bees to show in the plots."
      ),
      tags$ul(
        tags$li(
          strong("Choose a habitat type(s)."),
          "If you run an orchard and you just want to look at the collective data from other orchards, filter the data by checking the 'orchard' box."
        ),
        tags$li(
          strong("Choose a crop or flower type(s)."),
          "If you want to compare your apple bloom wild bee visit rate to other apple orchards, check the apple box to filter the data. Keep in mind that crops bloom at different times of year and have different inflorescences, so the bee visit rate and bee group composition will likely be different between crops."
        ),
        tags$li(
          strong("Choose a management type(s)."),
          "These categories are subjective (chosen by the survey taker) and very broad, so take any variation between management types with a grain of salt."
        ),
        tags$li(
          strong("Select or group bee types."), " This is optional, but you can group wild bees together (bumble bees, dark bees, and green bees), or select specific bee groups to show on the plots."
        ),
      ),
      p(
        strong("Step 4: Making sense of the data."),
        "Look at the average flower visits per minute and the composition of the bee visitors."),
      tags$ul(
        tags$li(
          "How do your overall bee visits per minute and wild bee visits per minute compare to the overall average?"
        ),
        tags$li(
          "What does your flower visit composition look like compared to the overall average? Do you have a lower, similar or higher percentage of wild bees compared to the overall average? Among your wild bees, how does the composition of bumble bees, large dark bees, small dark bees and green bees compare to your data?"
        ),
        tags$li(
          "You can make comparisons within this dashboard between select surveys and the overall averages, or you can look at your WiBee app survey data and compare individual surveys to the data you see here in the dashboard."
        )
      )
    )
  )
  
  tagList(
    sidebarLayout(sidebar, main_panel, position = "right"),
    br(),
    how_to_use
  )
}
