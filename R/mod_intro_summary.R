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
  
  sidebar <- {
    hb_totals <- filter(bee_totals, bee_name == "Honey bees")
    wb_totals <- filter(bee_totals, bee_name == "Wild bees")
    nb_totals <- filter(bee_totals, bee_name == "Non-bees")
    
    sidebarPanel(
      p(strong("Project summary")),
      p("Unique users: ", format(length(unique(surveys$user_id)), big.mark = ",")),
      p("Total completed surveys: ", format(nrow(surveys), big.mark = ",")),
      p("Most recent survey: ", max(surveys$date)),
      p("Total insect observations: ", format(sum(surveys_long$count), big.mark = ",")),
      tags$ul(
        tags$li(
          paste0("Honey bees: ", format(hb_totals$tot_count, big.mark = ","), " (", hb_totals$pct_count, ")")
        ),
        tags$li(
          paste0("Wild bees: ", format(wb_totals$tot_count, big.mark = ","), " (", wb_totals$pct_count, ")")
        ),
        tags$li(
          paste0("Non-bees: ", format(nb_totals$tot_count, big.mark = ","), " (", nb_totals$pct_count, ")")
        )
      )
    )
  }
  
  sidebarLayout(sidebar, main_panel, position = "right")
}
