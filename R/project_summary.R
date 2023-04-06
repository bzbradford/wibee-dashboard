## PROJECT SUMMARY ##

#' Requires global vars:
#' - surveys
#' - surveys_long
#' - bee_totals

projectSummaryUI <- function() {
  ns <- NS("project-summary")
  
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
        paste0(
          "Honey bees: ",
          format(hb_totals$tot_count, big.mark = ","),
          " (", hb_totals$pct_count, ")"
        )
      ),
      tags$li(
        paste0(
          "Wild bees: ",
          format(wb_totals$tot_count, big.mark = ","),
          " (", wb_totals$pct_count, ")"
        )
      ),
      tags$li(
        paste0(
          "Non-bees: ",
          format(nb_totals$tot_count, big.mark = ","),
          " (", nb_totals$pct_count, ")"
        )
      )
    )
  )
}
