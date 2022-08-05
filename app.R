# app.R

source("global.r")
source("ui.r")
source("server.r")

shiny::shinyApp(ui, server)

# renv::snapshot()
