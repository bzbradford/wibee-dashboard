# WiBee Shiny App
# developed by Ben Bradford, UW-Madison


library(shiny)

source("global.r")
source("ui.r")
source("server.r")

shinyApp(ui, server)
