# WiBee Shiny App
# developed by Ben Bradford, UW-Madison
# currently built under R 3.6.3 for compatibility with data-viz.it.wisc.edu

library(shiny)

source("backend.r")
source("ui.r")
source("server.r")

shinyApp(ui, server)
