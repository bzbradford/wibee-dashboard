## WIBEE DASHBOARD ##
# Ben Bradford, UW-Madison
# Requires data prep in separate RProj


shiny::shinyApp(ui, server)


#- Dependencies -#

#' app.R
#' - global.R
#' - ui.R
#' - server.R
#' - modules in ./R


#- Renv for pkg management -#

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::update()       # update project libraries
# renv::snapshot()     # save updated lock file to project
