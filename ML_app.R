library(shiny) #the R shiny library

source('ui.R')
source('server.R')


shinyApp(
  ui = ML_ui,
  server = ML_server 
)