library("shiny")
library("plotly")

ui <- source("ShinyApps/ui.R")$value
server <- source("ShinyApps/server.R")$value

shinyApp(ui, server)
