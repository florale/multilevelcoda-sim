source("functions.R")
source("multilevelcoda-sim-shiny/shiny_in.R")

source("multilevelcoda-sim-shiny/server.R")
source("multilevelcoda-sim-shiny/ui.R")

shinyApp(ui = ui, server = server)
