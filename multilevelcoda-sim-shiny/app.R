library(shiny)
library(shinythemes)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: #708885;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

