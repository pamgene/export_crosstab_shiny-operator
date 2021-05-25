library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  uiOutput("reacOut"),
  title = "Export View Data"
))