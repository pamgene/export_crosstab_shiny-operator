library(shiny)

shinyUI(fluidPage(
  useShinyjs(),
  uiOutput("reacOut"),
  title = "Export View Data"
))