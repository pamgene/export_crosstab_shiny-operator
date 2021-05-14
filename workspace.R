library(shiny)
library(tercen)
library(dplyr)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  # Set appropriate options
  #options("tercen.serviceUri"="http://tercen:5400/api/v1/")
  #options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
  #options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
  #options("tercen.username"= "admin")
  #options("tercen.password"= "admin")
  ctx <- tercenCtx()
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  uiOutput("reacOut"),
  title = "Export Crosstab"
))

server <- shinyServer(function(input, output, session) {
  
  rawData <- reactive({
    getRawData(session)
  })
  
  dataInput <- reactive({
    getData(session, rawData())
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Export Crosstab</center></h3>"),
      fluidRow(
        column(1),
        column(5, verbatimTextOutput("summary")),
        column(2, shiny::downloadButton("downloadData", "Export Crosstab file")))
    )
  })
  
  output$summary <- renderText({
    ctx       <- getCtx(session)
    raw_data  <- rawData()
    col_names <- ctx$cnames %>% unlist()
    paste(paste("Number of rows:", nrow(raw_data)),
          paste("Number of cols:", ncol(raw_data)),
          paste("Column names:",  paste(col_names, collapse = ",")), sep="\n")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "export.csv"
    },
    content = function(con) {
      write.csv(dataInput(), con)
    }
  )
})

getRawData <- function(session) {
  getCtx(session)$as.matrix()  
}

getData <- function(session, raw_data){
  ctx           <- getCtx(session)
  channels      <- ctx$rselect() %>% pull()
  col_names     <- ctx$cnames %>% unlist()
  # some columns might be of character type, but the input for flowFrame should be a numeric matrix
  columns       <- ctx$cselect() %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.numeric) %>% 
    replace(is.na(.), 0)
  res           <- as.matrix(cbind(t(raw_data), columns)) 
  colnames(res) <- c(channels, col_names)
  
  return(res)
}

runApp(shinyApp(ui, server))  