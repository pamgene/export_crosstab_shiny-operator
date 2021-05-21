library(shiny)
library(tercen)
library(dplyr)
library(writexl)

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
        column(2, shiny::downloadButton("downloadData", "Export Crosstab file")),
        column(2, checkboxInput("collapseCols", "Collapse columns", TRUE)),
        column(2, radioButtons("format", "File format", choices = c("tsv", "xlsx"), selected = "tsv")))
    )
  })
  
  output$summary <- renderText({
    ctx       <- getCtx(session)
    raw_data  <- rawData()
    col_names <- ctx$cnames %>% unlist()
    row_names <- ctx$rnames %>% unlist()
    paste(paste("Number of rows:", nrow(raw_data)),
          paste("Number of cols:", ncol(raw_data)),
          paste("Row names:",  paste(row_names, collapse = ",")),
          paste("Column names:",  paste(col_names, collapse = ",")), sep="\n")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("export.", input$format)
    },
    content = function(con) {
      if (input$format == "tsv") {
        write.table(dataInput(), con, sep="\t", col.names = TRUE, row.names = FALSE)
      } else {
        write_xlsx(dataInput(), con)
      }
    }
  )
})

getRawData <- function(session) {
  getCtx(session)$as.matrix()  
}

getData <- function(session, raw_data) {
  ctx        <- getCtx(session)
  row_names  <- names(ctx$rnames)
  col_values <- ctx$cselect()
  row_values <- ctx$rselect() %>% pull()
  
  new_col_names <- col_values %>% 
    mutate(newcol = apply(col_values[, colnames(col_values)], 1, paste, collapse = "_")) %>% 
    pull(newcol)
  result        <- data.frame(raw_data) %>% 
    mutate(!!row_names := row_values) %>% 
    select(!!row_names, everything())
  colnames(result) <- c(row_names, new_col_names)
  result
}

runApp(shinyApp(ui, server))  