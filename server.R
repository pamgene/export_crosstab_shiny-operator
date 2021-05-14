library(shiny)
library(tercen)
library(dplyr)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
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
        column(2, checkboxInput("collapseCols", "Collapse columns", TRUE)))
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
      "export.csv"
    },
    content = function(con) {
      write.table(dataInput(), con, sep="\t", col.names = TRUE, row.names = FALSE)
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