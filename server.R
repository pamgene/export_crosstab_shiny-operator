library(shiny)
library(tercen)
library(dplyr)
library(writexl)
library(tidyr)

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
    getData(session, rawData(), input$collapseCols, input$collapseRows)
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Export Crosstab</center></h3>"),
      fluidRow(
        column(1),
        column(5, verbatimTextOutput("summary")),
        column(2, shiny::downloadButton("downloadData", "Export Crosstab file")),
        column(1, checkboxInput("collapseCols", "Collapse columns", TRUE)),
        column(1, checkboxInput("collapseRows", "Collapse rows", TRUE)),
        column(1, radioButtons("format", "File format", choices = c("tsv", "xlsx"), selected = "tsv")))
    )
  })
  
  output$summary <- renderText({
    ctx         <- getCtx(session)
    raw_data    <- rawData()
    col_names   <- ctx$cnames %>% unlist()
    row_names   <- ctx$rnames %>% unlist()
    yaxis_names <- unlist(ctx$yAxis)
    
    paste(paste("Number of rows:", nrow(raw_data)),
          paste("Number of cols:", ncol(raw_data)),
          paste("Row names:",  paste(row_names, collapse = ",")),
          paste("Column names:",  paste(col_names, collapse = ",")),
          paste("Values:",  paste(yaxis_names, collapse = ",")), sep="\n")
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

make.wide <- function(df) {
  df %>% 
    arrange(.axisIndex) %>% 
    mutate(col = as.factor(paste(.ci, .axisIndex, sep = "_"))) %>% 
    select(col, .y) %>% 
    pivot_wider(names_from = col, values_from = .y)
}

getData <- function(session, raw_data, collapse_cols, collapse_rows) {
  ctx         <- getCtx(session)
  row_names   <- names(ctx$rnames)
  col_values  <- ctx$cselect()
  row_values  <- ctx$rselect()
  yaxis_names <- unlist(ctx$yAxis)
  
  if (length(yaxis_names) > 1) {
    raw_data <- ctx$select() %>% 
      select(".ri", ".ci", ".axisIndex", ".y") %>% 
      group_by(.ri) %>% 
      do(make.wide(.)) %>% 
      ungroup() %>% 
      select(-.ri)
  }
  
  new_col_names <- col_values
  if (ncol(col_values) == 1) {
    new_col_names <- new_col_names %>% pull()
  } else {
    if (collapse_cols) {
      col_values <- do.call(rbind, (lapply(yaxis_names, FUN = function(x) col_values %>% 
                                             mutate(Type = x) %>%
                                             select(Type, everything()))))
      new_col_names <- col_values %>% 
        mutate(newcol = apply(col_values[, colnames(col_values)], 1, paste, collapse = "_")) %>% 
        pull(newcol)
    } else {
      new_col_names <- new_col_names %>% pull()
    }
  }
  
  new_row_names <- row_names
  if (ncol(row_values) == 1) {
    result <- cbind(row_values, data.frame(raw_data))
  } else {
    if (collapse_rows) {
      new_row_names  <- paste(row_names, collapse = "_")
      new_row_values <- row_values %>% 
        mutate(!!new_row_names := apply(row_values[, colnames(row_values)], 1, paste, collapse = "_")) %>% 
        pull(!!new_row_names)
      result <- data.frame(raw_data) %>% 
        mutate(!!new_row_names := new_row_values) %>% 
        select(!!new_row_names, everything())
    } else {
      result <- cbind(row_values, data.frame(raw_data))
    }
  }
  colnames(result) <- c(new_row_names, new_col_names)
  result
}