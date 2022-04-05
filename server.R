library(shiny)
library(tercen)
library(dplyr)
library(writexl)
library(tidyr)
library(shinyjs)
library(data.table)

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

server <- shinyServer(function(input, output, session) {
  
  rawData <- reactive({
    getRawData(session)
  })
  
  dataInput <- reactive({
    getData(session, rawData(), input$collapseCols, input$collapseRows)
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Export View Data</center></h3>"),
      fluidRow(
        column(1),
        column(5, verbatimTextOutput("summary")),
        column(2, shiny::downloadButton("downloadData", "Export data")),
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
  
  # write headers + data
  write_list_data <- function(data, con) {
    cat(data[[1]], file = con)
    fwrite(x = data[[2]], file = con, sep = "\t", col.names = TRUE, append = TRUE)
  }
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("export.", input$format)
    },
    content = function(con) {
      data <- dataInput()
      if (input$format == "tsv") {
        if (class(data) == "list") {
          write_list_data(data, con)
        } else if (class(data) == "data.frame") {
          write.table(data, con, sep="\t", col.names = TRUE, row.names = FALSE)
        }
      } else {
        if (class(data) == "list") {
          # write to temp csv file and convert to xlsx
          tmp_csv_file = tempfile()
          write_list_data(data, tmp_csv_file)
          csv_data <- read.csv(tmp_csv_file, sep = "\t", check.names = FALSE)
          write_xlsx(csv_data, con)
          on.exit(unlink(tmp_csv_file))
        } else if (class(data) == "data.frame") {
          write_xlsx(data, con)
        }
      }
    }
  )
})

getRawData <- function(session) {
  result <- NULL
  tryCatch({
    ctx    <- getCtx(session)
    result <- ctx$as.matrix()
  }, error = function(e) {
    stop("Data could not be loaded: is your view setup correctly")
  })
  result
}

make.wide <- function(df) {
  df %>%
    mutate(col = as.factor(paste(.ci, .axisIndex, sep = "_"))) %>% 
    select(col, .y) %>% 
    pivot_wider(names_from = col, values_from = .y)
}

get_crosstab_view_header_row <- function(yaxis_names, headers, empty_cols, layer_count, i) {
  result <- ""
  header <- headers[i,]
  if (header != "") {
    result <- paste(header, collapse = "\t")
    if (length(yaxis_names) > 1) {
      result <- paste(unlist(lapply(headers[i,], FUN = function(x) rep(x, layer_count))), collapse = "\t")
    }
    result <- paste(c(empty_cols, rownames(headers)[i], result), collapse = "\t")
  }
  result
}

get_crosstab_view_quantitation_row <- function(yaxis_names, headers, empty_cols, yaxis_line) {
  yaxis_line <- rep(yaxis_names, length(headers[1,]))
  result     <- paste(c(empty_cols, "Y-axis", yaxis_line), collapse = "\t")
  result
}

getData <- function(session, raw_data, collapse_cols, collapse_rows) {
  ctx         <- getCtx(session)
  row_names   <- names(ctx$rnames)
  col_values  <- ctx$cselect()
  row_values  <- ctx$rselect()
  yaxis_names <- rev(unlist(ctx$yAxis))
  layer_count <- length(yaxis_names)
  
  # in case of multiple layers: create a structure like as.matrix() but with additional columns
  if (layer_count > 1) {
    raw_data <- ctx$select() %>% 
      select(".ri", ".ci", ".axisIndex", ".y") %>% 
      group_by(.ri) %>% 
      do(make.wide(.)) %>% 
      ungroup() %>% 
      select(-.ri)
  }
  
  # collapsed view  
  if (collapse_cols) {
    new_col_names <- col_values
    if (ncol(col_values) == 1) {
      new_col_names <- new_col_names %>% pull()
      new_col_names <- unlist(lapply(new_col_names, FUN = function(x) paste(yaxis_names, x, sep = ".")))
    } else {
      col_values <- do.call(rbind, (lapply(seq_along(yaxis_names), FUN = function(i, y = yaxis_names) col_values %>% 
                                             mutate(Type = y[i]) %>%
                                             mutate(rowseq = seq(i, i + (nrow(col_values) - 1) * length(yaxis_names), length(yaxis_names))) %>%
                                             select(Type, everything())))) %>%
        arrange(rowseq) %>%
        select(-rowseq)
      new_col_names <- col_values %>% 
        mutate(newcol = apply(col_values[, colnames(col_values)], 1, paste, collapse = "_")) %>% 
        pull(newcol)
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
  }
  # crosstab view
  else {
    new_col_names     <- rep("", nrow(col_values) * layer_count)
    if (collapse_rows) {
      new_row_names  <- paste(row_names, collapse = "_")
      new_row_values <- row_values %>% 
        mutate(!!new_row_names := apply(row_values[, colnames(row_values)], 1, paste, collapse = "_")) %>% 
        pull(!!new_row_names)
      df <- data.frame(raw_data) %>% 
        mutate(!!new_row_names := new_row_values) %>% 
        select(!!new_row_names, everything())
      colnames(df)      <- c(new_row_names, new_col_names)  
    } else {
      df                <- cbind(row_values, data.frame(raw_data))
      new_row_names     <- row_names
    } 
    colnames(df)      <- c(new_row_names, new_col_names)
    headers           <- t(col_values)
    colnames(headers) <- rep("", ncol(headers))
    header_length     <- nrow(headers) + length(yaxis_names) - 1
    empty_cols        <- rep(" ", length(new_row_names) - 1)
    
    if (header_length == 1) {
      row1         <- get_crosstab_view_header_row(yaxis_names, headers, empty_cols, layer_count, 1)
      row2         <- get_crosstab_view_quantitation_row(yaxis_names, headers, empty_cols, yaxis_line)
      if (row1 == "") {
        header_lines <- paste(row2, "\n")
      } else {
        header_lines <- paste(paste(row1, row2, sep = "\n"), "\n")
      }
    }
    else if (header_length > 1) {
      header_lines      <- paste(do.call(what = paste, args = c(lapply(seq(header_length), FUN = function(i) {
        if (i <= nrow(headers)) {
          result <- get_crosstab_view_header_row(yaxis_names, headers, empty_cols, layer_count, i)
        } else {
          result <- get_crosstab_view_quantitation_row(yaxis_names, headers, empty_cols, yaxis_line)
        }
        result
      }), sep = "\n")
      ), "\n")
    }
    result <- list(header_lines, df)
  }
  result
}
