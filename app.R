library(shiny)
library(shinyjs)
library(RMySQL)
library(dplyr)
library(DT)
library(rjson)
library(stringr)
library(readr)
source("file_check_functions.R")

open_file_func_pbp <- function(fname, filter_rows) {
  errors <- data.frame(
    file_name = character(),
    error = character(),
    stringsAsFactors = FALSE
  )

  ########## OPEN FILE ###########
  fEnc <- as.character(guess_encoding(fname)[1,1])
  df <- data.frame()
  tryCatch(
    df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                 check.names = FALSE, fileEncoding = fEnc), 
    error = function(e) {
      print("nope")
    }
  )

  tryCatch(
    if (length(colnames(df)) < 5) { 
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, sep = "\t",
                     check.names = FALSE, fileEncoding = fEnc)
    }, 
    error = function(e) {
      print("nope2")
    }
  )

  tryCatch(
    if (nrow(df) < 5) {
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                     check.names = FALSE, fileEncoding = "UTF-8-BOM")
      print("tried")
    },
    error = function(e) {
      print("nope3")
    }
  )
  
  tryCatch(
    if (nrow(df) < 5) {
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                     check.names = FALSE, fileEncoding = "WINDOWS-1252")
      print("tried2")
    },
    error = function(e) {
      print("nope4")
    }
  )
  
  ######## REMOVE EMPTY COLUMNS ##########
  df <- df[,colnames(df) != ""]
  
  ####### CONVERT COLUMNS IF READ AS LOGICAL #########
  df[,sapply(df,class) == "logical"] <-
    sapply(df[,sapply(df,class) == "logical"],
           function(i) substr(as.character(i),1,1))
  
  ######## REMOVE PRE-MATCH ROW(S) ##########
  df <- df[!grepl("pre-match", tolower(df$Name)),]
  
  ######### STRIP 'WORK AREA' FROM COLUMN NAMES #############
  for (i in 1:length(colnames((df)))) {
    if (grepl("Work Area", colnames(df)[i]) == TRUE) {
      names(df)[i] <- substr(colnames(df)[i], gregexpr(":", colnames(df)[i])[[1]] + 2, nchar(colnames(df)[i]))
    }
  }
  
  ####### RENAME ALIASES ###########
  if (is.null(df[1,]$`Rally Length (Actual)`) == FALSE) {
    names(df)[names(df) == "Rally Length (Actual)"] <- "Shot Count"
  }
  if (is.null(df[1,]$`Ball 3 Intention`) == FALSE) {
    names(df)[names(df) == "Ball 3 Intention"] <- "Ball 3 Situation"
  }
  #if (is.null(df[1,]$`Ball 4 Intention`) == FALSE) {
  #  names(df)[names(df) == "Ball 4 Intention"] <- "Ball 4 Situation"
  #}
  if (is.null(df[1,]$`Opp to Approach`) == FALSE) {
    names(df)[names(df) == "Opp to Approach"] <- "Opportunity to Approach"
  }
  if (is.null(df[1,]$`Neutral Backhand`) == FALSE) {
    names(df)[names(df) == "Neutral Backhand"] <- "Backhand"
  }
  if (is.null(df[1,]$`Offensive Forehand`) == FALSE) {
    names(df)[names(df) == "Offensive Forehand"] <- "Offensive FH"
  }
  if (is.null(df[1,]$`Defensive`) == FALSE) {
    names(df)[names(df) == "Defensive"] <- "Pushed Wide"
  }
  
  ####### RENAME 2018 APPROACHES ##########
  if (is.null(df[1,]$`Approach`) == FALSE & 
      is.null(df[i,]$`Player Net Approach`) == FALSE) {
    df$`Player Net Approach` <- ifelse(df$Approach != "", 
                                       paste(df$`Tagged Player`, gsub("Approach", 
                                                                      "Net Approach", df$Approach), sep = " "), 
                                       df$Approach)
  }
  if (is.null(df[1,]$`Opp Approach`) == FALSE & 
      is.null(df[i,]$`Opponent Net Approach`) == FALSE) {
    df$`Opponent Net Approach` <- ifelse(df$`Opp Approach` != "", 
                                         paste(df$`Opponent Player`, 
                                               gsub("Opp ", "", gsub("Approach", "Net Approach", 
                                                                     df$`Opp Approach`)), sep = " "), df$`Opp Approach`)
  }
  
  ######## REMOVE DUPLICATE COLUMNS
  df <- df[, !duplicated(colnames(df))]
  
  errors <- ErrorCheck_PBP(df, errors, fname) %>% select(-file_name)
  
  print(filter_rows)
  if(is.null(filter_rows) == FALSE) {
    df <- df[filter_rows,]
  }
  
  return(list("errors" = errors, "data" = df))
}
open_file_func_pbp_wc <- function(fname, filter_rows) {
  errors <- data.frame(
    file_name = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
  
  ########## OPEN FILE ###########
  fEnc <- as.character(guess_encoding(fname)[1,1])
  df <- data.frame()
  tryCatch(
    df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                   check.names = FALSE, fileEncoding = fEnc), 
    error = function(e) {
      print("nope")
    }
  )
  
  tryCatch(
    if (length(colnames(df)) < 5) { 
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, sep = "\t",
                     check.names = FALSE, fileEncoding = fEnc)
    }, 
    error = function(e) {
      print("nope2")
    }
  )
  
  tryCatch(
    if (nrow(df) < 5) {
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                     check.names = FALSE, fileEncoding = "UTF-8-BOM")
      print("tried")
    },
    error = function(e) {
      print("nope3")
    }
  )
  
  tryCatch(
    if (nrow(df) < 5) {
      df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE, 
                     check.names = FALSE, fileEncoding = "WINDOWS-1252")
      print("tried2")
    },
    error = function(e) {
      print("nope4")
    }
  )
  
  ######## REMOVE EMPTY COLUMNS ##########
  df <- df[,colnames(df) != ""]
  
  ####### CONVERT COLUMNS IF READ AS LOGICAL #########
  df[,sapply(df,class) == "logical"] <-
    sapply(df[,sapply(df,class) == "logical"],
           function(i) substr(as.character(i),1,1))
  
  ######## REMOVE PRE-MATCH ROW(S) ##########
  df <- df[!grepl("pre-match", tolower(df$Name)),]
  
  ######### STRIP 'WORK AREA' FROM COLUMN NAMES #############
  for (i in 1:length(colnames((df)))) {
    if (grepl("Work Area", colnames(df)[i]) == TRUE) {
      names(df)[i] <- substr(colnames(df)[i], gregexpr(":", colnames(df)[i])[[1]] + 2, nchar(colnames(df)[i]))
    }
  }
  
  ####### RENAME ALIASES ###########
  if (is.null(df[1,]$`Rally Length (Actual)`) == FALSE) {
    names(df)[names(df) == "Rally Length (Actual)"] <- "Shot Count"
  }
  if (is.null(df[1,]$`Ball 3 Intention`) == FALSE) {
    names(df)[names(df) == "Ball 3 Intention"] <- "Ball 3 Situation"
  }
  #if (is.null(df[1,]$`Ball 4 Intention`) == FALSE) {
  #  names(df)[names(df) == "Ball 4 Intention"] <- "Ball 4 Situation"
  #}
  if (is.null(df[1,]$`Opp to Approach`) == FALSE) {
    names(df)[names(df) == "Opp to Approach"] <- "Opportunity to Approach"
  }
  if (is.null(df[1,]$`Neutral Backhand`) == FALSE) {
    names(df)[names(df) == "Neutral Backhand"] <- "Backhand"
  }
  if (is.null(df[1,]$`Offensive Forehand`) == FALSE) {
    names(df)[names(df) == "Offensive Forehand"] <- "Offensive FH"
  }
  if (is.null(df[1,]$`Defensive`) == FALSE) {
    names(df)[names(df) == "Defensive"] <- "Pushed Wide"
  }
  
  ####### RENAME 2018 APPROACHES ##########
  if (is.null(df[1,]$`Approach`) == FALSE & 
      is.null(df[i,]$`Player Net Approach`) == FALSE) {
    df$`Player Net Approach` <- ifelse(df$Approach != "", 
                                       paste(df$`Tagged Player`, gsub("Approach", 
                                                                      "Net Approach", df$Approach), sep = " "), 
                                       df$Approach)
  }
  if (is.null(df[1,]$`Opp Approach`) == FALSE & 
      is.null(df[i,]$`Opponent Net Approach`) == FALSE) {
    df$`Opponent Net Approach` <- ifelse(df$`Opp Approach` != "", 
                                         paste(df$`Opponent Player`, 
                                               gsub("Opp ", "", gsub("Approach", "Net Approach", 
                                                                     df$`Opp Approach`)), sep = " "), df$`Opp Approach`)
  }
  
  ######## REMOVE DUPLICATE COLUMNS
  df <- df[, !duplicated(colnames(df))]
  
  errors <- ErrorCheck_PBP_WC(df, errors, fname) %>% select(-file_name)
  
  print(filter_rows)
  if(is.null(filter_rows) == FALSE) {
    df <- df[filter_rows,]
  }
  
  return(list("errors" = errors, "data" = df))
}

####################### UI ####################
ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(align = "center", width = 12, div(h1("Error Checker"), 
                                             style = "color:white; background-color:#16316f; padding: 5px;
                                                                     font-family:impact; margin: 10px;")),
  navbarPage("File Type", 
             ######################## PBP UI ######################
             tabPanel("PBP", 
                      fileInput("file_input_pbp", "Open File"),
                      textOutput("num_errors_pbp"), 
                      tags$hr(),
                      dataTableOutput("error_list_pbp"),
                      tags$hr(),
                      tags$hr(),
                      div(DT::dataTableOutput("raw_file_pbp"), style = "font-size:60%")  
             ), 
             ######################### PBP WC UI #####################
             tabPanel("PBP WC", 
                      fileInput("file_input_pbp_wc", "Open File"),
                      textOutput("num_errors_pbp_wc"),
                      tags$hr(),
                      dataTableOutput("error_list_pbp_wc"),
                      tags$hr(),
                      tags$hr(),
                      div(DT::dataTableOutput("raw_file_pbp_wc"), style = "font-size:60%")  
                      
             )
             
  )
  
  
  
  
)

####################### SERVER ##################
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  
  ####################### FILE INPUT BUTTONS ######################
  onclick("file_input_pbp", {
    rv$filter_rows <- NULL
  })
  
  onclick("file_input_pbp_wc", {
    rv$filter_rows <- NULL
  })
  
  ####################### ERROR LISTS ######################
  output$error_list_pbp <- renderDataTable({
    
    inFile <- input$file_input_pbp
    
    if (is.null(inFile))
      return(NULL)
    
    errs <- open_file_func_pbp(inFile$datapath, rv$filter_rows)
    output$num_errors_pbp <- renderText({
      paste("Number of errors:", nrow(errs$errors))
    })
    return(errs$errors)
    
  }, server = FALSE, selection = "single", options = list(dom = 't'))
  
  output$error_list_pbp_wc <- renderDataTable({
    
    inFile <- input$file_input_pbp_wc
    
    if (is.null(inFile))
      return(NULL)
    
    errs <- open_file_func_pbp_wc(inFile$datapath, rv$filter_rows)
    output$num_errors_pbp_wc <- renderText({
      paste("Number of errors:", nrow(errs$errors))
    })
    return(errs$errors)
    
  }, server = FALSE, selection = "single", options = list(dom = 't'))
  
  ####################### RAW OUTPUTS ######################
  output$raw_file_pbp <- DT::renderDataTable({
    
    inFile <- input$file_input_pbp
    
    if (is.null(inFile))
      return(NULL)
    
    return((open_file_func_pbp(inFile$datapath, rv$filter_rows))$data)
    
  })
  
  output$raw_file_pbp_wc <- DT::renderDataTable({
    
    inFile <- input$file_input_pbp_wc
    
    if (is.null(inFile))
      return(NULL)
    
    return((open_file_func_pbp_wc(inFile$datapath, rv$filter_rows))$data)
    
  })
  
  ####################### ERROR LIST SELECT ######################
  observeEvent(input$error_list_pbp_rows_selected, {
    if (length(input$error_list_pbp_rows_selected) > 0) {
      inFile <- input$file_input_pbp
      
      if (is.null(inFile))
        return(NULL)
      
      errs <- (open_file_func_pbp(inFile$datapath, rv$filter_rows))$errors
      
      rv$filter_rows <- as.numeric(str_split(gsub(":", "", 
                                                  str_extract(errs[input$error_list_pbp_rows_selected,], 
                                                              ":([\\d,\\s]+)")), ", ")[[1]])
      print(rv$filter_rows)
    }
  })
  
  observeEvent(input$error_list_pbp_wc_rows_selected, {
    if (length(input$error_list_pbp_wc_rows_selected) > 0) {
      inFile <- input$file_input_pbp_wc
      
      if (is.null(inFile))
        return(NULL)
      
      errs <- (open_file_func_pbp_wc(inFile$datapath, rv$filter_rows))$errors
      
      rv$filter_rows <- as.numeric(str_split(gsub(":", "", 
                                                  str_extract(errs[input$error_list_pbp_wc_rows_selected,], 
                                                              ":([\\d,\\s]+)")), ", ")[[1]])
      print(rv$filter_rows)
    }
  })
}


shinyApp(ui = ui, server = server)