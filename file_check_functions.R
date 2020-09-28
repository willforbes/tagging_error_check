library(dplyr)

AddError <- function(eFrame, err, fname) {
  errRw <- nrow(eFrame) + 1
  eFrame[errRw,]$file_name <- fname
  eFrame[errRw,]$error <- err
  return(eFrame)
}

no.value <- function(val) {
  return (is.na(val) | val == "" | val == "-")
}

ErrorCheck_PBP <- function(df, errs, fnm) {
  ################# MISSING COLUMNS #########################
  coreShotColumns <- c("Name", "Position", "Duration", "Set", "Game Score", "Scoreline", "Side", 
                       "Player Serving", "Player Returning", "Serve", "Serve 1 Direction", "Serve 2 Direction", 
                       "Return Stroke", "Return 1 Direction", "Return 2 Direction", "Ball 3 Stroke", 
                       "Ball 3 Situation", "Player Net Approach", "Opponent Net Approach", "Situation", 
                       "Shot Count", "Rally Length", "Rally Stroke", "Stroke Type", "Player Final Shot", 
                       "Finish", "Point Won By", "Point Lost By", "Tagged Player", "Opponent Player", 
                       "Tournament", "Tournament Level", "Round", "Court Surface", "Date", "Player Ranking", 
                       "Opponent Ranking", "Tagged By")
  missingColumns <- setdiff(coreShotColumns, colnames(df))
  if(length(missingColumns) > 0) {
    errs <- AddError(errs, paste("Missing columns: ", 
                                 paste(missingColumns, collapse = ", "),
                                 sep = ""), fnm) 
  }else {
    
    ################# COLUMN TYPES ##########################
    if (nrow(df %>% filter(is.na(as.Date(as.character(Date), "%Y%m%d")) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Bad 'Date' value: ", 
                                   paste(unique((df %>% filter(is.na(as.Date(as.character(Date), 
                                                                             "%Y%m%d")) == TRUE))$Date),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(is.na(as.numeric(as.character(Set))) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Non numeric 'Set' value: ", 
                                   paste(unique((df %>% filter(is.na(as.numeric(as.character(Set))) == TRUE))$Set),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(is.na(as.numeric(as.character(`Shot Count`))) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Non numeric 'Shot Count' value on row(s): ", 
                                   paste(unique((df %>% mutate(idx = row_number()) %>% 
                                                   filter(is.na(as.numeric(as.character(`Shot Count`))) == 
                                                            TRUE))$idx),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# NO BLANKS #####################
    noBlanksCols <- c("Set", "Game Score", "Scoreline", "Side", "Player Serving", "Player Returning", "Serve")
    for (i in 1:length(noBlanksCols)) {
      if (noBlanksCols[i] %in% colnames(df)) {
        if(nrow(df %>% filter(no.value(get(noBlanksCols[i])) == TRUE)) > 0) { 
          errs <- AddError(errs, paste("Blank in '", noBlanksCols[i], "' Column, on row(s): ", 
                                       paste((df %>% mutate(idx = row_number()) %>% 
                                                filter(no.value(get(noBlanksCols[i])) == TRUE))$idx, collapse = ", "), 
                                       sep = ""), fnm) 
        }
      }else {
        errs <- AddError(errs, paste("Missing Column: ", noBlanksCols[i], sep = ""), fnm) 
      }
    }
    
    ################# CONSISTENT MATCH DETAILS #################
    consistentCols <- c("Tagged Player", "Opponent Player", "Tournament", "Tournament Level", "Round", "Court Surface", 
                        "Date", "Player Ranking", "Opponent Ranking", "Tagged By")
    for (i in 1:length(consistentCols)) {
      if (consistentCols[i] %in% colnames(df)) {
        if(nrow(df %>% filter(no.value(get(consistentCols[i])) == TRUE)) > 0) { 
          errs <- AddError(errs, paste("Blank in '", consistentCols[i], "' Column, on row(s): ",
                                       paste((df %>% mutate(idx = row_number()) %>% 
                                                filter(no.value(get(consistentCols[i])) == TRUE))$idx, collapse = ", "), 
                                       sep = ""), fnm) 
        }else if(nrow(df %>% distinct(get(consistentCols[i]))) > 1) { 
          errs <- AddError(errs, paste("More than 1 value in '", consistentCols[i], "' Column: ", 
                                       paste("'", paste(as.list(df %>% distinct(get(consistentCols[i])))[[1]], 
                                                        collapse = "', '"), "'", sep = ""),
                                       sep = ""), fnm) 
        }
      }else {
        errs <- AddError(errs, paste("Missing Column: ", consistentCols[i], sep = ""), fnm) 
      }
    }
    
    ################# PLAYER SERVING / PLAYER RETURNING ################
    taggedPlayer <- df[1,]$`Tagged Player`
    opponentPlayer <- df[1,]$`Opponent Player`
    if (nrow(df %>% filter(`Player Serving` != paste(taggedPlayer, "Serving") &
                           `Player Serving` != paste(opponentPlayer, "Serving"))) > 0) {
      errs <- AddError(errs, paste("Bad value in `Player Serving` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(`Player Serving` != paste(taggedPlayer, "Serving") &
                                                     `Player Serving` != paste(opponentPlayer, "Serving")))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(`Player Returning` != paste(taggedPlayer, "Returning") &
                           `Player Returning` != paste(opponentPlayer, "Returning"))) > 0) {
      errs <- AddError(errs, paste("Bad value in `Player Returning` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(`Player Returning` != paste(taggedPlayer, "Returning") &
                                                     `Player Returning` != paste(opponentPlayer, "Returning")))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter((`Player Serving` == paste(taggedPlayer, "Serving") &
                            `Player Returning` != paste(opponentPlayer, "Returning")) |
                           (`Player Serving` == paste(opponentPlayer, "Serving") &
                            `Player Returning` != paste(taggedPlayer, "Returning")))) > 0) {
      errs <- AddError(errs, paste("Wrong combination in `Player Serving/Returning` columns on row(s): ",
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((`Player Serving` == paste(taggedPlayer, "Serving") &
                                                      `Player Returning` != paste(opponentPlayer, "Returning")) |
                                                     (`Player Serving` == paste(opponentPlayer, "Serving") &
                                                        `Player Returning` != paste(taggedPlayer, "Returning"))))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# SERVE ##############################
    if (length(setdiff(unique(df$Serve), c("First Serve In", "Second Serve In", "Second Serve Out"))) > 0) {
      errs <- AddError(errs, paste("Wrong value in `Serve` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% filter(!Serve %in% 
                                                                                         c("First Serve In", "Second Serve In", "Second Serve Out")))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# FIRST SERVE ############################
    if (nrow(df %>% filter(Serve == "First Serve In", is.na(as.numeric(`Serve 1 Direction`)) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Wrong 'Serve 1 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   is.na(as.numeric(`Serve 1 Direction`)) == TRUE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(Serve == "First Serve In", 
                           no.value(`Serve 2 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Serve 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   no.value(`Serve 2 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(Serve == "First Serve In", 
                           no.value(`Return 2 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Return 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   no.value(`Return 2 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# SECOND SERVE ############################
    if (nrow(df %>% filter(Serve != "First Serve In", 
                           is.na(as.numeric(`Serve 1 Direction`)) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Serve 1 Direction' is numeric when First Serve is out on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve != "First Serve In", 
                                                   is.na(as.numeric(`Serve 1 Direction`)) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Serve == "Second Serve In", 
                           is.na(as.numeric(`Serve 2 Direction`)) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Wrong 'Serve 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "Second Serve In", 
                                                   is.na(as.numeric(`Serve 2 Direction`)) == TRUE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Serve != "First Serve In", 
                           no.value(`Return 1 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Return 1 Direction' recorded when First Serve is out on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve != "First Serve In", 
                                                   no.value(`Return 1 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    ################# DF AND ACE ###########################
    DF_Ace_BlankCols <- c("Return Stroke", "Return 1 Direction", "Return 2 Direction", "Ball 3 Stroke", 
                          "Ball 3 Situation", "Situation", "Rally Length", "Rally Stroke", "Stroke Type", 
                          "Player Final Shot")
    for (i in 1:length(DF_Ace_BlankCols)) {
      if(nrow(df %>% filter((Finish == "Ace" | Finish == "Double Fault") & 
                            no.value(get(DF_Ace_BlankCols[i])) == FALSE)) > 0) { 
        errs <- AddError(errs, paste("Value recorded in '", DF_Ace_BlankCols[i], 
                                     "' Column when 'Finish' is DF/Ace, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Ace" | Finish == "Double Fault") & 
                                                       no.value(get(DF_Ace_BlankCols[i])) == FALSE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    ################# SIDE ################################
    if (nrow(df %>% filter(Side != "Deuce" & Side != "Ad")) > 0) {
      errs <- AddError(errs, paste("Side not recorded as Deuce or Ad, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Deuce" & Side != "Ad"))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Side != "Deuce" & 
                           ((Serve == "First Serve In" & as.character(`Serve 1 Direction`) %in% c("1", "2", "3")) | 
                            (Serve == "Second Serve In" & as.character(`Serve 2 Direction`) %in% c("1", "2", "3"))))) 
        > 0) {
      errs <- AddError(errs, paste("Wrong serve direction for Deuce Side, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Deuce" & 
                                                     ((Serve == "First Serve In" & 
                                                         as.character(`Serve 1 Direction`) %in% c("1", "2", "3")) | 
                                                        (Serve == "Second Serve In" & 
                                                           as.character(`Serve 2 Direction`) %in% c("1", "2", "3")))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Side != "Ad" & 
                           ((Serve == "First Serve In" & as.character(`Serve 1 Direction`) %in% c("4", "5", "6")) | 
                            (Serve == "Second Serve In" & as.character(`Serve 2 Direction`) %in% c("4", "5", "6"))))) 
        > 0) {
      errs <- AddError(errs, paste("Wrong serve direction for Ad Side, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Ad" & 
                                                     ((Serve == "First Serve In" & 
                                                         as.character(`Serve 1 Direction`) %in% c("4", "5", "6")) | 
                                                        (Serve == "Second Serve In" & 
                                                           as.character(`Serve 2 Direction`) %in% c("4", "5", "6")))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    ################# RETURN WINNER/ERROR ########################
    Return_BlankCols <- c("Ball 3 Stroke", "Ball 3 Situation", "Rally Length", "Rally Stroke")
    for (i in 1:length(Return_BlankCols)) {
      if(nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") & 
                            no.value(get(Return_BlankCols[i])) == FALSE)) > 0) { 
        errs <- AddError(errs, paste("Value recorded in '", Return_BlankCols[i], 
                                     "' Column when 'Finish' is Return Winner/Error, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Return Winner" | Finish == "Return Error") & 
                                                       no.value(get(Return_BlankCols[i])) == FALSE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    if (nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") &
                           no.value(`Stroke Type`) == TRUE)) > 0) {
      errs <- AddError(errs, paste("No 'Stroke Type' for Return Winner/Error, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Return Winner" | Finish == "Return Error") &
                                                     no.value(`Stroke Type`) == TRUE))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    
    if (nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") &
                           no.value(`Return Stroke`) == TRUE)) > 0) {
      errs <- AddError(errs, paste("No 'Return Stroke' for Return Winner/Error, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Return Winner" | Finish == "Return Error") &
                                                     no.value(`Return Stroke`) == TRUE))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    ################# WINNERS/ERRORS ######################
    W_UE_FE_NoBlanks <- c("Return Stroke", "Ball 3 Stroke", "Ball 3 Situation", "Situation", "Rally Length", 
                          "Rally Stroke", "Stroke Type", "Player Final Shot")
    for (i in 1:length(W_UE_FE_NoBlanks)) {
      if(nrow(df %>% filter((Finish == "Winner" | Finish == "Unforced Error" | Finish == "Forced Error") & 
                            no.value(get(W_UE_FE_NoBlanks[i])) == TRUE)) > 0) { 
        errs <- AddError(errs, paste("Missing value in '", W_UE_FE_NoBlanks[i], 
                                     "' Column when 'Finish' is Winner/Error, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Winner" | Finish == "Unforced Error" | 
                                                        Finish == "Forced Error") & 
                                                       no.value(get(W_UE_FE_NoBlanks[i])) == TRUE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    if (nrow(df %>% filter(Finish == "Winner" & (
      gsub(" Final Shot", "", `Player Final Shot`) != 
      gsub(" Point Won", "", `Point Won By`) | 
      gsub(" Final Shot", "", `Player Final Shot`) == 
      gsub(" Point Lost", "", `Point Lost By`) |
      (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
       gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`)))) > 0) {
      errs <- AddError(errs, paste("'Finish' is Winner but 'Final Shot' Player doens't match 'Point Won By' or Tagged/Opponent Player, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Finish == "Winner" & (
                                              gsub(" Final Shot", "", `Player Final Shot`) != 
                                                gsub(" Point Won", "", `Point Won By`) | 
                                                gsub(" Final Shot", "", `Player Final Shot`) == 
                                                gsub(" Point Lost", "", `Point Lost By`) |
                                                (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
                                                   gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter((Finish == "Unforced Error" | Finish == "Forced Error") & (
      gsub(" Final Shot", "", `Player Final Shot`) == 
      gsub(" Point Won", "", `Point Won By`) | 
      gsub(" Final Shot", "", `Player Final Shot`) != 
      gsub(" Point Lost", "", `Point Lost By`) |
      (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
       gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`)))) > 0) {
      errs <- AddError(errs, paste("'Finish' is an Error but 'Final Shot' Player doens't match 'Point Lost By' or Tagged/Opponent Player, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Unforced Error" | Finish == "Forced Error") & (
                                              gsub(" Final Shot", "", `Player Final Shot`) == 
                                                gsub(" Point Won", "", `Point Won By`) | 
                                                gsub(" Final Shot", "", `Player Final Shot`) != 
                                                gsub(" Point Lost", "", `Point Lost By`) |
                                                (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
                                                   gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    
  }
  
  return(errs)
}

ErrorCheck_PBP_WC <- function(df, errs, fnm) {
  ################# MISSING COLUMNS #########################
  coreShotColumns <- c("Name", "Position", "Duration", "Set", "Game Score", "Scoreline", "Side", 
                       "Player Serving", "Player Returning", "Serve", "Serve 1 Direction", "Serve 2 Direction", 
                       "Return Stroke", "Return 1 Direction", "Return 2 Direction", 
                       "Player Net Approach", "Opponent Net Approach", "Situation", 
                       "Shot Count", "Rally Length", "Rally Stroke", "Stroke Type", "Player Final Shot", 
                       "Finish", "Point Won By", "Point Lost By", "Tagged Player", "Opponent Player", 
                       "Tournament", "Tournament Level", "Round", "Court Surface", "Date", "Player Ranking", 
                       "Opponent Ranking", "Tagged By")
  missingColumns <- setdiff(coreShotColumns, colnames(df))
  if(length(missingColumns) > 0) {
    errs <- AddError(errs, paste("Missing columns: ", 
                                 paste(missingColumns, collapse = ", "),
                                 sep = ""), fnm) 
  }else {
    
    ################# COLUMN TYPES ##########################
    if (nrow(df %>% filter(is.na(as.Date(as.character(Date), "%Y%m%d")) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Bad 'Date' value: ", 
                                   paste(unique((df %>% filter(is.na(as.Date(as.character(Date), 
                                                                             "%Y%m%d")) == TRUE))$Date),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(is.na(as.numeric(as.character(Set))) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Non numeric 'Set' value: ", 
                                   paste(unique((df %>% filter(is.na(as.numeric(as.character(Set))) == TRUE))$Set),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(is.na(as.numeric(as.character(`Shot Count`))) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Non numeric 'Shot Count' value on row(s): ", 
                                   paste(unique((df %>% mutate(idx = row_number()) %>% 
                                                   filter(is.na(as.numeric(as.character(`Shot Count`))) == 
                                                            TRUE))$idx),
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# NO BLANKS #####################
    noBlanksCols <- c("Set", "Game Score", "Scoreline", "Side", "Player Serving", "Player Returning", "Serve")
    for (i in 1:length(noBlanksCols)) {
      if (noBlanksCols[i] %in% colnames(df)) {
        if(nrow(df %>% filter(no.value(get(noBlanksCols[i])) == TRUE)) > 0) { 
          errs <- AddError(errs, paste("Blank in '", noBlanksCols[i], "' Column, on row(s): ", 
                                       paste((df %>% mutate(idx = row_number()) %>% 
                                                filter(no.value(get(noBlanksCols[i])) == TRUE))$idx, collapse = ", "), 
                                       sep = ""), fnm) 
        }
      }else {
        errs <- AddError(errs, paste("Missing Column: ", noBlanksCols[i], sep = ""), fnm) 
      }
    }
    
    ################# CONSISTENT MATCH DETAILS #################
    consistentCols <- c("Tagged Player", "Opponent Player", "Tournament", "Tournament Level", "Round", "Court Surface", 
                        "Date", "Player Ranking", "Opponent Ranking", "Tagged By")
    for (i in 1:length(consistentCols)) {
      if (consistentCols[i] %in% colnames(df)) {
        if(nrow(df %>% filter(no.value(get(consistentCols[i])) == TRUE)) > 0) { 
          errs <- AddError(errs, paste("Blank in '", consistentCols[i], "' Column, on row(s): ",
                                       paste((df %>% mutate(idx = row_number()) %>% 
                                                filter(no.value(get(consistentCols[i])) == TRUE))$idx, collapse = ", "), 
                                       sep = ""), fnm) 
        }else if(nrow(df %>% distinct(get(consistentCols[i]))) > 1) { 
          errs <- AddError(errs, paste("More than 1 value in '", consistentCols[i], "' Column: ", 
                                       paste("'", paste(as.list(df %>% distinct(get(consistentCols[i])))[[1]], 
                                                        collapse = "', '"), "'", sep = ""),
                                       sep = ""), fnm) 
        }
      }else {
        errs <- AddError(errs, paste("Missing Column: ", consistentCols[i], sep = ""), fnm) 
      }
    }
    
    ################# PLAYER SERVING / PLAYER RETURNING ################
    taggedPlayer <- df[1,]$`Tagged Player`
    opponentPlayer <- df[1,]$`Opponent Player`
    if (nrow(df %>% filter(`Player Serving` != paste(taggedPlayer, "Serving") &
                           `Player Serving` != paste(opponentPlayer, "Serving"))) > 0) {
      errs <- AddError(errs, paste("Bad value in `Player Serving` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(`Player Serving` != paste(taggedPlayer, "Serving") &
                                                     `Player Serving` != paste(opponentPlayer, "Serving")))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(`Player Returning` != paste(taggedPlayer, "Returning") &
                           `Player Returning` != paste(opponentPlayer, "Returning"))) > 0) {
      errs <- AddError(errs, paste("Bad value in `Player Returning` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(`Player Returning` != paste(taggedPlayer, "Returning") &
                                                     `Player Returning` != paste(opponentPlayer, "Returning")))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter((`Player Serving` == paste(taggedPlayer, "Serving") &
                            `Player Returning` != paste(opponentPlayer, "Returning")) |
                           (`Player Serving` == paste(opponentPlayer, "Serving") &
                            `Player Returning` != paste(taggedPlayer, "Returning")))) > 0) {
      errs <- AddError(errs, paste("Wrong combination in `Player Serving/Returning` columns on row(s): ",
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((`Player Serving` == paste(taggedPlayer, "Serving") &
                                                      `Player Returning` != paste(opponentPlayer, "Returning")) |
                                                     (`Player Serving` == paste(opponentPlayer, "Serving") &
                                                        `Player Returning` != paste(taggedPlayer, "Returning"))))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# SERVE ##############################
    if (length(setdiff(unique(df$Serve), c("First Serve In", "Second Serve In", "Second Serve Out"))) > 0) {
      errs <- AddError(errs, paste("Wrong value in `Serve` column on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% filter(!Serve %in% 
                                                                                         c("First Serve In", "Second Serve In", "Second Serve Out")))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# FIRST SERVE ############################
    if (nrow(df %>% filter(Serve == "First Serve In", is.na(as.numeric(`Serve 1 Direction`)) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Wrong 'Serve 1 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   is.na(as.numeric(`Serve 1 Direction`)) == TRUE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(Serve == "First Serve In", 
                           no.value(`Serve 2 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Serve 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   no.value(`Serve 2 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    if (nrow(df %>% filter(Serve == "First Serve In", 
                           no.value(`Return 2 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Return 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "First Serve In", 
                                                   no.value(`Return 2 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm) 
    }
    
    ################# SECOND SERVE ############################
    if (nrow(df %>% filter(Serve != "First Serve In", 
                           is.na(as.numeric(`Serve 1 Direction`)) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Serve 1 Direction' is numeric when First Serve is out on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve != "First Serve In", 
                                                   is.na(as.numeric(`Serve 1 Direction`)) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Serve == "Second Serve In", 
                           is.na(as.numeric(`Serve 2 Direction`)) == TRUE)) > 0) {
      errs <- AddError(errs, paste("Wrong 'Serve 2 Direction' recorded when First Serve is in on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve == "Second Serve In", 
                                                   is.na(as.numeric(`Serve 2 Direction`)) == TRUE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Serve != "First Serve In", 
                           no.value(`Return 1 Direction`) == FALSE)) > 0) {
      errs <- AddError(errs, paste("'Return 1 Direction' recorded when First Serve is out on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Serve != "First Serve In", 
                                                   no.value(`Return 1 Direction`) == FALSE))$idx,
                                         collapse = ", "),
                                   sep = ""), fnm)
    }
    
    ################# DF AND ACE ###########################
    DF_Ace_BlankCols <- c("Return Stroke", "Return 1 Direction", "Return 2 Direction", 
                          "Situation", "Rally Length", "Rally Stroke", "Stroke Type", 
                          "Player Final Shot")
    for (i in 1:length(DF_Ace_BlankCols)) {
      if(nrow(df %>% filter((Finish == "Ace" | Finish == "Double Fault") & 
                            no.value(get(DF_Ace_BlankCols[i])) == FALSE)) > 0) { 
        errs <- AddError(errs, paste("Value recorded in '", DF_Ace_BlankCols[i], 
                                     "' Column when 'Finish' is DF/Ace, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Ace" | Finish == "Double Fault") & 
                                                       no.value(get(DF_Ace_BlankCols[i])) == FALSE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    ################# SIDE ################################
    if (nrow(df %>% filter(Side != "Deuce" & Side != "Ad")) > 0) {
      errs <- AddError(errs, paste("Side not recorded as Deuce or Ad, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Deuce" & Side != "Ad"))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Side != "Deuce" & 
                           ((Serve == "First Serve In" & as.character(`Serve 1 Direction`) %in% c("1", "2", "3")) | 
                            (Serve == "Second Serve In" & as.character(`Serve 2 Direction`) %in% c("1", "2", "3"))))) 
        > 0) {
      errs <- AddError(errs, paste("Wrong serve direction for Deuce Side, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Deuce" & 
                                                     ((Serve == "First Serve In" & 
                                                         as.character(`Serve 1 Direction`) %in% c("1", "2", "3")) | 
                                                        (Serve == "Second Serve In" & 
                                                           as.character(`Serve 2 Direction`) %in% c("1", "2", "3")))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter(Side != "Ad" & 
                           ((Serve == "First Serve In" & as.character(`Serve 1 Direction`) %in% c("4", "5", "6")) | 
                            (Serve == "Second Serve In" & as.character(`Serve 2 Direction`) %in% c("4", "5", "6"))))) 
        > 0) {
      errs <- AddError(errs, paste("Wrong serve direction for Ad Side, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Side != "Ad" & 
                                                     ((Serve == "First Serve In" & 
                                                         as.character(`Serve 1 Direction`) %in% c("4", "5", "6")) | 
                                                        (Serve == "Second Serve In" & 
                                                           as.character(`Serve 2 Direction`) %in% c("4", "5", "6")))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    ################# RETURN WINNER/ERROR ########################
    Return_BlankCols <- c("Rally Length", "Rally Stroke")
    for (i in 1:length(Return_BlankCols)) {
      if(nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") & 
                            no.value(get(Return_BlankCols[i])) == FALSE)) > 0) { 
        errs <- AddError(errs, paste("Value recorded in '", Return_BlankCols[i], 
                                     "' Column when 'Finish' is Return Winner/Error, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Return Winner" | Finish == "Return Error") & 
                                                       no.value(get(Return_BlankCols[i])) == FALSE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    if (nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") &
                           no.value(`Stroke Type`) == TRUE)) > 0) {
      errs <- AddError(errs, paste("No 'Stroke Type' for Return Winner/Error, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Return Winner" | Finish == "Return Error") &
                                                     no.value(`Stroke Type`) == TRUE))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    
    if (nrow(df %>% filter((Finish == "Return Winner" | Finish == "Return Error") &
                           no.value(`Return Stroke`) == TRUE)) > 0) {
      errs <- AddError(errs, paste("No 'Return Stroke' for Return Winner/Error, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Return Winner" | Finish == "Return Error") &
                                                     no.value(`Return Stroke`) == TRUE))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    ################# WINNERS/ERRORS ######################
    W_UE_FE_NoBlanks <- c("Return Stroke", "Situation", "Rally Length", 
                          "Rally Stroke", "Stroke Type", "Player Final Shot")
    for (i in 1:length(W_UE_FE_NoBlanks)) {
      if(nrow(df %>% filter((Finish == "Winner" | Finish == "Unforced Error" | Finish == "Forced Error") & 
                            no.value(get(W_UE_FE_NoBlanks[i])) == TRUE)) > 0) { 
        errs <- AddError(errs, paste("Missing value in '", W_UE_FE_NoBlanks[i], 
                                     "' Column when 'Finish' is Winner/Error, on row(s): ", 
                                     paste((df %>% mutate(idx = row_number()) %>% 
                                              filter((Finish == "Winner" | Finish == "Unforced Error" | 
                                                        Finish == "Forced Error") & 
                                                       no.value(get(W_UE_FE_NoBlanks[i])) == TRUE))$idx, 
                                           collapse = ", "), 
                                     sep = ""), fnm)
      }
    }
    
    if (nrow(df %>% filter(Finish == "Winner" & (
      gsub(" Final Shot", "", `Player Final Shot`) != 
      gsub(" Point Won", "", `Point Won By`) | 
      gsub(" Final Shot", "", `Player Final Shot`) == 
      gsub(" Point Lost", "", `Point Lost By`) |
      (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
       gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`)))) > 0) {
      errs <- AddError(errs, paste("'Finish' is Winner but 'Final Shot' Player doens't match 'Point Won By' or Tagged/Opponent Player, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter(Finish == "Winner" & (
                                              gsub(" Final Shot", "", `Player Final Shot`) != 
                                                gsub(" Point Won", "", `Point Won By`) | 
                                                gsub(" Final Shot", "", `Player Final Shot`) == 
                                                gsub(" Point Lost", "", `Point Lost By`) |
                                                (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
                                                   gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    if (nrow(df %>% filter((Finish == "Unforced Error" | Finish == "Forced Error") & (
      gsub(" Final Shot", "", `Player Final Shot`) == 
      gsub(" Point Won", "", `Point Won By`) | 
      gsub(" Final Shot", "", `Player Final Shot`) != 
      gsub(" Point Lost", "", `Point Lost By`) |
      (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
       gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`)))) > 0) {
      errs <- AddError(errs, paste("'Finish' is an Error but 'Final Shot' Player doens't match 'Point Lost By' or Tagged/Opponent Player, on row(s): ", 
                                   paste((df %>% mutate(idx = row_number()) %>% 
                                            filter((Finish == "Unforced Error" | Finish == "Forced Error") & (
                                              gsub(" Final Shot", "", `Player Final Shot`) == 
                                                gsub(" Point Won", "", `Point Won By`) | 
                                                gsub(" Final Shot", "", `Player Final Shot`) != 
                                                gsub(" Point Lost", "", `Point Lost By`) |
                                                (gsub(" Final Shot", "", `Player Final Shot`) != `Tagged Player` &
                                                   gsub(" Final Shot", "", `Player Final Shot`) != `Opponent Player`))))$idx, 
                                         collapse = ", "), 
                                   sep = ""), fnm)
    }
    
    
  }
  
  return(errs)
}