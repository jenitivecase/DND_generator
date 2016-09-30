###########################################################################
###########################################################################
### An attempt to combine all the D&D generators ##########################
### This slightly less crappy code written by Jennifer Brussow ############
### June 2016 #############################################################
###########################################################################
###########################################################################
options("stringsAsFactors"= FALSE)

#SET WORKING DIRECTORY
setwd("C:\\Users\\Jen\\Dropbox\\D&D Projects")

#AUTOMATING PACKAGES NEEDED FOR ANALYSES
needed_packages = c("dplyr", "tidyr", "gsheet", "gridExtra", "svDialogs")

for (i in 1:length(needed_packages)){
  haspackage = require(needed_packages[i], character.only = TRUE)
  if (haspackage==FALSE){
    install.packages(needed_packages[i])
  }
  library(needed_packages[i], character.only = TRUE)
}

retry_num <- function(msg, default=1){
  x <- dlgInput(message = msg, default = default)$res
    while(!length(x)){
      x <- dlgInput(message = "Please try again:", default = default)$res
      while(!length(x)){
        x <- dlgInput(message = "Please try again:", default = default)$res
      }
      while(suppressWarnings(is.na(as.integer(x)))){
        x <- dlgInput(message = "Please enter a *numeric* value:", default = default)$res
      }
    }
    while(suppressWarnings(is.na(as.integer(x)))){
      x <- dlgInput(message = "Please enter a *numeric* value:", default = default)$res
      while(!length(x)){
        x <- dlgInput(message = "Please try again:", default = default)$res
      }
      while(suppressWarnings(is.na(as.integer(x)))){
        x <- dlgInput(message = "Please enter a *numeric* value:", default = default)$res
      }
    }
  x <- as.numeric(x)
  return (x)
}

retry_set<- function(y, mult, msg){
  x <- dlgList(y, multiple = mult, title=msg)$res
    while(!length(x)){
      x <- dlgList(y, multiple = mult, title="Please try again")$res
    }
  return(x)
}

#picking your desired product (library/loot/reagents)
task_options <- c("Library", "Loot", "Reagents")
task <- retry_set(task_options, FALSE, "Please select your desired outcome:")

#picking your output format
out_options <- c(".pdf", ".csv")
out_format <- retry_set(out_options, FALSE, "Select your output format:")

#Library Generator####
if(task == "Library"){
  #pull data from google sheet
  booklib <- as.data.frame(gsheet2tbl('docs.google.com/spreadsheets/d/15VkFkO-1MmFQzrmwvixrVhgJxYqB7kx4UFGTEp98sGk'))
  #weighting by num copies
  booklib <- booklib[rep(1:nrow(booklib), times = booklib$Num_copies),]
  
  #logistical setup
  lib_title <- dlgInput(message = "Enter a title for this library:")$res
  topics <- c("All", unique(booklib$Primary_Topic))
  topic_choice <- retry_set(topics, TRUE, "Pick your topic(s):")
  languages <- c("All", unique(booklib$Language))
  lang_choice <- retry_set(languages, TRUE, "Pick your language(s):")
  book_num <- retry_num("How many books would you like to generate?")
  
  #filter the weighted dataset based on topic & language selections
  booklib_mod <- booklib
  if(!("All" %in% topic_choice)){
    booklib_mod <- booklib_mod[which(booklib_mod$Primary_Topic %in% c(topic_choice, "General") | 
                                       booklib_mod$Secondary_Topic %in% c(topic_choice, "General")),] 
  } 
  if(!("All" %in% lang_choice)){
    booklib_mod <- booklib_mod[which(booklib_mod$Language %in% lang_choice),]
  }
  
  #selection of books
  selection <- data.frame(booklib_mod)
  if(nrow(distinct(selection)) >= book_num){
    selection <- sample_n(booklib_mod, book_num, replace=FALSE)
    selection <- distinct(selection)
    
    while(nrow(selection)<book_num){  
      value <- sample_n(booklib_mod, 1, replace=FALSE)
      selection <- rbind(selection, value)
      selection <-distinct(selection)
    }
    #output
    if(out_format == ".pdf"){
      pdf(file=paste0(lib_title, ".pdf"), width=14, height=7.5, pointsize=8)
      print(lib_title)
      grid.table(selection)
      dev.off()
      dlgMessage(paste0("Your library has been saved at ", getwd(), "/", paste0(lib_title, out_format)))
    } else if(out_format == ".csv"){
      write.csv(selection, paste0(lib_title, ".csv"), row.names = FALSE)
      dlgMessage(paste0("Your library has been saved at ", getwd(), "/", paste0(lib_title, out_format)))
    }
  } else {
    dlgMessage("There are not enough books in the database to meet your criteria!")
  }

#Loot Generator####
} else if(task == "Loot"){
  #pull data from google sheet
  lootlib <- as.data.frame(gsheet2tbl('docs.google.com/spreadsheets/d/1GBcZV7h_bhnZGhJhU22ygqBCYpjKoFGgOPvAqUBol_g'))
  
  #logistics
  lvl <- retry_num("What is your party's average character level?")
  loot_title <- dlgInput(message = "Enter a title for this loot instance:")$res
  loot_locations <- names(lootlib[3:length(lootlib)])
  location <- retry_set(loot_locations, FALSE, "Where is this loot found?")
  
  #determine GP and number of items
  GP <- c("Gold Pieces", round(rnorm(1, mean=(lvl*3), sd=lvl), 0))
  num_items <- sample(c(1:4), 1)
  
  #creating location-specific data, weighting, & selecting
  if(location == "Desk"){
    desk <- lootlib[rep(1:nrow(lootlib), times = lootlib$Desk), c("Item", "Value")]
    results <- sample_n(desk, num_items, replace=FALSE)
  } else if(location == "Trunk"){
    trunk <- lootlib[rep(1:nrow(lootlib), times = lootlib$Trunk), c("Item", "Value")]
    results <- sample_n(trunk, num_items, replace=FALSE)
  } else if (location == "Wardrobe"){
    wardrobe <- lootlib[rep(1:nrow(lootlib), times = lootlib$Wardrobe), c("Item", "Value")]
    results <- sample_n(wardrobe, num_items, replace=FALSE)
  } else if(location == "Table"){
    table <- lootlib[rep(1:nrow(lootlib), times = lootlib$Table), c("Item", "Value")]
    results <- sample_n(table, num_items, replace=FALSE)
  } else if(location == "Body"){
    body <- lootlib[rep(1:nrow(lootlib), times = lootlib$Body), c("Item", "Value")]
    results <- sample_n(body, num_items, replace=FALSE)
  }
  results <- rbind(results, GP)
  #output
  if(out_format == ".pdf"){
    pdf(file=paste0(loot_title, ".pdf"), width=8, height=7.5, pointsize=12)
    print(loot_title)
    grid.table(results)
    dev.off()
    dlgMessage(paste0("Your loot has been saved at ", getwd(), "/", paste0(loot_title, out_format)))
  } else if(out_format == ".csv"){
    write.csv(results, paste0(loot_title, ".csv"), row.names = FALSE)
    dlgMessage(paste0("Your loot has been saved at ", getwd(), "/", paste0(loot_title, out_format)))
  }
#Reagents Generator####
} else if(task == "Reagents"){
  #get data
  reagents <- as.data.frame(gsheet2tbl('docs.google.com/spreadsheets/d/1NLBP-t62U9x7amnDs8xhybQUVrRsNJh66gVS-p0q-mU'))
  #logistical selections
  reagent_title <- dlgInput(message = "Enter a title for this set of reagents:")$res
  locations <- sort(unique(reagents$Location))
  location <- retry_set(locations, TRUE, "Select the location(s):")
  seasons <- c("Spri", "Summ", "Fall", "Wint")
  season <- retry_set(seasons, TRUE, "Select the season(s):")
  zones <- c("All", sort(unique(reagents$Zone)))
  zone <- retry_set(zones, TRUE, "Select the zone(s):")
  Nature <- retry_num("What is the character's Nature skill modifier?", 0)
  Survival <- retry_num("What is the character's Survival skill modifier?", 0)
  Hours <- retry_num("How many hours are being spent searching?")
  baselineskill <- 6
  skill_mod <- (Nature+Survival)-baselineskill
  #filtering by location, season, and zone
  location_filter <- function(initial){
    final <- NULL
    if(location[1] != "All"){
      for(i in 1:length(location)){
        temp <- filter(initial, grepl(location[i], initial$Location)) 
        final <- rbind(final, temp)
      }
    } else {
      final <- initial
    } 
    return(final)
  }
  
  season_filter <- function(initial){
    final <- NULL
    if(season[1] != "All"){
      for(i in 1:length(season)){
        temp <- filter(initial, grepl(season[i], initial$Season)) 
        final <- rbind(final, temp)
      }
    } else {
      final <- initial
    } 
    final <- unique(final)
    return(final)
  }
  
  zone_filter <- function(initial){
    final <- NULL
    if(zone[1] != "All"){
      for(i in 1:length(zone)){
        temp <- filter(initial, grepl(zone[i], initial$Zone)) 
        final <- rbind(final, temp)
      }
    } else {
      final <- initial
    } 
    return(final)
  }
  
  reagents_mod <- NULL
  reagents_mod <- location_filter(reagents)
  reagents_mod <- season_filter(reagents_mod)
  reagents_mod <- zone_filter(reagents_mod)
  #math to determine collection
  skill_mod <- 1+(.05*skill_mod)
  
  reagents_mod$Find.Pct <- reagents_mod$Find.Pct/100
  reagents_mod$Find.Pct <- reagents_mod$Find.Pct*skill_mod
  
  reagents_found <- NULL
  
  for(i in 1:Hours){
    for(i in 1:nrow(reagents_mod)){
      if(runif(1, 0, 1) < reagents_mod[i, "Find.Pct"]){
        reagents_found <- rbind(reagents_found, reagents_mod[i,])
      }
    }
  }
  #output
  if(length(reagents_found) > 0){
    if(out_format == ".pdf"){
      pdf(file=paste0(reagent_title, ".pdf"), width=22, height=7.5, pointsize=8)
      print(reagent_title)
      grid.table(reagents_found)
      dev.off()
      dlgMessage(paste0("Your reagents have been saved at ", getwd(), "/", paste0(reagent_title, out_format)))
    } else if(out_format == ".csv"){
      write.csv(reagents_found, paste0(lib_title, ".csv"), row.names = FALSE)
      dlgMessage(paste0("Your reagents have been saved at ", getwd(), "/", paste0(reagent_title, out_format)))
    }
  } else {
    dlgMessage("No reagents were found!")
  }
  
} 





