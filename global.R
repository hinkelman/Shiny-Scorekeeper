library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)

if (!dir.exists("gamelogs")) dir.create("gamelogs")

stats_group_by_opts <- c("Team" = "TeamID", "Game" = "GameID", "Player" = "PlayerID")

num_first <- function(num, first){
  ifelse(num == "" | is.na(num), first,
         ifelse(first == "" | is.na(first),
                paste0("#", num),
                paste0("#", num, " - ", first)))
}

first_last <- function(first, last){
  ifelse(last == "", first, paste(first, last))
}

log_action <- function(undo_bool, game_log_text, player){
  undo = ifelse(undo_bool, "UNDO ", "")
  paste0(undo, game_log_text, player)
}

display_shooting <- function(data, ri, stat, ma = NULL){
  dri = data[ri,]
  raw.value = dri[[stat]]
  rvt = "--"
  rvt.st = ifelse(stat == "TS%", "TS%", paste0(ma[1], " (0/0)"))
  if (length(raw.value) > 0){  
    if(!is.na(raw.value)){
      rvt = paste0(raw.value, "%")
      rvt.st = ifelse(stat == "TS%", "TS%", paste0(stat, " (", dri[[ma[1]]], "/", dri[[ma[2]]], ")"))
    }
  }
  return(c(rvt, rvt.st))
}
