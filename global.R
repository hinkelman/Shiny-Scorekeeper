library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)

stats_group_by_opts <- c("Team" = "TeamID", "Game" = "GameID", "Player" = "PlayerID")

first_num <- function(name, num){
  ifelse(num == "" | is.na(num), name,
         ifelse(name == "" | is.na(name),
                paste0("#", num),
                paste0("#", num, " - ", name)))
}

first_last <- function(first, last){
  ifelse(last == "", first, paste(first, last))
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
