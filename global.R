library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(scorekeepeR)

data_fldr = "data"
if (!dir.exists(data_fldr)) dir.create(data_fldr)
teams_file = file.path(data_fldr, "Teams.csv")
teams = if (file.exists(teams_file)) read.csv(teams_file) else init_teams_table()

players_file = file.path(data_fldr, "Players.csv")
players = if (file.exists(players_file)) read.csv(players_file) else init_players_table()

rosters_file = file.path(data_fldr, "Rosters.csv")
rosters = if (file.exists(rosters_file)) read.csv(rosters_file) else init_rosters_table()


# players <- read.csv(file.path(data_fldr, "Players.csv"), stringsAsFactors = FALSE, colClasses =  c("integer", "character", "character")) # specifying class important b/c an empty column will be read as logical; caused problems with paste
# # rosters are the players on each team (players can be on more than one team)
# rosters <- read.csv(file.path(data_fldr, "Rosters.csv"), stringsAsFactors = FALSE)
# # track IDs separately to avoid duplicating IDs
# gameIDs <- read.csv(file.path(data_fldr, "GameIDs.csv"), stringsAsFactors = FALSE) 
# games <- read.csv(file.path(data_fldr, "Games.csv"), stringsAsFactors = FALSE)
# game_stats <- read.csv(file.path(data_fldr, "GameStats.csv"), stringsAsFactors = FALSE) 

# if (!dir.exists("gamelogs")) dir.create("gamelogs")
# 
# stats_group_by_opts <- c("Team", "Game" = "GameID", "Player" = "PlayerID")
# 
# first_num <- function(first, num){
#   # assumes that roster includes at least first name or number
#   ifelse(num == "" | is.na(num), first,
#          ifelse(first == "" | is.na(first),
#                 paste0("#", num),
#                 paste0(first, " (#", num, ")")))
# }
# 
# first_last <- function(first, last){
#   ifelse(last == "", first, paste(first, last))
# }
# 
# log_action <- function(undo_bool, game_log_text, player){
#   undo = ifelse(undo_bool, "UNDO ", "")
#   paste0(undo, game_log_text, player)
# }
# 
# display_shooting <- function(data, ri, stat, ma = NULL){
#   # ma = is length 2 vector of made and attempted
#   dri = data[ri,]
#   raw.value = dri[[stat]]
#   # rv was probably short for render value; not sure about the t
#   rvt = "--"                # initial value for renderValueBox
#   # st = subtitle for renderValueBox
#   rvt.st = ifelse(stat == "TS%", "TS%", paste0(ma[1], " (0/0)"))
#   if (length(raw.value) > 0){  
#     if(!is.na(raw.value)){
#       rvt = paste0(raw.value, "%")
#       rvt.st = ifelse(stat == "TS%", "TS%", paste0(stat, " (", dri[[ma[1]]], "/", dri[[ma[2]]], ")"))
#     }
#   }
#   c(rvt, rvt.st)
# }
# 
# true_shooting <- function(PTS, FTA, FGA){
#   # https://en.wikipedia.org/wiki/True_shooting_percentage
#   PTS/(0.88 * FTA + 2 * FGA) * 100
# }
# 
# efficiency = function(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV){
#   # https://en.wikipedia.org/wiki/Efficiency_(basketball)
#   PTS + REB + AST + STL + BLK - (FGA - FGM) - (FTA - FTM) - TOV
# }
# 
# # counting stats stored in GameStats.csv
# # points and rebounds are only columns that could be calculated from other columns
# stats_cols = c("FTM", "FTA", "FGM2", "FGA2", "FGM3", "FGA3", "TOV", "STL", 
#                "DREB", "OREB", "BLK", "AST", "PF", "PTS", "REB", "DNP")
# 
# # common columns used for statistics display
# stats_display_cols = c("PTS", "FGM", "FGA", "FG%", "3PM" = "FGM3", "3PA" = "FGA3", "3P%", 
#                        "FTM", "FTA", "FT%", "TS%", "OREB", "DREB", "REB", "AST", "TOV", "STL", "BLK", "PF")


