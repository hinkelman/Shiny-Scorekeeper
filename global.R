library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(scorekeepeR)

# max height of value boxes
mxht = "120px"

data_dir = "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

teams_file = file.path(data_dir, "Teams.csv")
teams = if (file.exists(teams_file)) read.csv(teams_file) else init_teams_table()

players_file = file.path(data_dir, "Players.csv")
players = if (file.exists(players_file)) read.csv(players_file) else init_players_table()

rosters_file = file.path(data_dir, "Rosters.csv")
if (file.exists(rosters_file)){
  rosters = read.csv(rosters_file) |> 
    mutate(Number = as.character(Number))
} else { 
  rosters = init_rosters_table()
}

games_file = file.path(data_dir, "Games.csv")
games = if (file.exists(games_file)) read.csv(games_file) else init_games_table()

game_stats_file = file.path(data_dir, "GameStats.csv")
game_stats = if (file.exists(game_stats_file)) read.csv(game_stats_file) else init_game_stats_table()

if (!dir.exists(file.path(data_dir, "gamelogs"))) dir.create(file.path(data_dir, "gamelogs"))

# stats_group_by_opts <- c("Team", "Game" = "GameID", "Player" = "PlayerID")


