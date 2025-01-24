
function(input, output, session) {
  
  rosterServer("roster")
  
  observeEvent(input[["roster-set_roster"]], {
    updateTabsetPanel(session, "nav", "Scorekeeper")
  })
  
  
  # Reactive values ---------------------------------------------------------

  # rv <- reactiveValues(teams = teams, players = players,
  #                      rosters = NULL, roster = NULL,
  #                      game_id = NULL, games = NULL,
  #                      game_stats = NULL, game_stats_raw = NULL, game_stats_calc = NULL,
  #                      game_log = NULL)
  # 
  # # Scorekeeper ---------------------------------------------------------
  # 
  # ## set roster ---------------------------------------------------------
  # 
  # observeEvent(input$tabs, {
  #   if (input$tabs == "scorekeeper" & input$set_roster == 0){
  #     sendSweetAlert(
  #       session = session,
  #       title = "Warning: Need to set roster before scoring a game",
  #       text = NULL,
  #       type = "warning"
  #     )
  #   }
  # })
  # 
  # observeEvent(input$set_roster,{
  #   # update master list of game IDs; always starting a new game when setting a roster (unless testing app; other times?)
  #   gid <- nrow(gameIDs) + 1L # ID and row number are the same
  #   gameIDs[gid,] <<- gid
  #   rv[["game_id"]] <- gid
  #   write.csv(gameIDs, file.path(data_fldr, "GameIDs.csv"), row.names = FALSE)
  #   
  #   # setting roster initializes a new game
  #   rv[["game_stats_raw"]] <- rv[["roster"]] %>% 
  #     mutate(GameID = gid,
  #            FirstNum = first_num(FirstName, Number),
  #            FTM = 0L,
  #            FTA = 0L,
  #            FGM2 = 0L,
  #            FGA2 = 0L,
  #            FGM3 = 0L,
  #            FGA3 = 0L,
  #            TOV = 0L,
  #            STL = 0L,
  #            DREB = 0L,
  #            OREB = 0L,
  #            BLK = 0L,
  #            AST = 0L,
  #            PF = 0L)
  #   updateTabItems(session, "tabs", "scorekeeper") # move to scorekeeper tab after setting roster
  # })
  # 
  # ## game info ---------------------------------------------------------
  # 
  # gameInfo <- reactive({
  #   data.frame(GameID = rv[["game_id"]], 
  #              TeamID = rv[["roster"]]$TeamID[1], # same TeamID for all rows in roster 
  #              Date = as.character(input$game_date), 
  #              Opponent = input$opponent_name, 
  #              TeamScore = as.numeric(input$team_score), 
  #              OpponentScore = as.numeric(input$opp_score),
  #              stringsAsFactors = FALSE)
  # })
  # 
  # gameInfoText <- reactive({
  #   c(paste0("GameID: ", rv[["game_id"]], "; TeamID: ", rv[["roster"]]$TeamID[1], 
  #            "; Date: ", as.character(input$game_date), "; Opponent: ", input$opponent_name),
  #     "---------------------------------------------------------------------")
  # })
  # 
  # observe({
  #   req(rv[["game_id"]])
  #   input$save_game_info # take dependency on save button
  #   toggle("save_game_info", condition = !isTRUE(all.equal(filter(rv[["games"]], GameID == rv[["game_id"]]),
  #                                                          gameInfo())))
  # })
  # 
  # observeEvent(input$save_game_info,{
  #   rv[["games"]] <- filter(rv[["games"]], GameID != rv[["game_id"]]) # drop old game info
  #   tid <- rv[["roster"]]$TeamID[1] # same TeamID for all rows in roster
  #   gi <- nrow(rv[["games"]]) + 1L
  #   rv[["games"]][gi,] <- gameInfo()
  #   write.csv(rv[["games"]], file.path(data_fldr, "Games.csv"), row.names = FALSE) # overwrites previous file
  #   # if game info changes, then top part of game log also changed (so rewriting whole file)
  #   cat(c(gameInfoText(), rv[["game_log"]]), file = file.path("gamelogs", paste0("GameID_", rv[["game_id"]], ".txt")), sep = "\n")
  # })
  # 
  # 
  # ## player info ---------------------------------------------------------
  # 
  # firstNum <- reactive({
  #   req(rv[["roster"]], input$set_roster > 0)
  #   d <- rv[["roster"]] %>% 
  #     mutate(FirstNum = first_num(FirstName, Number ))
  #   unique(d$FirstNum)
  # })
  # 
  # output$selectedPlayer <- renderUI({
  #   radioGroupButtons(inputId = "selected_player", choices = firstNum(), size = "normal", 
  #                     justified = TRUE, direction = "vertical")
  # })
  # 
  # output$DNP <- renderUI({
  #   # did not play
  #   pickerInput(inputId = "dnp", label = "Did Not Play (DNP)", choices = firstNum(), multiple = TRUE)
  # })
  # 
  # ## save game stats ---------------------------------------------------------
  # 
  # observe({
  #   toggle("undo", condition = !is.null(rv[["game_stats_raw"]]))
  # })
  # 
  # gameStatsCalcSelected <- reactive({
  #   # this was how I initially solved this problem (i.e., after deciding not to save calculated columns)
  #   # wouldn't have to drop as many columns from rv[["game_stats_raw"]] but that dataframe doesn't include DNP column
  #   select(rv[["game_stats_calc"]], -FirstName, -LastName, -Number, -FirstNum, -`FT%`, -`FG%`, -`3PT%`, -`TS%`, -EFF)  # drop columns contained in other tables (e.g., names and numbers) and non-counting stats, i.e., stats that can't be summarized with sum
  # })
  # 
  # observe({
  #   req(rv[["game_stats_calc"]])
  #   toggle("save_game_stats", condition = !isTRUE(all.equal(gameStatsCalcSelected(), filter(rv[["game_stats"]], GameID == rv[["game_id"]]))))
  # })
  # 
  # observeEvent(input$save_game_stats,{
  #   rv[["game_stats"]] <- filter(rv[["game_stats"]], GameID != rv[["game_id"]])  # drop old game stats
  #   rv[["game_stats"]] <- bind_rows(rv[["game_stats"]], gameStatsCalcSelected()) # add new game stats
  #   write.csv(rv[["game_stats"]], file.path(data_fldr, "GameStats.csv"), row.names = FALSE)
  #   # if game stats change, then game log also changed (not accounting for situation where game log is changing separately from the stats)
  #   cat(c(gameInfoText(), rv[["game_log"]]), file = file.path("gamelogs", paste0("GameID_", rv[["game_id"]], ".txt")), sep = "\n")
  # })
  # 
  # ## update stats ---------------------------------------------------------
  # 
  # # values are incremented or decremented based on whether undo is switched on
  # change <- reactive({
  #   ifelse(input$undo, -1L, 1L)
  # })
  # 
  # rowIndex <- reactive({
  #   which(rv[["game_stats_raw"]]$FirstNum == input$selected_player)
  # })
  # 
  # output$gameLogLastTitle <- renderText({
  #   out = ""
  #   if (!is.null(rv[["game_log"]])) out = "Last game log entry:"
  #   out
  # })
  # 
  # output$gameLogLastText <- renderText({
  #   req(rv[["game_log"]])
  #   rv[["game_log"]][length(rv[["game_log"]])]
  # })
  # 
  # ## tried this approach but yielded "Error in : promise already under evaluation: recursive default argument reference or earlier problems?"
  # ## leads to redundant code below
  # # updateGameStats <- function(data = rv[["game_stats_raw"]], ri = rowIndex(), change = change(), columns){
  # #   for (i in columns){
  # #     ci = which(names(data) == i)
  # #     data[ri,ci] = data[ri,ci] + change
  # #   }
  # #   return(data)
  # # }
  # 
  # ### free throws ---------------------------------------------------------
  # 
  # observeEvent(input$miss_1,{
  #   # need the redundancy of calling log_action in every observeEvent so that the logged action is not too reactive (e.g., changing with undo or player)
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed free throw by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "FTA")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$make_1,{ 
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made free throw by ", input$selected_player))
  #   columns <- c("FTM", "FTA")
  #   ri <- rowIndex()
  #   for (i in columns){
  #     ci <- which(names(rv[["game_stats_raw"]]) == i)
  #     rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  #   }
  # })
  # 
  # ### field goals ---------------------------------------------------------
  # 
  # observeEvent(input$miss_2,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed field goal by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "FGA2")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$make_2,{ 
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made field goal by ", input$selected_player))
  #   columns <- c("FGM2", "FGA2")
  #   ri <- rowIndex()
  #   for (i in columns){
  #     ci <- which(names(rv[["game_stats_raw"]]) == i)
  #     rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  #   }
  # })
  # 
  # ### three pointers ---------------------------------------------------------
  # 
  # observeEvent(input$miss_3,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed 3-pt field goal by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "FGA3")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$make_3,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made 3-pt field goal by ", input$selected_player))
  #   columns <- c("FGM3", "FGA3")
  #   ri <- rowIndex()
  #   for (i in columns){
  #     ci <- which(names(rv[["game_stats_raw"]]) == i)
  #     rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  #   }
  # })
  # 
  # ### steals and turnovers ---------------------------------------------------------
  # 
  # observeEvent(input$stl,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Steal by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "STL")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$tov,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Turnover by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "TOV")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # ### rebounds ---------------------------------------------------------
  # 
  # observeEvent(input$dreb,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Defensive rebound by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "DREB")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$oreb,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Offensive rebound by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "OREB")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # ### blocks and assists ---------------------------------------------------------
  # 
  # observeEvent(input$blk,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Block by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "BLK")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # observeEvent(input$ast,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Assist by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "AST")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # ### fouls ---------------------------------------------------------
  # 
  # observeEvent(input$pf,{
  #   rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Foul by ", input$selected_player))
  #   ri <- rowIndex()
  #   ci <- which(names(rv[["game_stats_raw"]]) == "PF")
  #   rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  # })
  # 
  # ## calculate stats ---------------------------------------------------------
  # 
  # # watch for changes to rv[["game_stats_raw"]] (and input$dnp) to update calculated columns
  # observe({
  #   req(rv[["game_stats_raw"]])
  #   temp <- rv[["game_stats_raw"]] %>% 
  #     mutate(PTS = FTM + FGM2 * 2 + FGM3 * 3,
  #            REB = OREB + DREB,
  #            `FT%` = round(FTM/FTA*100),
  #            FGM = FGM2 + FGM3,
  #            FGA = FGA2 + FGA3,
  #            `FG%` = round(FGM/FGA*100),
  #            `3PT%` = round(FGM3/FGA3*100),
  #            `TS%` = round(true_shooting(PTS, FTA, FGA)),
  #            EFF = efficiency(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV))
  #   if (is.null(input$dnp)){  # if dnp is null, then no players selected
  #     rv[["game_stats_calc"]] <- temp %>% mutate(DNP = 0)
  #   }else{
  #     rv[["game_stats_calc"]] <- temp %>% 
  #       mutate(DNP = ifelse(FirstNum %in% input$dnp, 1, 0))
  #   }
  # })
  # 
  # ## display stats ---------------------------------------------------------
  # 
  # rowIndexGSC <- reactive({
  #   which(rv[["game_stats_calc"]][["FirstNum"]] == input$selected_player)
  # })
  # 
  # output$pts <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["PTS"]][ri], subtitle = "Points", color = "light-blue")
  # })
  # 
  # output$ft <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FT%", ma = c("FTM", "FTA"))
  #   valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  # })
  # 
  # output$fg <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FG%", ma = c("FGM", "FGA"))
  #   valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  # })
  # 
  # output$fg3 <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "3PT%", ma = c("FGM3", "FGA3"))
  #   valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  # })
  # 
  # output$ts <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "TS%")
  #   valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  # })
  # 
  # output$eff <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["EFF"]][ri], subtitle = "Efficiency", color = "light-blue")
  # })
  # 
  # output$reb <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   rvs <- rv[["game_stats_calc"]][ri,]
  #   valueBox(value = rvs[["REB"]], subtitle = paste0("Rebounds (", rvs[["DREB"]], "+", rvs[["OREB"]], ")"), color = "light-blue")
  # })
  # 
  # output$blk <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["BLK"]][ri], subtitle = "Blocks", color = "light-blue")
  # })
  # 
  # output$stl <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["STL"]][ri], subtitle = "Steals", color = "light-blue")
  # })
  # 
  # output$ast <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["AST"]][ri], subtitle = "Assists", color = "light-blue")
  # })
  # 
  # output$tov <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["TOV"]][ri], subtitle = "Turnovers", color = "light-blue")
  # })
  # 
  # output$pf <- renderValueBox({
  #   req(rv[["game_stats_calc"]])
  #   ri <- rowIndexGSC()
  #   valueBox(value = rv[["game_stats_calc"]][["PF"]][ri], subtitle = "Fouls", color = "light-blue")
  # })
  # 
  # # Stats Viewer ---------------------------------------------------------
  # 
  # observe({
  #   cond <- is.null(input$teamsTableStatsViewer_rows_selected)
  #   toggle("select_teams_row_msg", condition = cond)
  #   toggle("games_selectall", condition = !cond)
  #   toggle("games_deselectall", condition = !cond)
  #   # toggle("select_games_row_msg", condition = is.null(input$teamsTableStatsViewer_rows_selected) | is.null(input$gamesTable_rows_selected))
  # })
  # 
  # observe({
  #   cond <- !is.null(input$teamsTableStatsViewer_rows_selected) & !is.null(input$gamesTable_rows_selected)
  #   toggle("stats_type", condition = cond)
  #   toggle("stats_group_by", condition = cond)
  # })
  # 
  # output$statisticsMessage <- renderText({
  #   out <- ""
  #   if (is.null(input$teamsTableStatsViewer_rows_selected) & is.null(input$gamesTable_rows_selected)) out <- "Select row(s) in Teams and Games tables above to view statistics"
  #   if (is.null(input$gamesTable_rows_selected)) out <- "Select row(s) in Games table above to view statistics"
  #   out
  # })
  # 
  # output$teamsTableStatsViewer <- renderDT(
  #   rv[["teams"]], selection = "multiple",
  #   style = "bootstrap", rownames = FALSE,
  #   options = list(pageLength = 5, bLengthChange = FALSE, bPaginate = TRUE, searching = FALSE, scrollX = TRUE,
  #                  columnDefs = list(list(visible = FALSE, targets = 0)))) # hide TeamID column
  # 
  # proxyTeamsTableStatsViewer <- dataTableProxy("teamsTableStatsViewer")
  # 
  # observeEvent(input$teams_selectall, {
  #   proxyTeamsTableStatsViewer %>% selectRows(1:nrow(rv[["teams"]]))
  # })
  # 
  # observeEvent(input$teams_deselectall, {
  #   proxyTeamsTableStatsViewer %>% selectRows(NULL)
  # })
  # 
  # gamesDisplay <- reactive({
  #   req(input$teamsTableStatsViewer_rows_selected)
  #   ti <- rv[["teams"]]$TeamID[input$teamsTableStatsViewer_rows_selected]
  #   filter(rv[["games"]], TeamID %in% ti) %>% 
  #     mutate(Margin = TeamScore - OpponentScore)
  # })
  # 
  # output$gamesTable <- renderDT(
  #   gamesDisplay(), selection = "multiple", 
  #   style = "bootstrap", rownames = FALSE,
  #   options = list(pageLength = 5, bLengthChange = FALSE, bPaginate = TRUE, searching = FALSE, scrollX = TRUE,
  #                  columnDefs = list(list(visible = FALSE, targets = c(0,1))))) # hide GameID and TeamID columns
  # 
  # proxyGamesTable <- dataTableProxy("gamesTable")
  # 
  # observeEvent(input$games_selectall, {
  #   proxyGamesTable %>% selectRows(1:nrow(gamesDisplay()))
  # })
  # 
  # observeEvent(input$games_deselectall, {
  #   proxyGamesTable %>% selectRows(NULL)
  # })
  # 
  # playersDisplay <- reactive({
  #   req(input$teamsTableStatsViewer_rows_selected)
  #   ti <- rv[["teams"]]$TeamID[input$teamsTableStatsViewer_rows_selected]
  #   left_join(rv[["rosters"]], rv[["players"]], by = "PlayerID") %>% filter(TeamID %in% ti)
  # })
  # 
  # output$selectedPlayers <- renderUI({
  #   req(!is.null(input$gamesTable_rows_selected) & nrow(playersDisplay()) > 0 & "PlayerID" %in% input$stats_group_by) 
  #   
  #   d <- playersDisplay() %>% 
  #     select(PlayerID, FirstName, LastName) %>% 
  #     unique() %>%   # same player could be on more than one team
  #     mutate(PlayerName = first_last(FirstName, LastName)) %>% 
  #     filter(FirstName != "Opponent") %>%
  #     arrange(FirstName)
  #   
  #   picker.ids <- d[["PlayerID"]]
  #   names(picker.ids) <- d[["PlayerName"]]
  #   pickerInput("selected_players_stats", "Select players", choices = picker.ids, multiple = TRUE, selected = picker.ids,
  #               options = list(`actions-box` = TRUE))
  # })
  # 
  # gameStats <- reactive({
  #   req(input$teamsTableStatsViewer_rows_selected, input$gamesTable_rows_selected)
  #   rv[["game_stats"]] %>% 
  #     left_join(select(rv[["teams"]], TeamID, Season, Team)) %>% 
  #     left_join(select(rv[["games"]], GameID, Opponent)) %>% 
  #     left_join(select(rv[["players"]], PlayerID, FirstName)) %>%
  #     # Create Team to represent Opponent (which was recorded as the FirstName on a roster)
  #     mutate(Team = ifelse(FirstName == "Opponent", Opponent, Team),
  #            Team = paste0(Team, " (", Season, ")"))
  # })
  # 
  # gameStatsSub <- reactive({
  #   # req(input$teamsTableStatsViewer_rows_selected, input$gamesTable_rows_selected)
  #   ti <- gamesDisplay()$TeamID[input$gamesTable_rows_selected]
  #   gi <- gamesDisplay()$GameID[input$gamesTable_rows_selected]
  #   out = filter(gameStats(), TeamID %in% ti & GameID %in% gi)
  #   if ("PlayerID" %in% input$stats_group_by) out = filter(out, PlayerID %in% input$selected_players_stats)
  #   out
  # })
  # 
  # gameStatsSumm <- reactive({
  #   gs = gameStatsSub()
  #   
  #   gms <- gs %>% 
  #     group_by_at(input$stats_group_by) %>% 
  #     summarise(Games = length(unique(GameID)))
  # 
  #   d <- gs %>% 
  #     group_by_at(input$stats_group_by) %>%
  #     summarise(across(all_of(stats_cols), ~sum(., na.rm = TRUE))) %>%
  #     left_join(gms) %>% 
  #     mutate(GP = Games - DNP)
  # 
  #   if (input$stats_type == "Per game"){
  #     div = if("PlayerID" %in% input$stats_group_by) "GP" else "Games"
  #     d = mutate(d, across(all_of(stats_cols), ~round(. / .data[[div]], 2)))
  #   } 
  #   d
  # })
  # 
  # statisticsDisplay <- reactive({
  #   req(input$stats_group_by)
  #   # probably a fancier way to do this, but starting with brute force
  #   grps <- input$stats_group_by
  #   d <- gameStatsSumm() %>% 
  #     ungroup() %>% 
  #     mutate(FGM = FGM2 + FGM3,
  #            FGA = FGA2 + FGA3,
  #            `FT%` = round(FTM/FTA*100),
  #            `FG%` = round(FGM/FGA*100),
  #            `3P%` = round(FGM3/FGA3*100),
  #            `TS%` = round(true_shooting(PTS, FTA, FGA)))
  #   
  #   if ("GameID" %in% grps){
  #     d <- left_join(d, rv[["games"]])
  #   }
  #   
  #   if ("PlayerID" %in% grps){
  #     d <- d %>%
  #       left_join(rv[["players"]]) %>%
  #       mutate(Player = FirstName,
  #              EFF = round(efficiency(PTS, REB, AST, STL, BLK, FGA, FGM, FTA, FTM, TOV)))
  #   }
  #   
  #   if (all(c("Team", "GameID", "PlayerID") %in% grps)){
  #     d <- select(d, c("Team", "Date", "Opponent", "Player", "GP", stats_display_cols, "EFF"))
  #   }
  #   if (!("Team" %in% grps) && "GameID" %in% grps && "PlayerID" %in% grps){
  #     d <- select(d, c("Date", "Opponent", "Player", "GP", stats_display_cols, "EFF"))
  #   }
  #   if ("Team" %in% grps && "GameID" %in% grps && !("PlayerID" %in% grps)){
  #     d <- select(d, c("Team", "Date", "Opponent", stats_display_cols))
  #   }
  #   if (!("Team" %in% grps) && "GameID" %in% grps && !("PlayerID" %in% grps)){
  #     d <- select(d, c("Date", "Opponent", stats_display_cols))
  #   }
  # 
  #   if ("Team" %in% grps && !("GameID" %in% grps) && !("PlayerID" %in% grps)){
  #     d <- select(d, c("Team", "Games", stats_display_cols))
  #     if (input$stats_type == "Per game") d <- select(d, -Games)
  #   }
  #   if (!("Team" %in% grps) && !("GameID" %in% grps) && "PlayerID" %in% grps){
  #     d <- select(d, c("Player", "GP", stats_display_cols, "EFF"))
  #     if (input$stats_type == "Per game") d <- select(d, -GP)
  #   }
  #   if ("Team" %in% grps && !("GameID" %in% grps) && "PlayerID" %in% grps){
  #     d <- select(d, c("Team", "Player", "GP", stats_display_cols, "EFF"))
  #     if (input$stats_type == "Per game") d <- select(d, -GP)
  #   }
  #   d
  # })
  # 
  # output$statisticsTable <- renderDT(
  #   statisticsDisplay(), selection = "single",   
  #   style = "bootstrap", rownames = FALSE,
  #   options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE,
  #                  columnDefs = list(list(orderSequence = c('desc', 'asc'), targets = "_all"))))  # change table to sort descending on first click on column heading
  # 
}
