
function(input, output, session) {
  
  rosterData <- rosterServer("roster")
  
  observeEvent(input[["roster-set_roster"]], {
    updateTabsetPanel(session, "nav", "Scorekeeper")
  })
  
  scorekeeperServer("scorekeeper", rosterData)
  
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
