
function(input, output, session) {
  
  # Source functions ---------------------------------------------------------
  
  # Load data ---------------------------------------------------------
  
  # hinkdata is my personal dataset; not included on github
  data_fldr <- ifelse(dir.exists("hinkdata"), "hinkdata", "data")
  
  # using read.csv instead of read_csv b/c there was an issue with DT (maybe related to hiding columns; or issue was with other package? can't remember)
  # track IDs separately to avoid duplicating IDs
  teamIDs <- read.csv(file.path(data_fldr, "TeamIDs.csv"), stringsAsFactors = FALSE) 
  teams <- read.csv(file.path(data_fldr, "Teams.csv"), stringsAsFactors = FALSE)
  # track IDs separately to avoid duplicating IDs
  playerIDs <- read.csv(file.path(data_fldr, "PlayerIDs.csv"), stringsAsFactors = FALSE)
  players <- read.csv(file.path(data_fldr, "Players.csv"), stringsAsFactors = FALSE, colClasses =  c("integer", "character", "character")) # specifying class important b/c an empty column will be read as logical; caused problems with paste
  # rosters are the players on each team (players can be on more than one team)
  rosters <- read.csv(file.path(data_fldr, "Rosters.csv"), stringsAsFactors = FALSE)
  # track IDs separately to avoid duplicating IDs
  gameIDs <- read.csv(file.path(data_fldr, "GameIDs.csv"), stringsAsFactors = FALSE) 
  games <- read.csv(file.path(data_fldr, "Games.csv"), stringsAsFactors = FALSE)
  game_stats <- read.csv(file.path(data_fldr, "GameStats.csv"), stringsAsFactors = FALSE) 
  
  # Reactive values ---------------------------------------------------------
  
  # rv <- reactiveValues(teams = teams, players = players, 
  #                      rosters = rosters, roster = NULL, 
  #                      game_id = NULL, games = games,
  #                      game_stats = game_stats, game_stats_raw = NULL, game_stats_calc = NULL)
  
  rv <- reactiveValues(teams = arrange(teams, desc(Season)), players = players, 
                       rosters = rosters, roster = NULL, 
                       game_id = NULL, games = arrange(games, desc(Date)),
                       game_stats = game_stats, game_stats_raw = NULL, game_stats_calc = NULL,
                       game_log = NULL)
  
  # Teams ---------------------------------------------------------
  
  ## table ---------------------------------------------------------
  
  output$teamsTable <- renderDT(
    rv[["teams"]], selection = "single", style = "bootstrap", rownames = FALSE,
    editable = list(target = "cell", disable = list(columns = c(0))),              # disable TeamID
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE))
  
  proxyTeams <- dataTableProxy("teamsTable")
  
  ## edit cell ---------------------------------------------------------
  
  observeEvent(input$teamsTable_cell_edit, {
    info <- input$teamsTable_cell_edit
    i <- info$row
    j <- info$col + 1L  # column index offset by 1
    v <- info$value
    rv[["teams"]][i, j] <- coerceValue(v, rv[["teams"]][i, j])
    replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## delete row ---------------------------------------------------------
  
  observe({
    toggle("delete_teams_row", condition = nrow(rv[["teams"]]) > 0 & !is.null(input$teamsTable_rows_selected))
  })
  
  observeEvent(input$delete_teams_row,{
    req(input$teamsTable_rows_selected)
    i <- input$teamsTable_rows_selected
    rv[["rosters"]] <- rv[["rosters"]] %>% 
      filter(TeamID != rv[["teams"]]$TeamID[i])  # drop old roster
    rv[["players"]] <- rv[["players"]] %>% 
      filter(PlayerID %in% rv[["rosters"]][["PlayerID"]]) # drop players not on any rosters
    rv[["teams"]] <- rv[["teams"]][-i,]  # needs to come last b/c rv$rosters uses rv$teams
    replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## add row ---------------------------------------------------------
  
  observeEvent(input$add_teams_row,{
    #addRow() only works when server = FALSE
    req(rv[["teams"]])
    
    # update master list of team IDs
    tid <- nrow(teamIDs) + 1L # ID and row number are the same
    teamIDs[tid,] <<- tid
    write.csv(teamIDs, file.path(data_fldr, "TeamIDs.csv"), row.names = FALSE)
    
    # update master list of player IDs
    pid <- nrow(playerIDs) + 1L # ID and row number are the same
    playerIDs[pid,] <<- pid
    write.csv(playerIDs, file.path(data_fldr, "PlayerIDs.csv"), row.names = FALSE)
    
    # update all of the relevant tables
    ti <- nrow(rv[["teams"]]) + 1L
    rv[["teams"]][ti,] <- list(tid, "", "", "")
    ri <- nrow(rv[["rosters"]]) + 1L
    rv[["rosters"]][ri,] <- list(tid, pid, NA_integer_)
    pi <- nrow(rv[["players"]]) + 1L
    rv[["players"]][pi,] <- list(pid, "", "")
    replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  # Roster ---------------------------------------------------------
  
  ## table ---------------------------------------------------------
  
  output$rosterTable <- renderDT(
    rv[["roster"]], selection = "single", style = "bootstrap", rownames = FALSE,
    editable = list(target = "cell", disable = list(columns = c(0, 1))),        # disable PlayerID column
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = c(0)))))   # hide TeamID column
  
  proxyRoster <- dataTableProxy("rosterTable")
  
  observe({
    toggle("select_row_msg", condition = is.null(input$teamsTable_rows_selected))
  })
  
  # rv[["roster"]] is first assigned (and updated) when a row is selected in teamsTable
  observeEvent(input$teamsTable_rows_selected,{
    req(input$teamsTable_rows_selected)
    ti <- rv[["teams"]]$TeamID[input$teamsTable_rows_selected]
    rv[["roster"]] <- filter(rv[["rosters"]], TeamID == ti) %>% 
      left_join(rv[["players"]], by = "PlayerID")
    replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## dynamic UI ---------------------------------------------------------
  
  observe({
    req(rv[["roster"]])
    toggle("rosterTable", condition = !is.null(input$teamsTable_rows_selected))
    toggle("add_roster_row", condition = !is.null(input$teamsTable_rows_selected))
    toggle("delete_roster_row", condition = nrow(rv[["roster"]]) > 0 & !is.null(input$rosterTable_rows_selected))
  })
  
  ## edit cell ---------------------------------------------------------
  
  observeEvent(input$rosterTable_cell_edit, {
    info <- input$rosterTable_cell_edit
    i <- info$row
    j <- info$col + 1L  # column index offset by 1
    v <- info$value
    
    # get IDs for row where change was made
    tid <- rv[["roster"]][["TeamID"]][i]
    pid <- rv[["roster"]][["PlayerID"]][i]
    
    # find indices
    ri <- which(rv[["rosters"]][["TeamID"]] == tid & rv[["rosters"]][["PlayerID"]] == pid)
    pi <- which(rv[["players"]][["PlayerID"]] == pid)
    
    # find colunm name
    cn <- names(rv[["roster"]])[j]
    
    # update values
    if (cn == "Number"){ 
      rv[["rosters"]][ri, cn] <- coerceValue(v, rv[["rosters"]][ri, cn])
    }else{
      rv[["players"]][pi, cn] <- coerceValue(v, rv[["players"]][pi, cn])
    }
    
    rv[["roster"]] <- rv[["rosters"]] %>% # rebuild rv$roster
      filter(TeamID == tid) %>% 
      left_join(rv[["players"]], by = "PlayerID")
    
    replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## add row ---------------------------------------------------------
  
  observeEvent(input$add_roster_row,{
    #addRow() only works when server = FALSE
    req(rv[["roster"]])
    
    # update master list of player IDs
    pid <- nrow(playerIDs) + 1L # ID and row number are the same
    playerIDs[pid,] <<- pid
    write.csv(playerIDs, file.path(data_fldr, "PlayerIDs.csv"), row.names = FALSE)
    
    tid <- rv[["roster"]]$TeamID[1] # all rows in rv[["roster"]] have same TeamID
    ri <- nrow(rv[["rosters"]]) + 1L
    rv[["rosters"]][ri,] <- list(tid, pid, NA_integer_)
    pi <- nrow(rv[["players"]]) + 1L
    rv[["players"]][pi,] <- list(pid, "", "")
    
    rv[["roster"]] <- rv[["rosters"]] %>% # rebuild rv$roster; another heavy handed approach
      filter(TeamID == tid) %>% 
      left_join(rv[["players"]], by = "PlayerID")
    replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## delete row ---------------------------------------------------------
  
  observeEvent(input$delete_roster_row,{
    req(input$rosterTable_rows_selected)
    i <- input$rosterTable_rows_selected
    rv[["roster"]] <- rv[["roster"]][-i,]
    rv[["rosters"]] <- rv[["rosters"]] %>% 
      filter(TeamID != rv[["roster"]]$TeamID[1]) %>%                # drop old roster from table of all rosters (heavy handed approach)
      bind_rows(select(rv[["roster"]], TeamID, PlayerID, Number))   # replace it with appropriate columns from new roster
    
    rv[["players"]] <- rv[["players"]] %>% 
      filter(PlayerID %in% rv[["rosters"]][["PlayerID"]]) # drop players not on any rosters
    replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## select from previous entered players ---------------------------------------------------------
  
  output$previousPlayers <- renderUI({
    req(rv[["roster"]], rv[["players"]])
    
    sel_ids <- rv[["roster"]][["PlayerID"]]
    all_ids <- rv[["players"]][["PlayerID"]]
    ids <- all_ids[!(all_ids %in% sel_ids)] # find PlayerIDs that haven't been added to roster
    
    req(length(ids) > 0) # at least one player that could be selected
    
    d <- rv[["players"]] %>% 
      filter(PlayerID %in% ids) %>%
      mutate(PlayerName = first_last(FirstName, LastName)) %>% 
      arrange(FirstName)
    
    picker.ids <- d[["PlayerID"]]
    names(picker.ids) <- d[["PlayerName"]]
    pickerInput("selected_players", "Select previous players", 
                choices = picker.ids, multiple = TRUE,
                options = list(`live-search` = TRUE))
  })
  
  observe({
    toggle("add_selected_players", condition = !is.null(input$selected_players))
  })
  
  observeEvent(input$add_selected_players,{
    req(rv[["roster"]], rv[["players"]]) # probably not necessary b/c handled upstream
    
    tid <- rv[["roster"]]$TeamID[1] # all rows in rv[["roster"]] have same TeamID
    
    rv[["rosters"]] <- bind_rows(rv[["rosters"]],
                                 data.frame(TeamID = tid, 
                                            PlayerID = as.integer(input$selected_players), 
                                            Number = NA_integer_,
                                            stringsAsFactors = FALSE))
    
    rv[["roster"]] <- rv[["rosters"]] %>% # rebuild rv$roster; another heavy handed approach
      filter(TeamID == tid) %>% 
      left_join(rv[["players"]], by = "PlayerID")
    replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  ## show/hide save button ---------------------------------------------------------
  
  observe({
    input$save_teams_roster_changes # take dependency on save button to hide button after saving
    cond <- (isTRUE(all.equal(players, rv[["players"]])) | isTRUE(all.equal(rosters, rv[["rosters"]])) | isTRUE(all.equal(teams, rv[["teams"]])))
    toggle("set_roster", condition = cond & !is.null(input$teamsTable_rows_selected))
    # toggle("exportRosters", condition = cond)
    toggle("save_teams_roster_changes", condition = !isTRUE(all.equal(players, rv[["players"]])) | !isTRUE(all.equal(rosters, rv[["rosters"]])) | !isTRUE(all.equal(teams, rv[["teams"]])))
  })
  
  observeEvent(input$save_teams_roster_changes,{
    # write teams, rosters, & players from memory to disk
    write.csv(rv[["teams"]], file.path(data_fldr, "Teams.csv"), row.names = FALSE)
    write.csv(rv[["rosters"]], file.path(data_fldr, "Rosters.csv"), row.names = FALSE)
    write.csv(rv[["players"]], file.path(data_fldr, "Players.csv"), row.names = FALSE)
    # update non-reactive versions to keep track of changes
    teams <<- rv[["teams"]]
    rosters <<- rv[["rosters"]]
    players <<- rv[["players"]]
  })
  
  ## export rosters ---------------------------------------------------------
  
  # rosterFile <- reactive({
  #   left_join(rv[["teams"]], rv[["rosters"]], by = "TeamID")
  # })
  # 
  # output$exportRosters <- downloadHandler(
  #   filename = function() { "Rosters.csv"},
  #   content = function(file) {
  #     write.csv(rosterFile(), file, row.names = FALSE)
  #   }
  # )
  
  # Scorekeeper ---------------------------------------------------------
  
  ## set roster ---------------------------------------------------------
  
  observeEvent(input$tabs, {
    if (input$tabs == "scorekeeper" & input$set_roster == 0){
      sendSweetAlert(
        session = session,
        title = "Warning: Need to set roster before scoring a game",
        text = NULL,
        type = "warning"
      )
    }
  })
  
  observeEvent(input$set_roster,{
    # update master list of game IDs; always starting a new game when setting a roster (unless testing app; other times?)
    gid <- nrow(gameIDs) + 1L # ID and row number are the same
    gameIDs[gid,] <<- gid
    rv[["game_id"]] <- gid
    write.csv(gameIDs, file.path(data_fldr, "GameIDs.csv"), row.names = FALSE)
    
    # setting roster initializes a new game
    rv[["game_stats_raw"]] <- rv[["roster"]] %>% 
      mutate(GameID = gid,
             NumFirst = num_first(Number, FirstName),
             FTM = 0L,
             FTA = 0L,
             FGM = 0L,
             FGA = 0L,
             FGM3 = 0L,
             FGA3 = 0L,
             TOV = 0L,
             STL = 0L,
             DREB = 0L,
             OREB = 0L,
             BLK = 0L,
             AST = 0L,
             PF = 0L)
    updateTabItems(session, "tabs", "scorekeeper") # move to scorekeeper tab after setting roster
  })
  
  ## game info ---------------------------------------------------------
  
  gameInfo <- reactive({
    data.frame(GameID = rv[["game_id"]], 
               TeamID = rv[["roster"]]$TeamID[1], # same TeamID for all rows in roster 
               Date = as.character(input$game_date), 
               Opponent = input$opponent_name, 
               TeamScore = as.numeric(input$team_score), 
               OpponentScore = as.numeric(input$opp_score),
               stringsAsFactors = FALSE)
  })
  
  gameInfoText <- reactive({
    c(paste0("GameID: ", rv[["game_id"]], "; TeamID: ", rv[["roster"]]$TeamID[1], 
             "; Date: ", as.character(input$game_date), "; Opponent: ", input$opponent_name),
      "---------------------------------------------------------------------")
  })
  
  observe({
    req(rv[["game_id"]])
    input$save_game_info # take dependency on save button
    toggle("save_game_info", condition = !isTRUE(all.equal(filter(rv[["games"]], GameID == rv[["game_id"]]),
                                                           gameInfo())))
  })
  
  observeEvent(input$save_game_info,{
    rv[["games"]] <- filter(rv[["games"]], GameID != rv[["game_id"]]) # drop old game info
    tid <- rv[["roster"]]$TeamID[1] # same TeamID for all rows in roster
    gi <- nrow(rv[["games"]]) + 1L
    rv[["games"]][gi,] <- gameInfo()
    write.csv(rv[["games"]], file.path(data_fldr, "Games.csv"), row.names = FALSE) # overwrites previous file
    # if game info changes, then top part of game log also changed (so rewriting whole file)
    cat(c(gameInfoText(), rv[["game_log"]]), file = file.path("gamelogs", paste0("GameID_", rv[["game_id"]], ".txt")), sep = "\n")
  })
  
  
  ## player info ---------------------------------------------------------
  
  numFirst <- reactive({
    req(rv[["roster"]], input$set_roster > 0)
    d <- rv[["roster"]] %>% 
      mutate(NumFirst = num_first(Number, FirstName))
    unique(d$NumFirst)
  })
  
  output$selectedPlayer <- renderUI({
    radioGroupButtons(inputId = "selected_player", choices = numFirst(), size = "normal", 
                      justified = TRUE, direction = "vertical")
  })
  
  output$DNP <- renderUI({
    # did not play
    pickerInput(inputId = "dnp", label = "Did Not Play (DNP)", choices = numFirst(), multiple = TRUE)
  })
  
  ## save game stats ---------------------------------------------------------
  
  observe({
    toggle("undo", condition = !is.null(rv[["game_stats_raw"]]))
  })
  
  gameStatsCalcSelected <- reactive({
    # this was how I initially solved this problem (i.e., after deciding not to save calculated columns)
    # wouldn't have to drop as many columns from rv[["game_stats_raw"]] but that dataframe doesn't include DNP column
    select(rv[["game_stats_calc"]], -FirstName, -LastName, -Number, -NumFirst, -`FT%`, -`FG%`, -`3PT%`, -`TS%`, -EFF)  # drop columns contained in other tables (e.g., names and numbers) and non-counting stats, i.e., stats that can't be summarized with sum
  })
  
  observe({
    req(rv[["game_stats_calc"]])
    toggle("save_game_stats", condition = !isTRUE(all.equal(gameStatsCalcSelected(), filter(rv[["game_stats"]], GameID == rv[["game_id"]]))))
  })
  
  observeEvent(input$save_game_stats,{
    rv[["game_stats"]] <- filter(rv[["game_stats"]], GameID != rv[["game_id"]])  # drop old game stats
    rv[["game_stats"]] <- bind_rows(rv[["game_stats"]], gameStatsCalcSelected()) # add new game stats
    write.csv(rv[["game_stats"]], file.path(data_fldr, "GameStats.csv"), row.names = FALSE)
    # if game stats change, then game log also changed (not accounting for situation where game log is changing separately from the stats)
    cat(c(gameInfoText(), rv[["game_log"]]), file = file.path("gamelogs", paste0("GameID_", rv[["game_id"]], ".txt")), sep = "\n")
  })
  
  ## update stats ---------------------------------------------------------
  
  # values are incremented or decremented based on whether undo is switched on
  change <- reactive({
    ifelse(input$undo, -1L, 1L)
  })
  
  rowIndex <- reactive({
    which(rv[["game_stats_raw"]]$NumFirst == input$selected_player)
  })
  
  output$gameLogLastTitle <- renderText({
    out = ""
    if (!is.null(rv[["game_log"]])) out = "Last game log entry:"
    out
  })
  
  output$gameLogLastText <- renderText({
    req(rv[["game_log"]])
    rv[["game_log"]][length(rv[["game_log"]])]
  })
  
  ## tried this approach but yielded "Error in : promise already under evaluation: recursive default argument reference or earlier problems?"
  ## leads to redundant code below
  # updateGameStats <- function(data = rv[["game_stats_raw"]], ri = rowIndex(), change = change(), columns){
  #   for (i in columns){
  #     ci = which(names(data) == i)
  #     data[ri,ci] = data[ri,ci] + change
  #   }
  #   return(data)
  # }
  
  ### free throws ---------------------------------------------------------
  
  observeEvent(input$miss_1,{
    # need the redundancy of calling log_action in every observeEvent so that the logged action is not too reactive (e.g., changing with undo or player)
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed free throw by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "FTA")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$make_1,{ 
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made free throw by ", input$selected_player))
    columns <- c("FTM", "FTA")
    ri <- rowIndex()
    for (i in columns){
      ci <- which(names(rv[["game_stats_raw"]]) == i)
      rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
    }
  })
  
  ### field goals ---------------------------------------------------------
  
  observeEvent(input$miss_2,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed field goal by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "FGA")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$make_2,{ 
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made field goal by ", input$selected_player))
    columns <- c("FGM", "FGA")
    ri <- rowIndex()
    for (i in columns){
      ci <- which(names(rv[["game_stats_raw"]]) == i)
      rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
    }
  })
  
  ### three pointers ---------------------------------------------------------
  
  observeEvent(input$miss_3,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Missed 3-pt field goal by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "FGA3")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$make_3,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Made 3-pt field goal by ", input$selected_player))
    columns <- c("FGM3", "FGA3")
    ri <- rowIndex()
    for (i in columns){
      ci <- which(names(rv[["game_stats_raw"]]) == i)
      rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
    }
  })
  
  ### steals and turnovers ---------------------------------------------------------
  
  observeEvent(input$stl,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Steal by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "STL")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$tov,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Turnover by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "TOV")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  ### rebounds ---------------------------------------------------------
  
  observeEvent(input$dreb,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Defensive rebound by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "DREB")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$oreb,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Offensive rebound by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "OREB")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  ### blocks and assists ---------------------------------------------------------
  
  observeEvent(input$blk,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Block by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "BLK")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  observeEvent(input$ast,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Assist by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "AST")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  ### fouls ---------------------------------------------------------
  
  observeEvent(input$pf,{
    rv[["game_log"]] <- c(rv[["game_log"]], log_action(input$undo, "Foul by ", input$selected_player))
    ri <- rowIndex()
    ci <- which(names(rv[["game_stats_raw"]]) == "PF")
    rv[["game_stats_raw"]][ri,ci] <- rv[["game_stats_raw"]][ri,ci] + change()
  })
  
  ## calculate stats ---------------------------------------------------------
  
  # watch for changes to rv[["game_stats_raw"]] (and input$dnp) to update calculated columns
  observe({
    req(rv[["game_stats_raw"]])
    temp <- rv[["game_stats_raw"]] %>% 
      mutate(PTS = FTM + FGM * 2 + FGM3 * 3,
             REB = OREB + DREB,
             `FT%` = round(FTM/FTA*100),
             `FG%` = round(FGM/FGA*100),
             `3PT%` = round(FGM3/FGA3*100),
             `TS%` = round(PTS/(0.88 * FTA + 2 * (FGA + FGA3))*100),
             EFF = (PTS + REB + AST + STL + BLK - (FGA + FGA3 - FGM - FGM3) - (FTA - FTM) - TOV))
    if (is.null(input$dnp)){  # if dnp is null, then no players selected
      rv[["game_stats_calc"]] <- temp %>% mutate(DNP = 0)
    }else{
      rv[["game_stats_calc"]] <- temp %>% 
        mutate(DNP = ifelse(NumFirst %in% input$dnp, 1, 0))
    }
  })
  
  ## display stats ---------------------------------------------------------
  
  rowIndexGSC <- reactive({
    which(rv[["game_stats_calc"]][["NumFirst"]] == input$selected_player)
  })
  
  output$pts <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["PTS"]][ri], subtitle = "Points", color = "light-blue")
  })
  
  output$ft <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FT%", ma = c("FTM", "FTA"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$fg <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FG%", ma = c("FGM", "FGA"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$fg3 <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "3PT%", ma = c("FGM3", "FGA3"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$ts <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out <- display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "TS%")
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$eff <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["EFF"]][ri], subtitle = "Efficiency", color = "light-blue")
  })
  
  output$reb <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    rvs <- rv[["game_stats_calc"]][ri,]
    valueBox(value = rvs[["REB"]], subtitle = paste0("Rebounds (", rvs[["DREB"]], "+", rvs[["OREB"]], ")"), color = "light-blue")
  })
  
  output$blk <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["BLK"]][ri], subtitle = "Blocks", color = "light-blue")
  })
  
  output$stl <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["STL"]][ri], subtitle = "Steals", color = "light-blue")
  })
  
  output$ast <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["AST"]][ri], subtitle = "Assists", color = "light-blue")
  })
  
  output$tov <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["TOV"]][ri], subtitle = "Turnovers", color = "light-blue")
  })
  
  output$pf <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri <- rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["PF"]][ri], subtitle = "Fouls", color = "light-blue")
  })
  
  # Stats Viewer ---------------------------------------------------------
  
  observe({
    cond <- is.null(input$teamsTableStatsViewer_rows_selected)
    toggle("select_teams_row_msg", condition = cond)
    toggle("games_selectall", condition = !cond)
    toggle("games_deselectall", condition = !cond)
    # toggle("select_games_row_msg", condition = is.null(input$teamsTableStatsViewer_rows_selected) | is.null(input$gamesTable_rows_selected))
  })
  
  observe({
    cond <- !is.null(input$teamsTableStatsViewer_rows_selected) & !is.null(input$gamesTable_rows_selected)
    toggle("stats_type", condition = cond)
    toggle("stats_group_by", condition = cond)
  })
  
  output$statisticsMessage <- renderText({
    out <- ""
    if (is.null(input$teamsTableStatsViewer_rows_selected) & is.null(input$gamesTable_rows_selected)) out <- "Select row(s) in Teams and Games tables above to view statistics"
    if (is.null(input$gamesTable_rows_selected)) out <- "Select row(s) in Games table above to view statistics"
    out
  })
  
  output$teamsTableStatsViewer <- renderDT(
    rv[["teams"]], selection = "multiple",
    style = "bootstrap", rownames = FALSE,
    options = list(pageLength = 5, bLengthChange = FALSE, bPaginate = TRUE, searching = FALSE, scrollX = TRUE,
                   columnDefs = list(list(visible = FALSE, targets = 0)))) # hide TeamID column
  
  proxyTeamsTableStatsViewer <- dataTableProxy("teamsTableStatsViewer")
  
  observeEvent(input$teams_selectall, {
    proxyTeamsTableStatsViewer %>% selectRows(1:nrow(rv[["teams"]]))
  })
  
  observeEvent(input$teams_deselectall, {
    proxyTeamsTableStatsViewer %>% selectRows(NULL)
  })
  
  gamesDisplay <- reactive({
    req(input$teamsTableStatsViewer_rows_selected)
    ti <- rv[["teams"]]$TeamID[input$teamsTableStatsViewer_rows_selected]
    filter(rv[["games"]], TeamID %in% ti) %>% 
      mutate(Margin = TeamScore - OpponentScore)
  })
  
  output$gamesTable <- renderDT(
    gamesDisplay(), selection = "multiple", 
    style = "bootstrap", rownames = FALSE,
    options = list(pageLength = 5, bLengthChange = FALSE, bPaginate = TRUE, searching = FALSE, scrollX = TRUE,
                   columnDefs = list(list(visible = FALSE, targets = c(0,1))))) # hide GameID and TeamID columns
  
  proxyGamesTable <- dataTableProxy("gamesTable")
  
  observeEvent(input$games_selectall, {
    proxyGamesTable %>% selectRows(1:nrow(gamesDisplay()))
  })
  
  observeEvent(input$games_deselectall, {
    proxyGamesTable %>% selectRows(NULL)
  })
  
  playersDisplay <- reactive({
    req(input$teamsTableStatsViewer_rows_selected)
    ti <- rv[["teams"]]$TeamID[input$teamsTableStatsViewer_rows_selected]
    left_join(rv[["rosters"]], rv[["players"]], by = "PlayerID") %>% filter(TeamID %in% ti)
  })
  
  output$selectedPlayers <- renderUI({
    req(!is.null(input$gamesTable_rows_selected) & nrow(playersDisplay()) > 0 & "PlayerID" %in% input$stats_group_by) 
    
    d <- playersDisplay() %>% 
      select(PlayerID, FirstName, LastName) %>% 
      unique() %>%   # same player could be on more than one team
      mutate(PlayerName = first_last(FirstName, LastName)) %>% 
      arrange(FirstName)
    
    picker.ids <- d[["PlayerID"]]
    names(picker.ids) <- d[["PlayerName"]]
    pickerInput("selected_players_stats", "Select players", choices = picker.ids, multiple = TRUE, selected = picker.ids,
                options = list(`actions-box` = TRUE))
  })
  
  statistics <- reactive({
    req(input$teamsTableStatsViewer_rows_selected, input$gamesTable_rows_selected)
    ti <- gamesDisplay()$TeamID[input$gamesTable_rows_selected]
    gi <- gamesDisplay()$GameID[input$gamesTable_rows_selected]
    gs_sub <- rv[["game_stats"]] %>% 
      filter(TeamID %in% ti & GameID %in% gi & PlayerID %in% input$selected_players_stats)
    
    gms <- gs_sub %>% 
      group_by_at(input$stats_group_by) %>% 
      summarise(Games = length(unique(GameID)))
    d <- gs_sub %>% 
      filter(TeamID %in% ti & GameID %in% gi & PlayerID %in% input$selected_players_stats) %>% 
      mutate(PTS = FTM + FGM * 2 + FGM3 * 3,
             FGM23 = FGM + FGM3,
             FGA23 = FGA + FGA3) %>% 
      group_by_at(input$stats_group_by) %>% 
      summarise_all(sum, na.rm = TRUE) %>% 
      left_join(gms, by = input$stats_group_by) %>% 
      mutate(GP = Games - DNP)
    if (input$stats_type == "Per game"){
      # not pretty but works correctly
      if("PlayerID" %in% input$stats_group_by){
        d <- mutate_at(d, vars(-all_of(input$stats_group_by)), 
                       list(~round(. / GP, 2)))
      }else{
        d <- mutate_at(d, vars(-all_of(input$stats_group_by)), 
                       list(~round(. / Games, 2)))
      }
    } 
    d
  })
  
  statisticsDisplay <- reactive({
    req(input$stats_group_by)
    # probably a fancier way to do this, but starting with brute force
    grps <- input$stats_group_by
    d <- statistics() %>% 
      ungroup() %>% 
      select(-stats_group_by_opts[!(stats_group_by_opts %in% grps)]) %>% # drop unselected group by columns
      mutate(PTS = FTM + FGM * 2 + FGM3 * 3,
             FGM23 = FGM + FGM3,
             FGA23 = FGA + FGA3,
             `FT%` = round(FTM/FTA*100),
             `FG%` = round(FGM23/FGA23*100),
             `3P%` = round(FGM3/FGA3*100),
             `TS%` = round(PTS/(0.88 * FTA + 2 * (FGA + FGA3))*100))
    if ("TeamID" %in% grps){
      d <- d %>% 
        left_join(rv[["teams"]]) %>% 
        mutate(Team = paste0(Season, " (", Coach, ")"))
    }
    if ("GameID" %in% grps){
      d <- d %>% 
        left_join(rv[["games"]])
    }
    if ("PlayerID" %in% grps){
      d <- d %>% 
        left_join(rv[["players"]]) %>% 
        mutate(Player = FirstName,
               EFF = round((PTS + REB + AST + STL + BLK - (FGA + FGA3 - FGM - FGM3) - (FTA - FTM) - TOV), 2))
    }
    
    if (all(c("TeamID", "GameID", "PlayerID") %in% grps)){
      d <- select(d, Team, Date, Opponent, Player, GP, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF, EFF)
    }
    if ("TeamID" %in% grps && "GameID" %in% grps && !("PlayerID" %in% grps)){
      d <- select(d, Team, Date, Opponent, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF)
    }
    if (!("TeamID" %in% grps) && "GameID" %in% grps && !("PlayerID" %in% grps)){
      d <- select(d, Date, Opponent, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF)
    }
    if ("TeamID" %in% grps && !("GameID" %in% grps) && !("PlayerID" %in% grps)){
      d <- select(d, Team, Games, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF)
      if (input$stats_type == "Per game") d <- select(d, -Games)
    }
    if (!("TeamID" %in% grps) && !("GameID" %in% grps) && "PlayerID" %in% grps){
      d <- select(d, Player, GP, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF, EFF)
      if (input$stats_type == "Per game") d <- select(d, -GP)
    }
    if ("TeamID" %in% grps && !("GameID" %in% grps) && "PlayerID" %in% grps){
      d <- select(d, Team, Player, GP, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF, EFF)
      if (input$stats_type == "Per game") d <- select(d, -GP)
    }
    if (!("TeamID" %in% grps) && "GameID" %in% grps && "PlayerID" %in% grps){
      d <- select(d, Date, Opponent, Player, GP, PTS, FGM = FGM23, FGA = FGA23, `FG%`, `3PM` = FGM3, `3PA` = FGA3, `3P%`, FTM, FTA, `FT%`, `TS%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF, EFF)
      if (input$stats_type == "Per game") d <- select(d, -GP)
    }
    d
  })
  
  output$statisticsTable <- renderDT(
    statisticsDisplay(), selection = "single",   
    style = "bootstrap", rownames = FALSE,
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE,
                   columnDefs = list(list(orderSequence = c('desc', 'asc'), targets = "_all"))))  # change table to sort descending on first click on column heading
  
}
