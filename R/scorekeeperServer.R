
scorekeeperServer <- function(id, roster_out){
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    gameDate <- reactive({
      as.character(input$game_date)
    })
    
    rv <- reactiveValues(roster = NULL, team_id = NULL, game_id = ids::random_id(), 
                         player_names = NULL, games = games, game_stats = game_stats,
                         game_log = NULL)
    
    observe({
      req(roster_out()[["roster"]])
      rv[["roster"]] = create_player_names(roster_out()[["roster"]])
      rv[["team_id"]] = rv[["roster"]]$TeamID[1] # same TeamID for all rows in roster_out()[["roster"]]
      rv[["game_stats"]] = add_game_stats(game_stats, rv[["roster"]]$PlayerID, rv[["game_id"]])
    })
    
    gameStats <- reactive({
      # only tallied game stats are stored on disk
      # this reactive recalculates game stats every time rv[["game_stats"]] is updated
      # not efficient (but sufficient?)
      calc_game_stats(rv[["game_stats"]])
    })
    
    gameLogHeader <- reactive({
      create_log_header(rv[["team_id"]], rv[["game_id"]], gameDate(), input$opponent)
    })
    
    gameLogPath <- reactive({
      file.path("data", "gamelogs", paste0(gameDate(), "_GameID_", rv[["game_id"]], ".txt"))
    })
    
    nameNum <- reactive({
      req(rv[["roster"]])
      setNames(rv[["roster"]]$PlayerID, rv[["roster"]]$NameNum)
    })
    
    output$selectedPlayer <- renderUI({
      validate(need(!is.null(rv[["roster"]]), "Need to set roster before scoring a game"))
      tagList(
        radioButtons(ns("selected_player_id"), label = "Select Player", choices = nameNum()),
        br(),
        pickerInput(ns("dnp"), label = "Did Not Play (DNP)", choices = nameNum(),
                    multiple = TRUE, width = "100%", 
                    options = pickerOptions(size = 7, `live-search` = TRUE))
      )
    })
    
    nameNumSel <- reactive({
      names(nameNum())[nameNum() == input$selected_player_id]
    })
    
    observe({
      req(rv[["game_id"]], rv[["team_id"]])
      input$save_game_stats
      # update rv[["games"]] any time game info changes 
      rv[["games"]] = update_games_row(rv[["games"]], rv[["team_id"]], rv[["game_id"]], gameDate(), 
                                       input$opponent, input$team_score, input$opp_score)
      save_state = isTRUE(all.equal(games, rv[["games"]])) & isTRUE(all.equal(game_stats, rv[["game_stats"]]))
      updateActionButton(session, "save_game_stats", disabled = save_state)
    })
    
    observeEvent(input$save_game_stats,{
      # write games from memory to disk
      write.csv(rv[["games"]], file.path(data_dir, "Games.csv"), row.names = FALSE)
      write.csv(rv[["game_stats"]], file.path(data_dir, "GameStats.csv"), row.names = FALSE)
      # update non-reactive versions to keep track of changes
      games <<- rv[["games"]]
      game_stats <<- rv[["game_stats"]]
      cat(c(gameLogHeader(), rv[["game_log"]]), file = gameLogPath(), sep = "\n") 
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
    
    ## update stats ---------------------------------------------------------
    
    ### DNP -----------------------------------------------------------------
    
    observe({
      rv[["game_stats"]] <- update_dnp(rv[["game_stats"]], input$dnp, rv[["game_id"]])
    })
    
    ### free throws ---------------------------------------------------------
    
    observeEvent(input$miss_1,{
      # need the redundancy of calling log_action in every observeEvent so that the logged action is not too reactive (e.g., changing with undo or player)
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FTA", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "FTA", input$undo)
    })
    
    observeEvent(input$make_1,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FTM", input$undo)
      for (stat in c("FTM", "FTA")){
        rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                               rv[["game_id"]], stat, input$undo)
      }
    })
    
    ### field goals ---------------------------------------------------------
    
    observeEvent(input$miss_2,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FGA2", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "FGA2", input$undo)
    })
    
    observeEvent(input$make_2,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FGM2", input$undo)
      for (stat in c("FGM2", "FGA2")){
        rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                               rv[["game_id"]], stat, input$undo)
      }
    })
    
    ### three pointers ---------------------------------------------------------
    
    observeEvent(input$miss_3,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FGA3", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "FGA3", input$undo)
    })
    
    observeEvent(input$make_3,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "FGM3", input$undo)
      for (stat in c("FGM3", "FGA3")){
        rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                               rv[["game_id"]], stat, input$undo)
      }
    })
    
    ### steals and turnovers ---------------------------------------------------------
    
    observeEvent(input$stl,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "STL", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "STL", input$undo)
    })
    
    observeEvent(input$tov,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "TOV", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "TOV", input$undo)
    })
    
    ### rebounds ---------------------------------------------------------
    
    observeEvent(input$dreb,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "DREB", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "DREB", input$undo)
    })
    
    observeEvent(input$oreb,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "OREB", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "OREB", input$undo)
    })
    
    ### blocks and assists ---------------------------------------------------------
    
    observeEvent(input$blk,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "BLK", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "BLK", input$undo)
    })
    
    observeEvent(input$ast,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "AST", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "AST", input$undo)
    })
    
    ### fouls ---------------------------------------------------------
    
    observeEvent(input$pf,{
      rv[["game_log"]] <- add_log_entry(rv[["game_log"]], nameNumSel(), "PF", input$undo)
      rv[["game_stats"]] <- update_game_stat(rv[["game_stats"]], input$selected_player_id, 
                                             rv[["game_id"]], "PF", input$undo)
    })
    
    ## update stats ---------------------------------------------------------
    
    gsSub <- reactive({
      req(rv[["roster"]], input$selected_player_id)
      gs = gameStats()
      gs[gs[["PlayerID"]] == input$selected_player_id & gs[["GameID"]] == rv[["game_id"]], ]
    })
    
    output$pts <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["PTS"]] 
    })
    
    output$ftPer <- renderText({ 
      req(rv[["roster"]])
      per = gsSub()[["FT%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$ftTally <- renderText({ 
      req(rv[["roster"]])
      paste0(gsSub()[["FTM"]], "/", gsSub()[["FTA"]]) 
    })
    
    output$fgPer <- renderText({ 
      req(rv[["roster"]])
      per = gsSub()[["FG%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$fgTally <- renderText({ 
      req(rv[["roster"]])
      paste0(gsSub()[["FGM"]], "/", gsSub()[["FGA"]]) 
    })
    
    output$threePtPer <- renderText({ 
      req(rv[["roster"]])
      per = gsSub()[["3PT%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$threePtTally <- renderText({ 
      req(rv[["roster"]])
      paste0(gsSub()[["FGM3"]], "/", gsSub()[["FGA3"]]) 
    })
    
    output$ts <- renderText({ 
      req(rv[["roster"]])
      per = gsSub()[["TS%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$eff <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["EFF"]]
    })
    
    output$rebTotal <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["REB"]] 
    })
    
    output$rebTally <- renderText({ 
      req(rv[["roster"]])
      paste0(gsSub()[["DREB"]], " + ", gsSub()[["OREB"]]) 
    })
    
    output$blk <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["BLK"]] 
    })
    
    output$stl <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["STL"]] 
    })
    
    output$ast <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["AST"]] 
    })
    
    output$tov <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["TOV"]] 
    })
    
    output$pf <- renderText({ 
      req(rv[["roster"]])
      gsSub()[["PF"]] 
    })
    
    # return teams and roster
    reactive(list("roster" = rv[["roster"]],
                  "games" = rv[["games"]], 
                  "game_stats" = rv[["game_stats"]]))
  })
}

