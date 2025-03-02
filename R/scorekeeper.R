
# Sidebar -----------------------------------------------------------------

scorekeeperSB <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(ns("game_date"), "Date"),
    textInput(ns("opponent"), label = "Opponent", width = "100%"),
    h5(align = "center", "Final Score"),
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("team_score"), label = NULL, placeholder = "Team"),
      textInput(ns("opp_score"), label = NULL, placeholder = "Opp")
    ),
    br(),
    p(align = "center", actionButton(ns("save_game_info"), "Save game info", 
                                     icon = icon("save"), disabled = TRUE))
  )
}

# UI ----------------------------------------------------------------------

scorekeeperUI <- function(id){
  ns <- NS(id)
  tagList(
    h4(textOutput(ns("rosterWarning")), style="color:red", align = "center"),
    layout_columns(
      col_widths = c(3, 6, 3),
      div(
        br(),
        radioButtons(ns("selected_player_id"), label = NULL,
                     choices = c(""),  width = "100%"),
        pickerInput(ns("dnp"), label = "Did Not Play (DNP)", choices = c(""),
                    multiple = TRUE, width = "100%")
      ),
      card(
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton(ns("miss_1"), "Miss"),
          p(align = "center", "Free Throw"),
          actionButton(ns("make_1"), "Make")
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton(ns("miss_2"), "Miss"),
          p(align = "center", "Field Goal"),
          actionButton(ns("make_2"), "Make")
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton(ns("miss_3"), "Miss"),
          p(align = "center", "Three Point"),
          actionButton(ns("make_3"), "Make")
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton(ns("tov"), "Turnover"),
          actionButton(ns("stl"), "Steal")
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton(ns("dreb"), "Def. Rebound"),
          actionButton(ns("oreb"), "Off. Rebound")
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton(ns("blk"), "Block"),
          actionButton(ns("ast"), "Assist")
        ),
        layout_columns(
          col_widths = c(3, 6, 3),
          p(),
          actionButton(ns("pf"), "Foul"),
          p()
        )
      ),
      div(
        br(),
        actionButton(ns("save_game_stats"), "Save game stats", icon = icon("save"), 
                     width = "100%", disabled = TRUE),
        br(),
        br(),
        br(),
        switchInput(ns("undo"), label = "UNDO", size = "large"),
        br(),
        br(),
        h5(textOutput(ns("gameLogLastTitle"))),
        textOutput(ns("gameLogLastText"))
      )
    ),
    layout_columns(
      value_box(
        title = "Points", 
        value = textOutput(ns("pts"))),
      value_box(
        title = "Free Throws", 
        value = textOutput(ns("ftPer")), 
        textOutput(ns("ftTally"))
      ),
      value_box(
        title = "Field Goals", 
        value = textOutput(ns("fgPer")), 
        textOutput(ns("fgTally"))
      ),
      value_box(
        title = "3PT Field Goals", 
        value = textOutput(ns("threePtPer")), 
        textOutput(ns("threePtTally"))
      ),
      value_box(
        title = "True Shooting",
        value = textOutput(ns("ts"))
      ),
      value_box(
        title = "Efficiency", 
        value = textOutput(ns("eff"))
      )
    ),
    layout_columns(
      value_box(
        title = "Rebounds", 
        value = textOutput(ns("rebTotal")), 
        textOutput(ns("rebTally"))
      ),
      value_box(
        title = "Blocks",
        value = textOutput(ns("blk"))
      ),
      value_box(
        title = "Steals",
        value = textOutput(ns("stl"))
      ),
      value_box(
        title = "Assists", 
        value = textOutput(ns("ast"))
      ),
      value_box(
        title = "Turnovers",
        value = textOutput(ns("tov"))
      ),
      value_box(
        title = "Fouls",
        value = textOutput(ns("pf"))
      )
    )
  )
}

# Server ------------------------------------------------------------------

scorekeeperServer <- function(id, roster_data){
  moduleServer(id, function(input, output, session) {
    
    output$rosterWarning <- renderText({
      if (!is.null(rv[["roster"]])) NULL else "Warning: Need to set roster before scoring a game"
    })
    
    gameDate <- reactive({
      as.character(input$game_date)
    })
    
    rv <- reactiveValues(roster = NULL, team_id = NULL, game_id = ids::random_id(), 
                         player_names = NULL, games = NULL, game_stats = NULL,
                         game_log = NULL)
    
    observeEvent(roster_data(),{
      rv[["roster"]] = create_player_names(roster_data())
      rv[["team_id"]] = rv[["roster"]]$TeamID[1] # same TeamID for all rows in roster_data()
      rv[["game_stats"]] = add_game_stats(game_stats, rv[["roster"]]$PlayerID, rv[["game_id"]])
    })
    
    gameStats <- reactive({
      # only tallied game stats are stored on disk
      # this reactive recalculates game stats every time rv[["game_stats"]] is update
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
    
    observe({
      updateRadioButtons(session, "selected_player_id", choices = nameNum())
      updatePickerInput(session, "dnp", choices = nameNum())
    })
    
    nameNumSel <- reactive({
      names(nameNum())[nameNum() == input$selected_player_id]
    })
    
    observe({
      req(rv[["game_id"]], rv[["team_id"]])
      input$save_game_info
      # update rv[["games"]] any time game info changes 
      rv[["games"]] = update_games_row(games, rv[["team_id"]], rv[["game_id"]], gameDate(), 
                                       input$opponent, input$team_score, input$opp_score)
      save_state = isTRUE(all.equal(games, rv[["games"]]))
      updateActionButton(session, "save_game_info", disabled = save_state)
    })
    
    observeEvent(input$save_game_info,{
      # write games from memory to disk
      write.csv(rv[["games"]], file.path(data_dir, "Games.csv"), row.names = FALSE)
      # update non-reactive versions to keep track of changes
      games <<- rv[["games"]]
      # if game info changes, then game log header also changed (so rewriting whole file)
      cat(c(gameLogHeader(), rv[["game_log"]]), file = gameLogPath(), sep = "\n") 
    })
    
    observe({
      req(rv[["game_id"]], rv[["team_id"]])
      input$save_game_stats
      save_state = isTRUE(all.equal(game_stats, rv[["game_stats"]]))
      updateActionButton(session, "save_game_stats", disabled = save_state)
    })
    
    observeEvent(input$save_game_stats,{
      write.csv(rv[["game_stats"]], file.path(data_dir, "GameStats.csv"), row.names = FALSE)
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
      gs = gameStats()
      gs[gs[["PlayerID"]] == input$selected_player_id & gs[["GameID"]] == rv[["game_id"]], ]
    })
    
    output$pts <- renderText({ gsSub()[["PTS"]] })
    
    output$ftPer <- renderText({ 
      per = gsSub()[["FT%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$ftTally <- renderText({ 
      paste0(gsSub()[["FTM"]], "/", gsSub()[["FTA"]]) 
    })
    
    output$fgPer <- renderText({ 
      per = gsSub()[["FG%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$fgTally <- renderText({ 
      paste0(gsSub()[["FGM"]], "/", gsSub()[["FGA"]]) 
    })
    
    output$threePtPer <- renderText({ 
      per = gsSub()[["3PT%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$threePtTally <- renderText({ 
      paste0(gsSub()[["FGM3"]], "/", gsSub()[["FGA3"]]) 
    })
    
    output$ts <- renderText({ 
      per = gsSub()[["TS%"]] 
      if (is.na(per)) "--" else paste0(per, "%")
    })
    
    output$eff <- renderText({ 
      gsSub()[["EFF"]]
    })
    
    output$rebTotal <- renderText({ 
      gsSub()[["REB"]] 
    })
    
    output$rebTally <- renderText({ 
      paste0(gsSub()[["DREB"]], " + ", gsSub()[["OREB"]]) 
    })
    
    output$blk <- renderText({ 
      gsSub()[["BLK"]] 
    })
    
    output$stl <- renderText({ 
      gsSub()[["STL"]] 
    })
    
    output$ast <- renderText({ 
      gsSub()[["AST"]] 
    })
    
    output$tov <- renderText({ 
      gsSub()[["TOV"]] 
    })
    
    output$pf <- renderText({ 
      gsSub()[["PF"]] 
    })
    
  })
}

