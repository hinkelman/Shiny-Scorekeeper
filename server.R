library(dplyr)

function(input, output, session) {
  rstr = read.csv("RosterEmpty.csv", stringsAsFactors = FALSE)
  
  output$x2 = renderDT(rstr, selection = "multiple", editable = TRUE, style = "bootstrap", 
                       options = list(searching = FALSE,
                                      bPaginate = FALSE,
                                      # pageLength = 20, 
                                      info = FALSE
                                      ))
  
  proxy2 = dataTableProxy('x2')
  
  observeEvent(input$x2_cell_edit, {
    info = input$x2_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    rstr[i, j] <<- coerceValue(v, rstr[i, j])
    replaceData(proxy2, rstr, resetPaging = FALSE)  # important
  })
  
  create_name <- function(name, num){
    ifelse(num == "" | is.na(num), name,
           ifelse(name == "" | is.na(name),
                  paste0("#", num),
                  paste0("#", num, " - ", name)))
  }
  
  rv <- reactiveValues(name_ids = NULL, num_ids = NULL, game_stats = NULL, game_stats_calc = NULL)
  
  observeEvent(input$add_btn, {
    btn <- input$add_btn + 1 # need at least one row
    
    name_id <- paste0("name_", btn)
    num_id <- paste0("num_", btn)
    
    insertUI(
      where = "beforeEnd",
      selector = '#player_names',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        textInput(inputId = name_id, label = NULL, value = "", placeholder = "Name"),
        id = name_id
      )
    )
    
    insertUI(
      where = "beforeEnd",
      selector = '#player_nums',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        textInput(inputId = num_id, label = NULL, value = "", placeholder = "00"),
        id = num_id
      )
    )
    
    rv$name_ids = c(rv$name_ids, name_id)
    rv$num_ids = c(rv$num_ids, num_id)
  })
  
  observeEvent(input$remove_btn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', rv$name_ids[length(rv$name_ids)])
    )
    
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', rv$num_ids[length(rv$num_ids)])
    )
    
    rv$name_ids = rv$name_ids[-length(rv$name_ids)]
    rv$num_ids = rv$num_ids[-length(rv$num_ids)]
  })
  
  roster <- reactive({
    if (input$roster_type == "Upload roster"){
      if(is.null(input$roster_upload)){
        d = data.frame()
      }else{
        d = read.csv(input$roster_upload$datapath, stringsAsFactors = FALSE)
      }
    }else{
      d = data.frame()
      player_names = unlist(lapply(c("name_1", rv$name_ids), function(x) input[[x]]), use.names = FALSE)
      player_nums = unlist(lapply(c("num_1", rv$num_ids), function(x) input[[x]]), use.names = FALSE)
      
      if(length(player_names) == length(player_nums)){ # ran into issues with these values getting out of sync
        d = data.frame(Name = player_names, Num = player_nums, stringsAsFactors = FALSE)
      }
    }
    if (!is.null(d)) d = d[d$Name != "" | d$Num != "", ]
    return(d)
  })
  
  observe({
    toggle("downloadRoster", condition = nrow(roster()) > 0)
    toggle("set_roster", condition = nrow(roster()) > 0)
    toggle("undo", condition = !is.null(rv[["game_stats"]]))
    toggle("downloadGameFile", condition = !is.null(rv[["game_stats_calc"]]))
  })
  
  output$rosterTable <- renderTable({
    req(nrow(roster()) > 0)
    roster()
  })
  
  output$downloadRoster <- downloadHandler(
    filename = function() { "Roster.csv"},
    content = function(file) {
      write.csv(roster(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$set_roster,{
    rv[["game_stats"]] = roster() %>% 
      mutate(NumName = create_name(Name, Num),
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
  
  output$onCourt <- renderUI({
    req(roster())
    d <- roster() %>% 
      mutate(NumName = create_name(Name, Num))
    nn = unique(d$NumName)
    checkboxGroupButtons(inputId = "on_court", size = "normal", justified = TRUE,
                         direction = "vertical", choices = nn)
  })
  
  output$selectedPlayer <- renderUI({
    req(input$on_court)
    radioGroupButtons(inputId = "selected_player", choices = input$on_court, size = "normal", 
                      justified = TRUE, direction = "vertical")
  })
  
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
  
  change <- reactive({
    ifelse(input$undo, -1L, 1L)
  })
  
  rowIndex <- reactive({
    which(rv[["game_stats"]]$NumName == input$selected_player)
  })
  
  ## tried this more functional approach, but yielded "Error in : promise already under evaluation: recursive default argument reference or earlier problems?"
  ## leads to redundant code below
  # updateGameStats <- function(data = rv[["game_stats"]], ri = rowIndex(), change = change(), columns){
  #   for (i in columns){
  #     ci = which(names(data) == i)
  #     data[ri,ci] = data[ri,ci] + change
  #   }
  #   return(data)
  # }
  
  observeEvent(input$miss_1,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "FTA")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$make_1,{ 
    columns = c("FTM", "FTA")
    ri = rowIndex()
    for (i in columns){
      ci = which(names(rv[["game_stats"]]) == i)
      rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
    }
  })
  
  observeEvent(input$miss_2,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "FGA")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$make_2,{ 
    columns = c("FGM", "FGA")
    ri = rowIndex()
    for (i in columns){
      ci = which(names(rv[["game_stats"]]) == i)
      rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
    }
  })
  
  observeEvent(input$miss_3,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "FGA3")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$make_3,{ 
    columns = c("FGM3", "FGA3")
    ri = rowIndex()
    for (i in columns){
      ci = which(names(rv[["game_stats"]]) == i)
      rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
    }
  })
  
  observeEvent(input$tov,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "TOV")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$stl,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "STL")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$dreb,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "DREB")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$oreb,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "OREB")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$blk,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "BLK")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$ast,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "AST")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  observeEvent(input$pf,{
    ri = rowIndex()
    ci = which(names(rv[["game_stats"]]) == "PF")
    rv[["game_stats"]][ri,ci] = rv[["game_stats"]][ri,ci] + change()
  })
  
  # watch for changes to rv[["game_stats"]] to update calculated columns
  observe({
    req(rv[["game_stats"]])
    rv[["game_stats_calc"]] = rv[["game_stats"]] %>% 
      mutate(PTS = FTM + FGM * 2 + FGM3 * 3,
             REB = OREB + DREB,
             `FT%` = round(FTM/FTA*100),
             `FG%` = round(FGM/FGA*100),
             `3PT%` = round(FGM3/FGA3*100),
             `TS%` = round(PTS/(0.88 * FTA + 2 * (FGA + FGA3))*100),
             EFF = (PTS + REB + AST + STL + BLK - (FGA + FGA3 - FGM - FGM3) - (FTA - FTM) - TOV))
  })
  
  rowIndexGSC <- reactive({
    which(rv[["game_stats_calc"]][["NumName"]] == input$selected_player)
  })
  
  output$pts <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["PTS"]][ri], subtitle = "Points", color = "light-blue")
  })
  
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
  
  output$ft <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out = display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FT%", ma = c("FTM", "FTA"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$fg <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out = display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "FG%", ma = c("FGM", "FGA"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$fg3 <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out = display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "3PT%", ma = c("FGM3", "FGA3"))
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$ts <- renderValueBox({
    req(rv[["game_stats_calc"]])
    out = display_shooting(data = rv[["game_stats_calc"]], ri = rowIndexGSC(), stat = "TS%")
    valueBox(value = out[1], subtitle = out[2], color = "light-blue")
  })
  
  output$eff <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["EFF"]][ri], subtitle = "Efficiency", color = "light-blue")
  })
  
  output$reb <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    rvs = rv[["game_stats_calc"]][ri,]
    valueBox(value = rvs[["REB"]], subtitle = paste0("Rebounds (", rvs[["DREB"]], "+", rvs[["OREB"]], ")"), color = "light-blue")
  })
  
  output$blk <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["BLK"]][ri], subtitle = "Blocks", color = "light-blue")
  })
  
  output$stl <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["STL"]][ri], subtitle = "Steals", color = "light-blue")
  })
  
  output$ast <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["AST"]][ri], subtitle = "Assists", color = "light-blue")
  })
  
  output$tov <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["TOV"]][ri], subtitle = "Turnovers", color = "light-blue")
  })
  
  output$pf <- renderValueBox({
    req(rv[["game_stats_calc"]])
    ri = rowIndexGSC()
    valueBox(value = rv[["game_stats_calc"]][["PF"]][ri], subtitle = "Fouls", color = "light-blue")
  })
  
  gameTime <- reactive({
    strftime(input$game_time, format="%H:%M:%S")
  })
  
  gameFile <- reactive({
    req(rv[["game_stats_calc"]])
    rv[["game_stats_calc"]] %>% 
      mutate(Datetime = paste(input$game_date, gameTime()),
             Season = input$season,
             Team = input$team_name,
             Opponent = input$opponent_name,
             TeamScore = input$team_score,
             OpponentScore = input$opponent_score,
             Outcome = ifelse(TeamScore > OpponentScore, "Win",
                              ifelse(TeamScore < OpponentScore, "Loss"))) %>% 
      select(-NumName)
  })
  
  output$downloadGameFile <- downloadHandler(
    filename = function() { paste0(input$game_date, "_", gameTime(), "_", input$opponent_name, ".csv")},
    content = function(file) {
      write.csv(gameFile(), file, row.names = FALSE)
    }
  )
  
}