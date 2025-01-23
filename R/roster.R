
# Sidebar -----------------------------------------------------------------

rosterSB <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("save_teams_roster_changes"), "Save changes", icon = icon("save"), disabled = TRUE),
    actionButton(ns("set_roster"), "Set roster", icon = icon("edit"), disabled = TRUE)
  )
}

# UI ----------------------------------------------------------------------

rosterUI <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("teamsTable")),
    br(),
    layout_column_wrap(
      actionButton(ns("add_teams_row"), "Add row", icon = icon("plus-square")),
      actionButton(ns("delete_teams_row"), "Delete row", icon = icon("trash"), disabled = TRUE)
    ),
    hr(),
    DTOutput(ns("rosterView")),
    br(),
    uiOutput(ns("rosterAddDelete")),
    uiOutput(ns("previousPlayers"))
  )
}

# Server ------------------------------------------------------------------

rosterServer <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(teams = teams, players = players, rosters = rosters,
                         roster = NULL)
    
    # Teams Table -------------------------------------------------------------
    
    output$teamsTable <- renderDT({
      rv[["teams"]]
    }, style = "default", rownames = FALSE, 
    selection = "single", editable = list(target = "cell"),              
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = c("TeamID")))))
    
    proxyTeams <- dataTableProxy("teamsTable")
    
    observe({
      req(nrow(rv[["teams"]]) > 0)
      state = if (is.null(input$teamsTable_rows_selected)) TRUE else FALSE
      updateActionButton(session, "delete_teams_row", disabled = state)
      updateActionButton(session, "add_roster_row", disabled = state)
    })
    
    observeEvent(input$delete_teams_row,{
      req(input$teamsTable_rows_selected)
      tmp = delete_teams_row(rv[["teams"]], 
                             input$teamsTable_rows_selected,
                             rv[["players"]],
                             rv[["rosters"]])
      rv[["teams"]] <- tmp$teams_table
      rv[["players"]] <- tmp$players_table
      rv[["rosters"]] <- tmp$rosters_table
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$add_teams_row,{
      rv[["teams"]] <- add_teams_row(rv[["teams"]])
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$teamsTable_cell_edit, {
      info <- input$teamsTable_cell_edit
      rv[["teams"]] <- edit_teams_row(rv[["teams"]], info$row, info$col + 1L, info$value)
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # Roster View -------------------------------------------------------------
    
    output$rosterView <- renderDT({
      req(nrow(rv[["teams"]]) > 0, input$teamsTable_rows_selected)
      rv[["roster"]]
    }, style = "default", rownames = FALSE, 
    selection = "single", editable = list(target = "cell"),                 
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = c("TeamID", "PlayerID")))))
    
    proxyRoster <- dataTableProxy("rosterView")
    
    # roster is first assigned (and updated) when a row is selected in teamsTable
    observeEvent(input$teamsTable_rows_selected,{
      req(nrow(rv[["teams"]]) > 0, input$teamsTable_rows_selected)
      team_id <- rv[["teams"]]$TeamID[input$teamsTable_rows_selected]
      rv[["roster"]] <- create_roster_view(team_id, rv[["players"]], rv[["rosters"]])
      replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  
    })
    
    output$rosterAddDelete <- renderUI({
      req(!is.null(input$teamsTable_rows_selected)) 
      layout_column_wrap(
        actionButton(ns("add_roster_row"), "Add row", icon = icon("plus-square")),
        actionButton(ns("delete_roster_row"), "Delete row", icon = icon("trash"), disabled = TRUE)
      )
    })
    
    observe({
      req(nrow(rv[["roster"]]) > 0)
      state = if (is.null(input$rosterView_rows_selected)) TRUE else FALSE
      updateActionButton(session, "delete_roster_row", disabled = state)
    })
    
    observeEvent(input$delete_roster_row,{
      req(input$rosterView_rows_selected)
      tmp = delete_roster_row(rv[["roster"]], 
                              input$rosterView_rows_selected,
                              rv[["players"]],
                              rv[["rosters"]])
      rv[["players"]] <- tmp$players_table
      rv[["rosters"]] <- tmp$rosters_table
      rv[["roster"]] <- tmp$roster_view
      replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$add_roster_row,{
      team_id = rv[["teams"]]$TeamID[input$teamsTable_rows_selected]
      tmp <- add_roster_row(team_id, rv[["players"]], rv[["rosters"]])
      rv[["players"]] <- tmp$players_table
      rv[["rosters"]] <- tmp$rosters_table
      rv[["roster"]] <- tmp$roster_view
      replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$rosterView_cell_edit, {
      info <- input$rosterView_cell_edit
      tmp = edit_roster_row(rv[["roster"]], info$row, info$col + 1L, info$value,
                            rv[["players"]], rv[["rosters"]])
      rv[["players"]] <- tmp$players_table
      rv[["rosters"]] <- tmp$rosters_table
      rv[["roster"]] <- tmp$roster_view
      replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    output$previousPlayers <- renderUI({
      req(rv[["roster"]], rv[["players"]])
      
      rv_ids <- rv[["roster"]][["PlayerID"]]
      all_ids <- rv[["players"]][["PlayerID"]]
      # find PlayerIDs that haven't been added to roster
      ids <- all_ids[!(all_ids %in% rv_ids)] 
      
      req(!is.null(input$teamsTable_rows_selected) & length(ids) > 0) 
      
      d <- rv[["players"]] |> 
        filter(PlayerID %in% ids) |> 
        mutate(PlayerName = case_when(
          is.na(FirstName) & is.na(LastName) ~ NA_character_,
          is.na(FirstName) ~ LastName,
          is.na(LastName) ~ FirstName,
          .default = paste(FirstName, LastName))) |> 
        filter(!is.na(PlayerName)) |> 
        arrange(FirstName)
      
      picker_ids <- setNames(d[["PlayerID"]], d[["PlayerName"]])
      
      layout_column_wrap(
        pickerInput(ns("selected_players"), "Select players",
                    choices = picker_ids, multiple = TRUE, 
                    options = list(`live-search` = TRUE)),
        actionButton(ns("add_selected_players"), "Add selected players", 
                     style = "margin-top: 32px;", icon = icon("plus-square"))
      )
    })
    
    
    observeEvent(input$add_selected_players,{
      req(!is.null(input$teamsTable_rows_selected))
      
      team_id <- rv[["teams"]]$TeamID[input$teamsTable_rows_selected]
      
      rv[["rosters"]] <- bind_rows(rv[["rosters"]],
                                   data.frame(TeamID = team_id,
                                              PlayerID = input$selected_players,
                                              Number = ""))
      
      rv[["roster"]] <- create_roster_view(team_id, rv[["players"]], rv[["rosters"]])
      replaceData(proxyRoster, rv[["roster"]], resetPaging = FALSE, rownames = FALSE)  
    })
    
    observe({
      req(nrow(rv[["roster"]]) > 0)
      state = if (is.null(input$rosterView_rows_selected)) TRUE else FALSE
      updateActionButton(session, "delete_roster_row", disabled = state)
    })
    
    observe({
      input$save_teams_roster_changes # take dependency on save button to disable button after saving
      save_state = (isTRUE(all.equal(teams, rv[["teams"]])) & 
                      isTRUE(all.equal(players, rv[["players"]])) & 
                      isTRUE(all.equal(rosters, rv[["rosters"]])))
      updateActionButton(session, "save_teams_roster_changes", disabled = save_state)
      
      updateActionButton(session, "set_roster", 
                         disabled = (!save_state | 
                                       is.null(input$teamsTable_rows_selected) |
                                       nrow(rv[["roster"]]) == 0))
    })
    
    observeEvent(input$save_teams_roster_changes,{
      # write teams, rosters, & players from memory to disk
      write.csv(rv[["teams"]], file.path(data_fldr, "Teams.csv"), row.names = FALSE)
      write.csv(rv[["players"]], file.path(data_fldr, "Players.csv"), row.names = FALSE)
      write.csv(rv[["rosters"]], file.path(data_fldr, "Rosters.csv"), row.names = FALSE)
      # update non-reactive versions to keep track of changes
      teams <<- rv[["teams"]]
      players <<- rv[["players"]]
      rosters <<- rv[["rosters"]]
    })
    
    
  })
}


