

rosterSB <- function(id){
  tagList(
    actionButton(NS(id, "save_teams_roster_changes"), "Save changes", icon = icon("save"), disabled = TRUE),
    actionButton(NS(id, "set_roster"), "Set roster", icon = icon("edit"), disabled = TRUE)
  )
}

rosterUI <- function(id){
  tagList(
    card(
      fill = FALSE,
      DTOutput(NS(id, "teamsTable")),
      layout_column_wrap(
        actionButton(NS(id, "add_teams_row"), "Add row", icon = icon("plus-square")),
        actionButton(NS(id, "delete_teams_row"), "Delete selected row", icon = icon("trash"), disabled = TRUE)
      ),
      hr(),
      p("Roster table goes here"),
      layout_column_wrap(
        actionButton("add_roster_row", "Add row", icon = icon("plus-square"), disabled = TRUE),
        actionButton("delete_roster_row", "Delete selected row", icon = icon("trash"), disabled = TRUE)
      )
    )
  )
}

rosterServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(teams = teams, players = players)
    
    output$teamsTable <- renderDT(
      rv[["teams"]], selection = "single", style = "bootstrap", rownames = FALSE,
      editable = list(target = "cell"),              
      options = list(searching = FALSE, bPaginate = FALSE, info = FALSE,
                     columnDefs = list(list(visible = FALSE, targets = 0))))
    
    proxyTeams <- dataTableProxy("teamsTable")
    
    observe({
      req(nrow(rv[["teams"]]) > 0)
      state = if (is.null(input$teamsTable_rows_selected)) TRUE else FALSE
      updateActionButton(session, "delete_teams_row", disabled = state)
    })
    
    observeEvent(input$delete_teams_row,{
      req(input$teamsTable_rows_selected)
      i <- input$teamsTable_rows_selected
      rv[["teams"]] <- rv[["teams"]][-i,] 
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$add_teams_row,{
      rv[["teams"]] <- add_teams_row(rv[["teams"]])
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE) 
    })
    
    observeEvent(input$teamsTable_cell_edit, {
      info <- input$teamsTable_cell_edit
      # column index offset by 1 b/c TeamID is hidden
      rv[["teams"]] <- edit_teams_row(rv[["teams"]], info$row, info$col + 1L, info$value)
      replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE)
    })
    
  })
}

# #     tabItems(
# #       # Roster -----------------------------------------------------------------
# #       tabItem(tabName = "roster",
# #               fluidRow(
# #                 column(
# #                   width = 7,
# #                   DTOutput("teamsTable"),
# #                   actionButton("add_teams_row", "Add row", icon = icon("plus-square")),
# #                   hidden(actionButton("delete_teams_row", "Delete selected row", icon = icon("trash"))),
# #                   hr(),
# #                   h4(id = "select_row_msg", align = "center", "Select row in table above to view and edit roster"),
# #                   uiOutput("previousPlayers"),
# #                   hidden(actionButton("add_selected_players", "Add selected players", icon = icon("plus-square"))),
# #                   br(),
# #                   hidden(DTOutput("rosterTable")),
# #                   hidden(actionButton("add_roster_row", "Add row", icon = icon("plus-square"))),
# #                   hidden(actionButton("delete_roster_row", "Delete selected row", icon = icon("trash")))
# #                 ),
# #                 
# #                 column(width = 5,
# #                        h4("Click on table to select row"),
# #                        h4("Double click to edit table cell"),
# #                        br(),
# #                        hidden(actionButton("save_teams_roster_changes", "Save changes", icon = icon("save"))),
# #                        hidden(actionButton("set_roster", "Set roster", icon = icon("edit")))
# #                        # downloadButton(outputId = "exportRosters", label = "Export rosters")
# #                 )
# #               )
# #       ),
