
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
