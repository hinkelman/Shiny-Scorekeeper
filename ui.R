page_navbar(
  title = "Shiny Scorekeeper",
  window_title = "Scorekeeper",
  theme = bs_theme(preset = "bootstrap"),
  fillable = FALSE,
  id = "nav",
  sidebar = sidebar(
    conditionalPanel(
      condition = 'input.nav == "Roster"',
      rosterSB("roster")
    ),
    conditionalPanel(
      condition = 'input.nav == "Scorekeeper"',
      scorekeeperSB("scorekeeper")
    )
  ),
  nav_spacer(),
  nav_panel("Roster",
            rosterUI("roster")),
  nav_panel("Scorekeeper",
            scorekeeperUI("scorekeeper")),
  nav_panel("Stats Viewer"),
  nav_menu(title = "About", align = "right",
           nav_panel("Instructions",
                     includeCSS("custom-css.css"),
                     card(
                       includeMarkdown(file.path("markdown", "instructions.md"))
                     )
           ),
           nav_panel("Background",
                     card(
                       includeMarkdown(file.path("markdown", "background.md"))
                     )
           )
  )
)

#       ),
#       # Stats Viewer -----------------------------------------------------------------
#       tabItem(tabName = "stats_viewer",
#               fluidRow(
#                 column(width = 5,
#                        h3("Teams"),
#                        actionButton('teams_selectall', 'Select all'),
#                        actionButton('teams_deselectall', 'Deselect all'),
#                        DTOutput("teamsTableStatsViewer")
#                 ),
#                 column(width = 7,
#                        h3("Games"),
#                        hidden(actionButton('games_selectall', 'Select all')),
#                        hidden(actionButton('games_deselectall', 'Deselect all')),
#                        h4(id = "select_teams_row_msg", "Select row(s) in Teams table on left to view games"),
#                        DTOutput("gamesTable")
#                 )
#               ),
#               br(),
#               h3("Statistics"),
#               h4(textOutput("statisticsMessage")),
#               hidden(pickerInput("stats_group_by", "Group by", choices = stats_group_by_opts, 
#                                  multiple = TRUE, selected = stats_group_by_opts)),
#               uiOutput("selectedPlayers"),
#               hidden(radioGroupButtons(inputId = "stats_type", label = NULL, choices = c("Per game", "Total"))),    
#               DTOutput("statisticsTable"),
#               br()
#       )
#     )
#   )
# )
