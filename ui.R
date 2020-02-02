dashboardPage(

  dashboardHeader(title = "Shiny Scorekeeper"),
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Roster", tabName = "roster", icon = icon("list")),
      menuItem(text = "Scorekeeper", tabName = "scorekeeper", icon = icon("basketball-ball")),
      menuItem("Stats Viewer", tabName = "stats_viewer", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      conditionalPanel(
        condition = 'input.tabs == "scorekeeper"',
        br(),
        br(),
        dateInput("game_date", "Date"),
        textInput("opponent_name", "Opponent", placeholder = "Name", width = "100%"),
        fluidRow(
          tags$b(h4(align = "center", "Final score")),
          column(width = 6, textInput("team_score", label = NULL, placeholder = "Team", width = "100%")),
          column(width = 6, textInput("opp_score", label = NULL, placeholder = "Opp.", width = "100%"))
        ),
        br(),
        p(align = "center", hidden(actionButton("save_game_info", "Save game info", icon = icon("save"))))
      )
    )
  ),
  
  dashboardBody(
    tags$style(".bttn-gradient.bttn-default  {color: #5D8CB9 !important;}"),
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    useSweetAlert(),
    useShinyjs(),  # Include shinyjs
    tabItems(
      # Roster -----------------------------------------------------------------
      tabItem(tabName = "roster",
              fluidRow(
                column(
                  width = 7,
                  DTOutput("teamsTable"),
                  actionButton("add_teams_row", "Add row", icon = icon("plus-square")),
                  hidden(actionButton("delete_teams_row", "Delete selected row", icon = icon("trash"))),
                  hr(),
                  h4(id = "select_row_msg", align = "center", "Select row in table above to view and edit roster"),
                  uiOutput("previousPlayers"),
                  hidden(actionButton("add_selected_players", "Add selected players", icon = icon("plus-square"))),
                  br(),
                  hidden(DTOutput("rosterTable")),
                  hidden(actionButton("add_roster_row", "Add row", icon = icon("plus-square"))),
                  hidden(actionButton("delete_roster_row", "Delete selected row", icon = icon("trash")))
                ),
                
                column(width = 5,
                       h4("Click on table to select row"),
                       h4("Double click to edit table cell"),
                       br(),
                       hidden(actionButton("save_teams_roster_changes", "Save changes", icon = icon("save"))),
                       hidden(actionButton("set_roster", "Set roster", icon = icon("edit")))
                       # downloadButton(outputId = "exportRosters", label = "Export rosters")
                )
              )
      ),
      # Scorekeeper -----------------------------------------------------------------
      tabItem(tabName = "scorekeeper",
              fluidRow(
                column(width = 3, uiOutput("onCourt")),
                column(width = 6,
                       box(
                         width = 12,
                         fluidRow(
                           column(width = 4, actionBttn("miss_1", "Miss", style = "gradient", block = TRUE)),
                           column(width = 4, h4(align = "center", "Free Throw")),
                           column(width = 4, actionBttn("make_1", "Make", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 4, actionBttn("miss_2", "Miss", style = "gradient", block = TRUE)),
                           column(width = 4, h4(align = "center", "Field Goal")),
                           column(width = 4, actionBttn("make_2", "Make", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 4, actionBttn("miss_3", "Miss", style = "gradient", block = TRUE)),
                           column(width = 4, h4(align = "center", "Three Point")),
                           column(width = 4, actionBttn("make_3", "Make", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 6, actionBttn("tov", "Turnover", style = "gradient", block = TRUE)),
                           column(width = 6, actionBttn("stl", "Steal", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 6, actionBttn("dreb", "Def. Rebound", style = "gradient", block = TRUE)),
                           column(width = 6, actionBttn("oreb", "Off. Rebound", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 6, actionBttn("blk", "Block", style = "gradient", block = TRUE)),
                           column(width = 6, actionBttn("ast", "Assist", style = "gradient", block = TRUE))
                         ),
                         br(),
                         fluidRow(
                           column(width = 3),
                           column(width = 6, actionBttn("pf", "Foul", style = "gradient", block = TRUE)),
                           column(width = 3)
                         )
                       )
                ),
                column(width = 3, 
                       uiOutput("selectedPlayer"),
                       hidden(h4(id = "roster_msg", "Select players on court from roster on left")),
                       uiOutput("DNP"),
                       # uiOutput("DNP_CD"),
                       br(),
                       hidden(actionButton("save_game_stats", "Save game stats", icon = icon("save"), width = "100%")),
                       br(),
                       br(),
                       br(),
                       hidden(switchInput("undo", label = "UNDO", size = "large"))
                )
              ),
              fluidRow(
                valueBoxOutput("pts", width = 2),
                valueBoxOutput("ft", width = 2),
                valueBoxOutput("fg", width = 2),
                valueBoxOutput("fg3", width = 2),
                valueBoxOutput("ts", width = 2),
                valueBoxOutput("eff", width = 2)
              ),
              fluidRow(
                valueBoxOutput("reb", width = 2),
                valueBoxOutput("blk", width = 2),
                valueBoxOutput("stl", width = 2),
                valueBoxOutput("ast", width = 2),
                valueBoxOutput("tov", width = 2),
                valueBoxOutput("pf", width = 2)
              )
      ),
      # Stats Viewer -----------------------------------------------------------------
      tabItem(tabName = "stats_viewer",
              fluidRow(
                column(width = 5,
                       h3("Teams"),
                       actionButton('teams_selectall', 'Select all'),
                       actionButton('teams_deselectall', 'Deselect all'),
                       DTOutput("teamsTableStatsViewer")
                ),
                column(width = 7,
                       h3("Games"),
                       hidden(actionButton('games_selectall', 'Select all')),
                       hidden(actionButton('games_deselectall', 'Deselect all')),
                       h4(id = "select_teams_row_msg", "Select row(s) in Teams table on left to view games"),
                       DTOutput("gamesTable")
                )
              ),
              br(),
              h3("Statistics"),
              h4(textOutput("statisticsMessage")),
              hidden(pickerInput("stats_group_by", "Group by", choices = stats_group_by_opts, 
                                 multiple = TRUE, selected = stats_group_by_opts)),
              uiOutput("selectedPlayers"),
              hidden(radioGroupButtons(inputId = "stats_type", label = NULL, choices = c("Per game", "Total"))),    
              DTOutput("statisticsTable"),
              br()
      ),
      tabItem(tabName = "about",
              includeCSS("custom-css.css"),
              includeMarkdown("about.md")
      )
    )
  )
)
