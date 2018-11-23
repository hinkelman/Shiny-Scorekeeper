library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyTime)
library(DT)

dashboardPage(
  dashboardHeader(
    title = "Shiny Scorekeeper"),
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Game Info", tabName = "game_info", icon = icon("calendar")),
      menuItem("Roster", tabName = "roster", icon = icon("list")),
      menuItem("Scorekeeper", tabName = "scorekeeper", icon = icon("basketball-ball")),
      menuItem("Stats Viewer", tabName = "stats_viewer", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useSweetAlert(),
    useShinyjs(),  # Include shinyjs
    tabItems(
      # Game Info -----------------------------------------------------------------
      tabItem(tabName = "game_info",
              fluidRow(
                column(
                  width = 7,
                  fluidRow(
                    column(width = 5, dateInput("game_date", "Date", width = "100%")),
                    column(width = 5, timeInput("game_time", "Start time", seconds = FALSE)),
                    column(width = 2)
                  ),
                  fluidRow(
                    column(width = 10, textInput("season", "Season", placeholder = "2018-19 or Spring 2019", width = "100%")),
                    column(width = 2)
                  ),
                  fluidRow(
                    column(width = 7, textInput("team_name", "Team", placeholder = "Name", width = "100%")),
                    column(width = 3, textInput("team_score", label = "Score", placeholder = "48")),
                    column(width = 2)
                  ),
                  fluidRow(
                    column(width = 7, textInput("opponent_name", "Opponent", placeholder = "Name", width = "100%")),
                    column(width = 3, textInput("opponent_score", label = "Score", placeholder = "39")),
                    column(width = 2)
                  )
                ),
                column(width = 5)
              )
      ),
      # Roster -----------------------------------------------------------------
      tabItem(tabName = "roster",
              actionButton('add_row', 'Add row'),
              fluidRow(column(12, h1('Server-side processing'), hr(), DTOutput("x2", width = "50%"))),
              fluidRow(
                column(
                  width = 7,
                  radioGroupButtons(inputId = "roster_type", label = NULL, justified = TRUE,
                                    choices = c("Enter roster", "Upload roster")),
                  conditionalPanel(
                    condition = 'input.roster_type == "Enter roster"',
                    fluidRow(
                      column(width = 9,
                             textInput(inputId = "name_1", label = NULL, value = "", placeholder = "Name"),
                             tags$div(id = "player_names")),
                      column(width = 3,
                             textInput(inputId = "num_1", label = NULL, value = "", placeholder = "00"),
                             tags$div(id = "player_nums"))
                    ),
                    fluidRow(
                      column(width = 6, actionButton("add_btn", "Add row", width = "100%")),
                      column(width = 6, actionButton("remove_btn", "Remove row", width = "100%"))
                    ),
                    br()
                  ),
                  conditionalPanel(
                    condition = 'input.roster_type == "Upload roster"',
                    fileInput("roster_upload", label = NULL, width = "100%", 
                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    tableOutput("rosterTable")
                  ),
                  p(align = "center", hidden(actionButton("set_roster", "Set roster", icon = icon("cogs")))),
                  conditionalPanel(
                    condition = 'input.roster_type == "Enter roster"',
                    p(align = "center", hidden(downloadButton("downloadRoster", "Download roster")))
                  )
                ),
                column(width = 5)
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
                       p(align = "center", hidden(downloadButton(outputId = "downloadGameFile", label = "Download game file"))),
                       br(),
                       br(),
                       br(),
                       br(),
                       hidden(switchInput("undo", label = "UNDO", size = "large", width = "100%"))
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
              h2("Stats Viewer tab content")
      ),
      tabItem(tabName = "about",
              h2("About tab content"),
              includeMarkdown("about.md")
      )
    )
  )
)
