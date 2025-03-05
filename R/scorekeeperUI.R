
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

