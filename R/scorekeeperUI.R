
scorekeeperSB <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(ns("game_date"), "Date"),
    textInput(ns("opponent"), label = "Opponent", width = "100%"),
    h5(align = "center", "Final Score"),
    layout_columns(
      col_widths = c(6, 6),
      textInput(ns("team_score"), label = NULL, placeholder = "Team", width = "60%"),
      textInput(ns("opp_score"), label = NULL, placeholder = "Opp", width = "60%")
    ),
    br(),
    p(align = "center", actionButton(ns("save_game_stats"), "Save game stats", 
                                     icon = icon("save"), disabled = TRUE))
  )
}

scorekeeperUI <- function(id){
  ns <- NS(id)
  tagList(
    h4(textOutput(ns("rosterWarning")), style="color:red", align = "center"),
    layout_columns(
      col_widths = c(2, 4, 6),
      div(
        radioButtons(ns("selected_player_id"), label = "Select Player", choices = c("")),
        br(),
        pickerInput(ns("dnp"), label = "Did Not Play (DNP)", choices = c(""),
                    multiple = TRUE, width = "100%", 
                    options = pickerOptions(size = 7, `live-search` = TRUE))
      ),
      div(
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
          col_widths = c(6, 2, 2, 2),
          actionButton(ns("pf"), "Foul"),
          p(),
          input_switch(ns("undo"), label = "UNDO"),
          p()
        ),
        br(),
        h5(align = "center", textOutput(ns("gameLogLastTitle"))),
        h6(align = "center", textOutput(ns("gameLogLastText")))
      ),
      div(
        layout_column_wrap(
          value_box(
            title = "Points", 
            max_height = mxht,
            value = textOutput(ns("pts"))),
          value_box(
            title = "Free Throws", 
            max_height = mxht,
            value = textOutput(ns("ftPer")), 
            textOutput(ns("ftTally"))
          ),
          value_box(
            title = "Field Goals", 
            max_height = mxht,
            value = textOutput(ns("fgPer")), 
            textOutput(ns("fgTally"))
          ),
          value_box(
            title = "3PT Field Goals", 
            max_height = mxht,
            value = textOutput(ns("threePtPer")), 
            textOutput(ns("threePtTally"))
          ),
          value_box(
            title = "True Shooting",
            max_height = mxht,
            value = textOutput(ns("ts"))
          ),
          value_box(
            title = "Efficiency", 
            max_height = mxht,
            value = textOutput(ns("eff"))
          ),
          value_box(
            title = "Rebounds", 
            max_height = mxht,
            value = textOutput(ns("rebTotal")), 
            textOutput(ns("rebTally"))
          ),
          value_box(
            title = "Blocks",
            max_height = mxht,
            value = textOutput(ns("blk"))
          ),
          value_box(
            title = "Steals",
            max_height = mxht,
            value = textOutput(ns("stl"))
          ),
          value_box(
            title = "Assists", 
            max_height = mxht,
            value = textOutput(ns("ast"))
          ),
          value_box(
            title = "Turnovers",
            max_height = mxht,
            value = textOutput(ns("tov"))
          ),
          value_box(
            title = "Fouls",
            max_height = mxht,
            value = textOutput(ns("pf"))
          )
        )
      )
    )
  )
}

