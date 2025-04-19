

# Sidebar -----------------------------------------------------------------

statsviewerSB <- function(id){
  ns <- NS(id)
  tagList(
    pickerInput(ns("leagues"), label = "Leagues", choices = c(""), multiple = TRUE, width = "100%", 
                options = pickerOptions(size = 7, liveSearch = TRUE, actionsBox = TRUE)),
    pickerInput(ns("teams"), label = "Teams", choices = c(""), multiple = TRUE, width = "100%", 
                options = pickerOptions(size = 7, `live-search` = TRUE, actionsBox = TRUE)),
    pickerInput(ns("seasons"), label = "Seasons", choices = c(""), multiple = TRUE, width = "100%", 
                options = pickerOptions(size = 7, `live-search` = TRUE, actionsBox = TRUE)),
    sliderInput(ns("margin"), label = "Scoring margin", min = -100, max = 100, 
                value = c(-50, 50), step = 1),
    pickerInput(ns("opponents"), label = "Opponents", choices = c(""), multiple = TRUE, width = "100%", 
                options = pickerOptions(size = 7, `live-search` = TRUE, actionsBox = TRUE)),
    pickerInput(ns("dates"), label = "Dates", choices = c(""), multiple = TRUE, width = "100%", 
                options = pickerOptions(size = 7, `live-search` = TRUE, actionsBox = TRUE))
  )
}

# UI ----------------------------------------------------------------------

statsviewerUI <- function(id){
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 2, 4),
      pickerInput(ns("group_by"), "Group by", choices = group_by_opts, 
                  multiple = TRUE, width = "100%", selected = c("TeamID", "PlayerID")),
      uiOutput(ns("selectedPlayers")),
      conditionalPanel(
        condition = 'input.group_by.indexOf("GameID") === -1',
        radioButtons(ns("stats_type"), label = "Statistics", choices = c("Per game", "Total")),
        ns = NS(id)
      ),
      p()
    ),
    reactableOutput(ns("table"))
  )
}
