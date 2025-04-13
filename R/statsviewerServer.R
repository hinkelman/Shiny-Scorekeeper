

statsviewerServer <- function(id, roster_out, scorekeeper_out){
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    observe({
      dfx = roster_out()[["teams"]]
      opts = sort(unique(dfx$League))
      freezeReactiveValue(input, "leagues")
      updatePickerInput(session, "leagues", choices = opts, selected = opts)
    })
    
    observe({
      dfx = filter(roster_out()[["teams"]], League %in% input$leagues)
      opts = sort(unique(dfx$Team))
      freezeReactiveValue(input, "teams")
      updatePickerInput(session, "teams", choices = opts, selected = opts)
    })
    
    observe({
      dfx = filter(roster_out()[["teams"]], League %in% input$leagues & Team %in% input$teams)
      opts = sort(unique(dfx$Season))
      freezeReactiveValue(input, "seasons")
      updatePickerInput(session, "seasons", choices = opts, selected = opts)
    })
    
    teamsSub <- reactive({
      roster_out()[["teams"]] |> 
        filter(League %in% input$leagues & Team %in% input$teams & Season %in% input$seasons)
    })
    
    gamesSubTmp <- reactive({
      scorekeeper_out()[["games"]] |>
        filter(TeamID %in% teamsSub()$TeamID) |>
        mutate(Margin = TeamScore - OpponentScore)
    })
    
    observe({
      req(nrow(gamesSubTmp()) > 0)
      mn = min(gamesSubTmp()$Margin, na.rm = TRUE)
      mx = max(gamesSubTmp()$Margin, na.rm = TRUE)
      freezeReactiveValue(input, "margin")
      updateSliderInput(session, "margin", min = mn, max = mx, value = c(mn, mx))
    })
    
    observe({
      dfx = filter(gamesSubTmp(), Margin >= input$margin[1] & Margin <= input$margin[2])
      opts = sort(unique(dfx$Opponent))
      freezeReactiveValue(input, "opponents")
      updatePickerInput(session, "opponents", choices = opts, selected = opts)
    })
    
    observe({
      dfx = gamesSubTmp() |> 
        filter(Margin >= input$margin[1] & Margin <= input$margin[2] & Opponent %in% input$opponents)
      opts = sort(unique(dfx$Date))
      freezeReactiveValue(input, "dates")
      updatePickerInput(session, "dates", choices = opts, selected = opts)
    })
    
    gamesSub<- reactive({
      gamesSubTmp() |>
        filter(Margin >= input$margin[1] & Margin <= input$margin[2] & 
                 Opponent %in% input$opponents & Date %in% input$dates)
    })
    
    gameStatsSub <- reactive({
      scorekeeper_out()[["game_stats"]] |> 
        filter(GameID %in% gamesSub()$GameID) |>
        left_join(gamesSub(), by = join_by(GameID)) |> 
        left_join(teamsSub(), by = join_by(TeamID)) |>
        left_join(roster_out()[["players"]], by = join_by(PlayerID))
    })
    
    output$selectedPlayers <- renderUI({
      req("PlayerID" %in% input$group_by, nrow(gameStatsSub()) > 0)
      
      dfx = gameStatsSub() |> 
        filter(FirstName != "Opponent") |> 
        mutate(Name = create_player_name(FirstName, LastName)) |> 
        select(PlayerID, Name) |> 
        distinct() |> 
        arrange(Name)
      
      opts = setNames(dfx$PlayerID, dfx$Name) 
      
      pickerInput(ns("selected_players"), "Select players", multiple = TRUE, width = "100%", 
                  choices = opts, selected = opts, 
                  options = pickerOptions(size = 7, `live-search` = TRUE, `actions-box` = TRUE))
    })
    
    gameStatsDisplay <- reactive({
      req(nrow(gameStatsSub()) > 0)
      
      out = gameStatsSub()
      gb = input$group_by
      
      if ("PlayerID" %in% input$group_by){
        req(input$selected_players)
        gb = c(gb, "Name")
        out = out |> 
          filter(PlayerID %in% input$selected_players) |> 
          mutate(Name = create_player_name(FirstName, LastName))
      } 
      
      if ("TeamID" %in% input$group_by) {
        gb = c(gb, "Team")
        # Create Team to represent Opponent (which was recorded as the FirstName on a roster)
        out = mutate(out, Team = ifelse(FirstName == "Opponent", Opponent, Team)) 
      }
      
      if ("GameID" %in% input$group_by) {
        gb = c(gb, "Date", "Opponent")
      }
      
      out = out |> 
        group_by(across(all_of(gb))) |> 
        summarise(across(all_of(c("DNP", events)), ~sum(.x, na.rm = TRUE)),
                  Games = length(unique(GameID))) |> 
        mutate(GP = Games - DNP) 
      
      if (input$stats_type == "Per game"){
        div = if("PlayerID" %in% input$stats_group_by) "GP" else "Games"
        out = mutate(out, across(all_of(events), ~round(.x / .data[[div]], 2)))
      }
      
      out = calc_game_stats(ungroup(out))
      
      if (all(c("Team", "GameID", "PlayerID") %in% gb)){
        out = select(out, all_of(c("Team", "Date", "Opponent", "Name", "GP", stats_display_cols, "EFF")))
      }
      if (!("Team" %in% gb) && "GameID" %in% gb && "PlayerID" %in% gb){
        out = select(out, all_of(c("Date", "Opponent", "Name", "GP", stats_display_cols, "EFF")))
      }
      if ("Team" %in% gb && "GameID" %in% gb && !("PlayerID" %in% gb)){
        out = select(out, all_of(c("Team", "Date", "Opponent", stats_display_cols)))
      }
      if (!("Team" %in% gb) && "GameID" %in% gb && !("PlayerID" %in% gb)){
        out = select(out, all_of(c("Date", "Opponent", stats_display_cols)))
      }
      if ("Team" %in% gb && !("GameID" %in% gb) && !("PlayerID" %in% gb)){
        out = select(out, all_of(c("Team", "Games", stats_display_cols)))
        if (input$stats_type == "Per game") out = select(out, -Games)
      }
      if (!("Team" %in% gb) && !("GameID" %in% gb) && "PlayerID" %in% gb){
        out = select(out, all_of(c("Name", "GP", stats_display_cols, "EFF")))
        if (input$stats_type == "Per game") out = select(out, -GP)
      }
      if ("Team" %in% gb && !("GameID" %in% gb) && "PlayerID" %in% gb){
        out = select(out, all_of(c("Team", "Name", "GP", stats_display_cols, "EFF")))
        if (input$stats_type == "Per game") out = select(out, -GP)
      }
      out
    })
    
    output$table <- renderReactable({
      req(nrow(gameStatsDisplay()) > 0)
      
      dfx = gameStatsDisplay()
      types = sapply(dfx, class)
      chars = sapply(dfx, function(x) max(nchar(x), na.rm = TRUE))
      
      col_widths <- function(type, chars){
        colDef(minWidth = if (type %in% c("integer", "numeric")) 70 else chars*12 + 16)
      }
      
      reactable(dfx,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  defaultSortOrder = "desc",
                  headerStyle = list(background = "#f7f7f8")),
                columns = mapply(function(ty, ch) col_widths(ty, ch),
                                 types, chars, SIMPLIFY = FALSE),
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 15, 20),
                defaultPageSize = 15)
    })
    
  })
}
