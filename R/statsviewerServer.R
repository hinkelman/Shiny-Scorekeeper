

statsviewerServer <- function(id, roster_out, scorekeeper_out){
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    observe({
      dfx = roster_out()[["teams"]]
      opts = sort(unique(dfx$League))
      updatePickerInput(session, "leagues", choices = opts, selected = opts)
    })
    
    teamsSub1 <- reactive({
      dfx = roster_out()[["teams"]]
      dfx[dfx[["League"]] %in% input$leagues, ]
    })
    
    observe({
      dfx = teamsSub1()
      opts = sort(unique(dfx$Team))
      updatePickerInput(session, "teams", choices = opts, selected = opts)
    })
    
    teamsSub2 <- reactive({
      dfx = teamsSub1()
      dfx[dfx[["Team"]] %in% input$teams, ]
    })
    
    observe({
      dfx = teamsSub2()
      opts = sort(unique(dfx$Season))
      updatePickerInput(session, "seasons", choices = opts, selected = opts)
    })
    
    teamsSub3 <- reactive({
      dfx = teamsSub2()
      dfx[dfx[["Season"]] %in% input$seasons, ]
    })
    
    gamesSub1 <- reactive({
      dfx = scorekeeper_out()[["games"]]
      dfx = dfx[dfx$TeamID %in% teamsSub3()$TeamID, ]
      mutate(dfx, Margin = TeamScore - OpponentScore)
    })
    
    observe({
      req(nrow(gamesSub1()) > 0)
      mn = min(gamesSub1()$Margin, na.rm = TRUE)
      mx = max(gamesSub1()$Margin, na.rm = TRUE)
      updateSliderInput(session, "margin", min = mn, max = mx, value = c(mn, mx))
    })
    
    gamesSub2 <- reactive({
      dfx = gamesSub1()
      dfx[dfx$Margin >= input$margin[1] & dfx$Margin <= input$margin[2], ]
    })
    
    observe({
      dfx = gamesSub2()
      opts = sort(unique(dfx$Opponent))
      updatePickerInput(session, "opponents", choices = opts, selected = opts)
    })
    
    gamesSub3 <- reactive({
      dfx = gamesSub2()
      dfx[dfx$Opponent %in% input$opponents, ]
    })
    
    observe({
      dfx = gamesSub3()
      opts = sort(unique(dfx$Date))
      updatePickerInput(session, "dates", choices = opts, selected = opts)
    })
    
    gamesSub4 <- reactive({
      dfx = gamesSub3()
      dfx[dfx$Date %in% input$dates, ]
    })
    
    gameStatsSub <- reactive({
      scorekeeper_out()[["game_stats"]] |> 
        filter(GameID %in% gamesSub4()$GameID) |>
        left_join(gamesSub4(), by = join_by(GameID)) |> 
        left_join(teamsSub3(), by = join_by(TeamID)) |>
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
        out[which(out$FirstName == "Opponent"), c("Team", "Opponent")] = 
          rev(out[which(out$FirstName == "Opponent"), c("Team", "Opponent")])
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
