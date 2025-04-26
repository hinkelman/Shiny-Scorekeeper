page_navbar(
  title = "Shiny Scorekeeper",
  window_title = "Scorekeeper",
  fillable = FALSE,
  id = "nav",
  sidebar = sidebar(
    width = 300,
    conditionalPanel(
      condition = 'input.nav == "Roster"',
      rosterSB("roster")
    ),
    conditionalPanel(
      condition = 'input.nav == "Scorekeeper"',
      scorekeeperSB("scorekeeper")
    ),
    conditionalPanel(
      condition = 'input.nav == "Stats Viewer"',
      statsviewerSB("statsviewer")
    )
  ),
  nav_spacer(),
  nav_panel("Roster",
            rosterUI("roster")),
  nav_panel("Scorekeeper",
            scorekeeperUI("scorekeeper")),
  nav_panel("Stats Viewer",
            statsviewerUI("statsviewer")),
  nav_spacer(),
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
