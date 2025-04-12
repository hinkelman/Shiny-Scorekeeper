
function(input, output, session) {
  
  rosterOut <- rosterServer("roster")
  
  observeEvent(input[["roster-set_roster"]], {
    updateTabsetPanel(session, "nav", "Scorekeeper")
  })
  
  scorekeeperOut <- scorekeeperServer("scorekeeper", rosterOut)
  
  statsviewerServer("statsviewer", rosterOut, scorekeeperOut)
  
}
