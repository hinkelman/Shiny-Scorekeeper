
<a href="https://www.travishinkelman.com/project/shiny-scorekeeper/" target="_blank">Shiny Scorekeeper</a> is a basketball scorekeeper app built with the Shiny web framework for R. All of the code for the app is available on <a href="https://github.com/hinkelman/Shiny-Scorekeeper" target="_blank">GitHub.</a> The app is designed for use on a desktop computer while watching film of a game, i.e., not intended for mobile use at a live game. Below are brief instructions on how to use the key features of the app.

#### Create and Set Roster

Initially, the Teams table is empty. Add a row and double-click on cells to edit the placeholder values. Selecting a row in the Teams table brings up the Roster table. Add rows and double-click on cells to edit the placeholder values to fill out the roster. Players previously entered on rosters for other teams can be selected from a dropdown menu to add to a roster. Players are linked to multiple teams via a hidden PlayerID column. Updating a linked player's name on one team will update the player's name on all teams. The same player can have different numbers on different teams. The application detects unsaved changes to the tables. Save changes to enable setting a roster for use in scoring a game.

#### Score Game

When scoring a game, it is often useful to hide the sidebar (via hamburger button) to optimize the window size for collecting and viewing stats but do not forget to enter and save game info. The full roster is shown on the left side the scorekeeper buttons. Select the players on the court from the full roster. The players on the court will be shown on the right side of the scorekeeper buttons. Clicking on the scorekeeper buttons changes the stats of the player selected in the menu on the right. Toggling UNDO between off and on changes whether clicking on a scorekeeper button increments or decrements a statistic, respectively. Selecting the players that did not play allows for correct tallying of per game statistics. Saving game info and game stats makes the game stats immediately available through the Stats Viewer.

#### View Game Stats

View game stats by selecting one or more rows in the Teams and Games tables. Sort tables by clicking on column names. When no players are selected, team-level stats are displayed. 
