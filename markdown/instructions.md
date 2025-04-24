
<a href="https://www.travishinkelman.com/project/shiny-scorekeeper/" target="_blank">Shiny Scorekeeper</a> is a basketball scorekeeper app built with the Shiny web framework for R. All of the code for the app is available on <a href="https://github.com/hinkelman/Shiny-Scorekeeper" target="_blank">GitHub.</a> The app is designed for use on a desktop computer while watching film of a game, i.e., not intended for mobile use at a live game. Below are brief instructions on how to use the key features of the app.

#### Create and Set Roster

Initially, the `Teams` table is empty. Add a row and double-click on cells to edit. Selecting a row in the `Teams` table brings up the `Roster` table. Add rows and double-click on cells to fill out the roster. Players previously entered on rosters for other teams can be selected from a dropdown menu to add to a roster. Players are linked to multiple teams via a `PlayerID` column. Updating a linked player's name on one team will update the player's name on all teams. The same player can have different numbers on different teams. The application detects unsaved changes to the tables. Save changes to enable setting a roster for use in scoring a game. Shiny Scorekeeper is designed for tracking only one team in each game. However, if you include a player on each roster with the first name of `Opponent`, then you can you track team stats for the opponent with that "player" on the roster.  

#### Score Game

When scoring a game, it is often useful to hide the sidebar to optimize the window size for collecting and viewing stats but do not forget to enter and save game info. The full roster is shown on the left side the scorekeeper buttons. Clicking on the scorekeeper buttons changes the stats of the player selected in the roster menu. Toggling `UNDO` between off and on changes whether clicking on a scorekeeper button increments or decrements a statistic, respectively. Selecting the players that did not play allows for correct tallying of per game statistics. Game stats are immediately available through the Stats Viewer, but need to be saved to for viewing in a new session. 

#### View Statistics

Modify the summary stats table with the filter options in the sidebar and by choosing different combinations of `Team`, `Game`, and `Player` from the `Group by` dropdown menu. Click on column names to sort columns. 
