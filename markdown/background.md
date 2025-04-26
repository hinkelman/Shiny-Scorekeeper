
Shiny Scorekeeper is a basketball scorekeeper app that I built with the Shiny web framework for R. This is a hobby project that I started in the fall of 2018. I was using a free iPhone app for scoring my son's basketball game films and then entering the tallies from the app into a Google spreadsheet. The app only allowed for recording stats for one player and I thought it would be interesting to compile stats on the whole team. I also wanted to avoid manual entry into a spreadsheet. There is no shortage of basketball scorekeeper apps in the world but, rather than search for the best app, I decided to embark on building my own app with R and Shiny. I've built numerous Shiny apps but never one quite like this. I've used the app to score 38 of my son's games and plan to continue to use it. It's quite empowering to build a tool tailored to your uses.

Shiny was designed as a web framework but, when used locally, it works well as a GUI toolkit. In building Shiny Scorekeeper, I primarily stuck to R packages designed for use with Shiny (see list below). I was hoping that this would keep me focused on the core functionality and not chasing my tail trying to beautify the app. One of the lessons learned from this hobby project is that it can be difficult to sustain motivation on a silly side project with no deadlines, particularly as the app grew in complexity and adding new features required more careful thought.

Because I was looking for a learning experience, I gave myself permission to fumble around and create my own solution rather than spending a lot of time trying to identify and follow examples of accepted best practices. For example, I created a [homemade database](https://www.travishinkelman.com/post/dt-datatable-crud) comprised of five CSV files rather than using an established database (e.g., [SQLite](https://www.sqlite.org/index.html)).

#### Missing Features

The following is a list of features that I decided not to pursue.  

* Adding mechanisms to import and export data through the app. 
* Ability to edit or delete games, game stats, and game logs in the app.  
* Tracking minutes played for calculating stats on a per minute basis.
* Tracking stats by period (i.e., quarter or half).
* Final team scores are entered, not calculated from individual game stats.
* Including win-loss record in team-level summary statistics.
* Presenting confirmation message when trying to close app with unsaved changes.

#### Dependencies

* [shiny](https://shiny.posit.co)
* [bslib](https://rstudio.github.io/bslib/index.html)
* [shinyWidgets](https://dreamrs.github.io/shinyWidgets/index.html)
* [DT](https://rstudio.github.io/DT/)
* [reactable](https://glin.github.io/reactable/)
* [dplyr](https://dplyr.tidyverse.org)
* [tidyr](https://tidyr.tidyverse.org)
* [ids](https://reside-ic.github.io/ids/)
