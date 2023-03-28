
Shiny Scorekeeper is a basketball scorekeeper app that I built with the Shiny web framework for R. This is a hobby project that I started in the fall of 2018. I was using a free iPhone app for scoring my son's basketball game films and then entering the tallies from the app into a Google spreadsheet. The app only allowed for recording stats for one player and I thought it would be interesting to compile stats on the whole team. I also wanted to avoid manual entry into a spreadsheet. There is no shortage of basketball scorekeeper apps in the world but, rather than search for the best app, I decided to embark on building my own app with R and Shiny. I've built numerous Shiny apps but never one quite like this. I've used the app to score 38 of my son's games and plan to continue to use it. It's quite empowering to build a tool tailored to your uses.

Shiny was designed as a web framework but, when used locally, it works well as a GUI toolkit. Shiny apps can even be packaged as [standalone desktop applications.](https://www.travishinkelman.com/post/dsm2-viz-tool/) One of the constraints I placed on myself in building Shiny Scorekeeper was to stick to R packages designed for use with Shiny (see list below) and avoid the use of custom JavaScript. I was hoping that this would keep me focused on the core functionality and not chasing my tail trying to beautify the app. Thus, I'm not thriled with the overall look of the app but also happy to get to a point where the app is functional (for my purposes) so that I can move on to other side projects. One of the lessons learned from this hobby project is that it can be difficult to sustain motivation on a silly side project with no deadlines, particularly as the app grew in complexity and adding new features required more careful thought.

Although the app contains no custom JavaScript, I couldn't help myself and used a little CSS and HTML to change styling of the buttons and the horizontal rule. The app likely could be simplified through the use of [Shiny modules](https://shiny.rstudio.com/articles/modules.html) but I haven't yet taken the plunge on learning how to incorporate modules into my Shiny apps. Because I was looking for a learning experience, I gave myself permission to fumble around and create my own solution rather than spending a lot of time trying to identify and follow examples of accepted best practices. For example, I created a [homemade database](https://www.travishinkelman.com/post/dt-datatable-crud) comprised of 8 CSV files rather than using an established database (e.g., [SQLite](https://www.sqlite.org/index.html)).

#### Missing Features
The following is a list of missing features that I decided not to pursue. Some items on this list are easy to implement but were a low priority. Others were abandoned after concluding that implementation would be tricky or even impossible without including custom JavaScript code.

* Adding mechanisms to import and export data. If Shiny Scorekeeper was packaged as a [standalone desktop application](https://www.travishinkelman.com/post/dsm2-viz-tool/), then the option to import and export data becomes more important than if using Shiny Scorekeeper through R directly because the data folder is easily accessible. Similarly, a standalone desktop app should also provide the option to edit game data from within the app.
* Tracking minutes played for calculating stats on a per minute basis.
* Presenting confirmation message when trying to close app with unsaved changes.
* Including win-loss record in team-level summary statistics.

#### Dependencies
* [shiny](http://shiny.rstudio.com)
* [shinydashboard](https://rstudio.github.io/shinydashboard/)
* [shinyWidgets](https://dreamrs.github.io/shinyWidgets/index.html)
* [shinyjs](https://deanattali.com/shinyjs/)
* [DT](https://rstudio.github.io/DT/)
* [dplyr](https://dplyr.tidyverse.org)
* [tidyr](https://tidyr.tidyverse.org)
